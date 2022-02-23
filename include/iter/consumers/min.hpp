#ifndef INCLUDE_ITER_MIN_HPP
#define INCLUDE_ITER_MIN_HPP

#include "iter/core/core.hpp"

ITER_DECLARE(min)
ITER_DECLARE(min_by)

namespace iter::detail::minmax {
    template<class C, iter::iterable I, class F>
    requires iter::concepts::owned_item<iter::next_t<I>>
    constexpr auto apply(C&& comp, I&& iterable, F&& func) {
        decltype(auto) iter = iter::to_iter(FWD(iterable));
        auto next = iter::no_next<I>(), current = iter::no_next<I>();
        auto emplace_next = [&]() -> bool { return iter::detail::emplace_next(next, iter); };
        if (emplace_next()) {
            current = std::move(next);
            while (emplace_next())
                if (std::invoke(FWD(comp), std::invoke(FWD(func), iter::as_const(*current), iter::as_const(*next))))
                    *current = std::move(*next);
        }
        return current;
    }

    template<class C, iter::iterable I, class F>
    requires (!iter::concepts::owned_item<iter::next_t<I>>)
    constexpr auto apply(C&& comp, I&& iterable, F&& func) {
        decltype(auto) iter = iter::to_iter(FWD(iterable));
        auto val = iter::no_next<I>();
        auto emplace_next = [&]() -> bool { return val = impl::next(iter); };
        iter::item<iter::value_t<I>> result;
        if (emplace_next()) {
            result = *val;
            while (emplace_next())
                if (std::invoke(FWD(comp), std::invoke(FWD(func), iter::as_const(*result), iter::as_const(*val))))
                    *result = *val;
        }
        return result;
    }

    static constexpr auto min = [](auto&& l) { return l > 0; };
    static constexpr auto max = [](auto&& l) { return l < 0; };

    template<class C, iter::iterable I, class F>
    requires iter::concepts::owned_item<iter::next_t<I>>
    constexpr auto by(C&& comp, I&& iterable, F&& func) {
        decltype(auto) iter = iter::to_iter(FWD(iterable));
        auto next = iter::no_next<I>(), current = iter::no_next<I>();
        auto emplace_next = [&]() -> bool { return iter::detail::emplace_next(next, iter); };
        if (emplace_next()) {
            auto current_proj = std::invoke(FWD(func), iter::as_const(*next));
            current = std::move(next);
            while (emplace_next()) {
                auto next_proj = std::invoke(FWD(func), iter::as_const(*next));
                if (std::invoke(FWD(comp), iter::as_const(current_proj), iter::as_const(next_proj))) {
                    *current = std::move(*next);
                    current_proj = std::move(next_proj);
                }
            }
        }
        return current;
    }

    template<class C, iter::concepts::iterable I, class F>
    requires (!iter::concepts::owned_item<iter::next_t<I>>)
    constexpr auto by(C&& comp, I&& iterable, F&& func) {
        decltype(auto) iter = iter::to_iter(FWD(iterable));
        auto val = iter::no_next<I>();
        auto emplace_next = [&]() -> bool { return val = impl::next(iter); };
        item<iter::value_t<I>> result;
        if (emplace_next()) {
            auto current_proj = std::invoke(FWD(func), iter::as_const(*val));
            result = *val;
            while (emplace_next()) {
                auto next_proj = std::invoke(FWD(func), iter::as_const(*val));
                if (std::invoke(FWD(comp), iter::as_const(current_proj), iter::as_const(next_proj))) {
                    *result = *val;
                    current_proj = std::move(next_proj);
                }
            }
        }
        return result;
    }

    static constexpr auto min_by = [](auto&& next, auto&& current) { return next > current; };
    static constexpr auto max_by = [](auto&& next, auto&& current) { return next < current; };
}

template<iter::assert_iterable I, std::invocable<iter::cref_t<I>, iter::cref_t<I>> F = std::compare_three_way>
constexpr auto ITER_IMPL(min) (I&& iterable, F&& func = {}) {
    return iter::detail::minmax::apply(iter::detail::minmax::min, FWD(iterable), FWD(func));
}

template<iter::assert_iterable I, std::invocable<iter::cref_t<I>> F>
requires std::totally_ordered<std::invoke_result_t<F, iter::cref_t<I>>>
constexpr auto ITER_IMPL(min_by) (I&& iterable, F&& func) {
    return iter::detail::minmax::by(iter::detail::minmax::min_by, FWD(iterable), FWD(func));
}

#endif /* INCLUDE_ITER_MIN_HPP */

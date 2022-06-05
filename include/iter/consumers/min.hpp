#ifndef ITER_CONSUMERS_MIN_HPP
#define ITER_CONSUMERS_MIN_HPP

#include "iter/core/core.hpp"

ITER_DECLARE(min)
ITER_DECLARE(min_by)

namespace iter::detail::minmax {
    template<class C, iterable I, class F>
    requires concepts::owned_item<next_t<I>>
    constexpr auto apply(C&& comp, I&& iterable, F&& func) {
        decltype(auto) iter = to_iter(FWD(iterable));
        auto next = no_next<I>(), current = no_next<I>();
        auto emplace_next = [&]() -> bool { return detail::emplace_next(next, iter); };
        if (emplace_next()) {
            current = std::move(next);
            while (emplace_next())
                if (std::invoke(FWD(comp), std::invoke(FWD(func), as_const(*current), as_const(*next))))
                    *current = std::move(*next);
        }
        return current;
    }

    template<class C, iterable I, class F>
    requires concepts::stable_item<iter::next_t<I>>
        && (!concepts::owned_item<iter::next_t<I>>)
    constexpr auto apply(C&& comp, I&& iterable, F&& func) {
        decltype(auto) iter = to_iter(FWD(iterable));
        auto next = no_next<I>(), result = no_next<I>();
        auto emplace_next = [&]() -> bool { return next = impl::next(iter); };
        if (emplace_next()) {
            result = next;
            while (emplace_next())
                if (std::invoke(FWD(comp), std::invoke(FWD(func), as_const(*result), as_const(*next))))
                    result = next;
        }
        return result;
    }

    inline constexpr auto min = [](auto&& l) { return l > 0; };
    inline constexpr auto max = [](auto&& l) { return l < 0; };

    template<class C, iterable I, class F>
    requires concepts::owned_item<next_t<I>>
    constexpr auto by(C&& comp, I&& iterable, F&& func) {
        decltype(auto) iter = to_iter(FWD(iterable));
        auto next = no_next<I>(), current = no_next<I>();
        auto emplace_next = [&]() -> bool { return detail::emplace_next(next, iter); };
        if (emplace_next()) {
            auto current_proj = std::invoke(FWD(func), as_const(*next));
            current = std::move(next);
            while (emplace_next()) {
                auto next_proj = std::invoke(FWD(func), as_const(*next));
                if (std::invoke(FWD(comp), as_const(current_proj), as_const(next_proj))) {
                    *current = std::move(*next);
                    current_proj = std::move(next_proj);
                }
            }
        }
        return current;
    }

    template<class C, iterable I, class F>
    requires concepts::stable_item<next_t<I>>
        && (!concepts::owned_item<next_t<I>>)
    constexpr auto by(C&& comp, I&& iterable, F&& func) {
        decltype(auto) iter = to_iter(FWD(iterable));
        auto val = no_next<I>(), result = no_next<I>();
        auto emplace_next = [&]() -> bool { return val = impl::next(iter); };
        if (emplace_next()) {
            auto current_proj = std::invoke(FWD(func), as_const(*val));
            result = val;
            while (emplace_next()) {
                auto next_proj = std::invoke(FWD(func), as_const(*val));
                if (std::invoke(FWD(comp), as_const(current_proj), as_const(next_proj))) {
                    result = val;
                    current_proj = std::move(next_proj);
                }
            }
        }
        return result;
    }

    inline constexpr auto min_by = [](auto&& next, auto&& current) { return next > current; };
    inline constexpr auto max_by = [](auto&& next, auto&& current) { return next < current; };
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

#endif /* ITER_CONSUMERS_MIN_HPP */

#ifndef INCLUDE_ITER_MAX_HPP
#define INCLUDE_ITER_MAX_HPP

#include "iter/core.hpp"

ITER_DECLARE(max)
ITER_DECLARE(max_by)

template<iter::concepts::optional_iterable I, std::invocable<iter::cref_t<I>, iter::cref_t<I>> F>
constexpr auto ITER_IMPL(max) (I&& iterable, F&& func) {
    decltype(auto) iter = iter::to_iter((I&&) iterable);
    iter::next_t<I> results[2] = {iter::no_next<I>(), iter::no_next<I>()};
    char i = 0;
    auto current = [&]() -> auto& { ITER_ASSUME(i == 0 || i == 1); return results[i ^ 1]; };
    auto next = [&]() -> auto& { ITER_ASSUME(i == 0 || i == 1); return results[i]; };
    while (iter::detail::emplace_next(next(), iter)) {
        if (!current() || std::invoke((F&&) func, iter::as_const(*current()), iter::as_const(*next())) < 0)
            i ^= 1;
    }
    return std::move(current());
}

template<iter::concepts::pointer_iterable I, std::invocable<iter::cref_t<I>, iter::cref_t<I>> F>
constexpr auto ITER_IMPL(max) (I&& iterable, F&& func) {
    decltype(auto) iter = iter::to_iter((I&&) iterable);
    std::optional<iter::value_t<I>> result;
    ITER_FOR (val, iter) {
        if (!result || std::invoke((F&&) func, iter::as_const(*result), iter::as_const(*val)) < 0) [[unlikely]] {
            result = *val;
        }
    }
    return result;
}

template<iter::iterable I>
requires std::three_way_comparable<iter::value_t<I>>
constexpr auto ITER_IMPL(max) (I&& iterable) {
    return iter::max((I&&) iterable, [](auto& max, auto& i) {
        return max <=> i;
    });
}

template<iter::concepts::optional_iterable I, std::invocable<iter::cref_t<I>> F>
requires std::three_way_comparable<std::invoke_result_t<F, iter::cref_t<I>>>
constexpr auto ITER_IMPL(max_by) (I&& iterable, F&& func) {
    decltype(auto) iter = iter::to_iter((I&&) iterable);
    iter::next_t<I> results[2] = {iter::no_next<I>(), iter::no_next<I>()};
    using projection_t = std::invoke_result_t<F, iter::cref_t<I>>;
    std::optional<projection_t> projections[2] = {std::nullopt, std::nullopt};
    char i = 0;
    auto current_result = [&]() -> auto& { ITER_ASSUME(i == 0 || i == 1); return results[i ^ 1]; };
    auto current_proj = [&]() -> auto& { ITER_ASSUME(i == 0 || i == 1); return projections[i ^ 1]; };
    auto next_result = [&]() -> auto& { ITER_ASSUME(i == 0 || i == 1); return results[i]; };
    auto next_proj = [&]() -> auto& { ITER_ASSUME(i == 0 || i == 1); return projections[i]; };
    while (iter::detail::emplace_next(next_result(), iter)) {
        EMPLACE_NEW(next_proj(), MAKE_OPTIONAL(std::invoke((F&&) func, iter::as_const(*next_result()))));
        if (current_proj() < next_proj())
            i ^= 1;
    }
    return std::move(current_result());
}

template<iter::concepts::pointer_iterable I, std::invocable<iter::cref_t<I>> F>
requires std::three_way_comparable<std::invoke_result_t<F, iter::cref_t<I>>>
constexpr auto ITER_IMPL(max_by) (I&& iterable, F&& func) {
    decltype(auto) iter = iter::to_iter((I&&) iterable);
    std::optional<iter::value_t<I>> result;
    using projection_t = std::invoke_result_t<F, iter::cref_t<I>>;
    std::optional<projection_t> projections[2] = {std::nullopt, std::nullopt};
    char i = 0;
    auto current_proj = [&]() -> auto& { ITER_ASSUME(i == 0 || i == 1); return projections[i ^ 1]; };
    auto next_proj = [&]() -> auto& { ITER_ASSUME(i == 0 || i == 1); return projections[i]; };
    ITER_FOR (val, iter) {
        EMPLACE_NEW(next_proj(), MAKE_OPTIONAL(std::invoke((F&&) func, iter::as_const(*val))));
        if (current_proj() < next_proj()) {
            result = *val;
            i ^= 1;
        }
    }
    return result;
}

#endif /* INCLUDE_ITER_MAX_HPP */

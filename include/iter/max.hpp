#ifndef INCLUDE_ITER_MAX_HPP
#define INCLUDE_ITER_MAX_HPP

#include "iter/min.hpp"

ITER_DECLARE(max)
ITER_DECLARE(max_by)

template<iter::concepts::iterable I, std::invocable<iter::cref_t<I>, iter::cref_t<I>> F = std::compare_three_way>
constexpr auto ITER_IMPL(max) (I&& iterable, F&& func = {}) {
    return iter::detail::minmax::apply(iter::detail::minmax::max, FWD(iterable), FWD(func));
}

template<iter::concepts::iterable I, std::invocable<iter::cref_t<I>> F>
requires std::totally_ordered<std::invoke_result_t<F, iter::cref_t<I>>>
constexpr auto ITER_IMPL(max_by) (I&& iterable, F&& func) {
    return iter::detail::minmax::by(iter::detail::minmax::max_by, FWD(iterable), FWD(func));
}

#endif /* INCLUDE_ITER_MAX_HPP */

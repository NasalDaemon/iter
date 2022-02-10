#ifndef INCLUDE_ITER_ENABLE_RANGES_HPP
#define INCLUDE_ITER_ENABLE_RANGES_HPP

#include "iter/core.hpp"

#include <ranges>

template<iter::iter I>
constexpr bool std::ranges::enable_view<I> = true;

#endif /* INCLUDE_ITER_ENABLE_RANGES_HPP */

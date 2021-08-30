#ifndef INCLUDE_ITER_FIND_MAP_HPP
#define INCLUDE_ITER_FIND_MAP_HPP

#include "iter/filter_map.hpp"

ITER_DECLARE(find_map)

template<iter::iterable I, std::invocable<iter::consume_t<I>> F>
constexpr auto ITER_IMPL(find_map) (I&& iterable, F&& func) {
    auto fm = iter::filter_map(FWD(iterable), FWD(func));
    if constexpr (iter::concepts::optional_iterable<decltype(fm)>)
        return iter::next(fm);
    else {
        auto val = iter::next(fm);
        return val ? std::make_optional(*val) : std::nullopt;
    }
}

#endif /* INCLUDE_ITER_FIND_MAP_HPP */

#ifndef INCLUDE_ITER_FIND_MAP_HPP
#define INCLUDE_ITER_FIND_MAP_HPP

#include "iter/filter_map.hpp"

ITER_DECLARE(find_map)

template<iter::assert_iterable I, std::invocable<iter::consume_t<I>> F>
constexpr auto ITER_IMPL(find_map) (I&& iterable, F&& func) {
    auto fm = iter::filter_map(FWD(iterable), FWD(func));
    if constexpr (iter::concepts::owned_item<iter::next_t<decltype(fm)>>)
        return iter::detail::impl::next(fm);
    else {
        auto val = iter::detail::impl::next(fm);
        return val ? iter::item{val.consume()} : iter::noitem;
    }
}

#endif /* INCLUDE_ITER_FIND_MAP_HPP */

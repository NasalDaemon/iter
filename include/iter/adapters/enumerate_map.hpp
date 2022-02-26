#ifndef INCLUDE_ITER_ENUMERATE_MAP_HPP
#define INCLUDE_ITER_ENUMERATE_MAP_HPP

#include "iter/adapters/zip_map.hpp"
#include "iter/iters/range.hpp"

XTD_INVOKER(iter_enumerate_map)

namespace iter {
    namespace detail::tag {
        template<class T = std::size_t>
        struct enumerate_map_ : xtd::tagged_bindable<enumerate_map_<T>, xtd::invokers::iter_enumerate_map> {};
    }

    template<class T = std::size_t>
    static constexpr detail::tag::enumerate_map_<T> enumerate_map_;
}

ITER_ALIAS(enumerate_map, enumerate_map_<>)

template<class T, iter::assert_iterable I, class F>
constexpr decltype(auto) XTD_IMPL_TAG_(iter_enumerate_map, iter::detail::tag::enumerate_map_<T>) (I&& iterable, F&& func) {
    return iter::zip_map(FWD(iterable), iter::indices_<T>, FWD(func));
}

#endif /* INCLUDE_ITER_ENUMERATE_MAP_HPP */

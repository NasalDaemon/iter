#ifndef INCLUDE_ITER_ENUMERATE_WITH_HPP
#define INCLUDE_ITER_ENUMERATE_WITH_HPP

#include "iter/zip_with.hpp"
#include "iter/range.hpp"

XTD_INVOKER(iter_enumerate_with)

namespace iter {
    namespace tag {
        template<class T = std::size_t>
        struct enumerate_with_ : xtd::tagged_bindable<enumerate_with_<T>, xtd::invokers::iter_enumerate_with> {};
    }

    template<class T = std::size_t>
    static constexpr tag::enumerate_with_<T> enumerate_with_;
}

ITER_ALIAS(enumerate_with, enumerate_with_<>)

template<class T, iter::assert_iterable I, class F>
constexpr decltype(auto) XTD_IMPL_TAG_(iter_enumerate_with, iter::tag::enumerate_with_<T>) (I&& iterable, F&& func) {
    return iter::zip_with(FWD(iterable), iter::indices_<T>, FWD(func));
}

#endif /* INCLUDE_ITER_ENUMERATE_WITH_HPP */

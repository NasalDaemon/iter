#ifndef INCLUDE_ITER_ENUMERATE_HPP
#define INCLUDE_ITER_ENUMERATE_HPP

#include "iter/zip.hpp"
#include "iter/range.hpp"

XTD_INVOKER(iter_enumerate)

namespace iter {
    namespace tag {
        template<class T = std::size_t>
        struct enumerate_ : xtd::tagged_bindable<enumerate_<T>, xtd::invokers::iter_enumerate> {};
    }

    template<class T = std::size_t>
    static constexpr tag::enumerate_<T> enumerate_;
}

ITER_ALIAS(enumerate, enumerate_<>)

template<class T, iter::assert_iterable I>
constexpr decltype(auto) XTD_IMPL_TAG_(iter_enumerate, iter::tag::enumerate_<T>) (I&& iterable) {
    return iter::zip(FWD(iterable), iter::indices_<T>);
}

#endif /* INCLUDE_ITER_ENUMERATE_HPP */

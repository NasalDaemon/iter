#ifndef ITER_COLLECTORS_SORTED_HPP
#define ITER_COLLECTORS_SORTED_HPP

#include "iter/collectors/collect.hpp"

XTD_INVOKER(iter_sorted)

namespace iter {
    namespace detail::tag {
        template<template<class...> class C = std::vector, template<class> class A = std::allocator>
        struct sorted_ : xtd::tagged_bindable<sorted_<C, A>, xtd::invokers::iter_sorted> {};
    }

    template<template<class...> class C = std::vector, template<class> class A = std::allocator>
    static constexpr detail::tag::sorted_<C, A> sorted_;
}

ITER_ALIAS(sorted, sorted_<>)

template<template<class...> class CT, template<class> class AT,
         iter::assert_iter I, std::invocable<iter::ref_t<I>, iter::ref_t<I>> P>
constexpr auto XTD_IMPL_TAG_(iter_sorted, iter::detail::tag::sorted_<CT, AT>)(I&& iter, P&& predicate) {
    auto container = iter::collect<CT, AT>(FWD(iter));
    std::sort(std::begin(container), std::end(container), FWD(predicate));
    return container;
}

template<template<class...> class CT, template<class> class AT, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_sorted, iter::detail::tag::sorted_<CT, AT>)(I&& iter) {
    auto container = iter::collect<CT, AT>(FWD(iter));
    std::sort(std::begin(container), std::end(container));
    return container;
}

#endif /* ITER_COLLECTORS_SORTED_HPP */

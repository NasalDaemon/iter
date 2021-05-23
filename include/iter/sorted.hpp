#ifndef INCLUDE_ITER_SORTED_HPP
#define INCLUDE_ITER_SORTED_HPP

#include "iter/collect.hpp"

XTD_INVOKER(iter_sorted)

namespace iter {
    namespace tag {
        template<template<class...> class C = std::vector, template<class> class A = std::allocator>
        struct sorted : xtd::tagged_bindable<sorted<C, A>, xtd::invokers::iter_sorted> {};
    }

    template<template<class...> class C = std::vector, template<class> class A = std::allocator>
    static constexpr tag::sorted<C, A> sorted_;
}

ITER_ALIAS(sorted, sorted_<>);

template<template<class...> class CT, template<class> class AT,
         iter::iter I, std::invocable<iter::ref_t<I>, iter::ref_t<I>> P>
constexpr auto XTD_IMPL_TAG_(iter_sorted, iter::tag::sorted<CT, AT>)(I&& iter, P&& predicate) {
    auto container = iter::collect<CT, AT>((I&&) iter);
    std::sort(std::begin(container), std::end(container), (P&&) predicate);
    return container;
}

template<template<class...> class CT, template<class> class AT, iter::iter I>
constexpr auto XTD_IMPL_TAG_(iter_sorted, iter::tag::sorted<CT, AT>)(I&& iter) {
    auto container = iter::collect<CT, AT>((I&&) iter);
    std::sort(std::begin(container), std::end(container));
    return container;
}

#endif /* INCLUDE_ITER_SORTED_HPP */

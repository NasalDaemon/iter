#ifndef ITER_COLLECTORS_COLLECT_HPP
#define ITER_COLLECTORS_COLLECT_HPP

#include "iter/core/core.hpp"
#include "iter/core/std_fwd.hpp"

XTD_INVOKER(iter_collect)

namespace iter {
    namespace detail::tag {
        template<template<class...> class C = std::vector, template<class> class A = std::allocator, template<class> class... Traits>
        struct collect : xtd::tagged_bindable<collect<C, A, Traits...>, xtd::invokers::iter_collect> {};
    }

    template<template<class...> class C = std::vector, template<class> class A = std::allocator, template<class> class... Traits>
    inline constexpr detail::tag::collect<C, A, Traits...> collect;
}

ITER_ALIAS(to_vector, collect<std::vector>)
ITER_ALIAS(to_map, collect<std::map>)
ITER_ALIAS(to_string, collect<std::basic_string, std::allocator, std::char_traits>)

template<template<class...> class CT, template<class> class AT, template<class> class... Traits, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_collect, iter::detail::tag::collect<CT, AT, Traits...>)(I&& iter) {
    using T = iter::value_t<I>;
    CT<T, Traits<T>..., AT<T>> container;
    if constexpr (iter::concepts::random_access_iter<I>) {
        container.reserve(iter::traits::random_access::size(iter));
    }
    while (auto val = iter::traits::next(iter)) {
        container.push_back(iter::detail::consume(val));
    }
    return container;
}

template<template<class...> class CT, template<class> class AT, template<class> class... Traits, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_collect, iter::detail::tag::collect<CT, AT, Traits...>)(I&& iter, std::size_t reserve) {
    using T = iter::value_t<I>;
    CT<T, Traits<T>..., AT<T>> container;
    if constexpr (iter::concepts::random_access_iter<I>) {
        reserve = std::max(reserve, iter::traits::random_access::size(iter));
    }
    container.reserve(reserve);
    while (auto val = iter::traits::next(iter)) {
        container.push_back(iter::detail::consume(val));
    }
    return container;
}

template<template<class> class AT, iter::assert_iter I, class Comp>
constexpr auto XTD_IMPL_TAG_(iter_collect, iter::detail::tag::collect<std::map, AT>)(I&& iter, Comp&& compare) {
    using KV = iter::value_t<I>;
    using K = std::tuple_element_t<0, KV>;
    using V = std::tuple_element_t<1, KV>;
    using A = AT<std::pair<K const, V>>;
    std::map<K, V, std::remove_cvref_t<Comp>, A> container(FWD(compare));
    while (auto val = iter::traits::next(iter)) {
        container.emplace(iter::detail::consume(val));
    }
    return container;
}

template<template<class> class AT, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_collect, iter::detail::tag::collect<std::map, AT>)(I&& iter) {
    using KV = iter::value_t<I>;
    using K = std::tuple_element_t<0, KV>;
    return iter::collect<std::map, AT>(FWD(iter), std::less<K>{});
}

#endif /* ITER_COLLECTORS_COLLECT_HPP */

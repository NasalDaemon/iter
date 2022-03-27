#ifndef INCLUDE_ITER_UNZIP_HPP
#define INCLUDE_ITER_UNZIP_HPP

#include "iter/core/core.hpp"
#include "iter/core/std_fwd.hpp"

XTD_INVOKER(iter_unzip)

namespace iter {
    namespace detail::tag {
        template<template<class...> class C = std::vector, template<class> class A = std::allocator>
        struct unzip_ : xtd::tagged_bindable<unzip_<C, A>, xtd::invokers::iter_unzip> {};
    }

    template<template<class...> class C = std::vector, template<class> class A = std::allocator>
    inline constexpr detail::tag::unzip_<C, A> unzip_;

    namespace detail {
        template<template<class...> class CT, template<class> class AT, class>
        struct unzipped;

        template<template<class...> class CT, template<class> class AT, class... Ts>
        struct unzipped<CT, AT, tuple<Ts...>> {
            using type = tuple<CT<Ts, AT<Ts>>...>;
            static constexpr std::size_t size = sizeof...(Ts);
        };
        template<template<class...> class CT, template<class> class AT, class... Ts>
        struct unzipped<CT, AT, std::tuple<Ts...>> {
            using type = std::tuple<CT<Ts, AT<Ts>>...>;
            static constexpr std::size_t size = sizeof...(Ts);
        };
        template<template<class...> class CT, template<class> class AT, class T, std::size_t N>
        struct unzipped<CT, AT, std::array<T, N>> {
            using type = std::array<CT<T, AT<T>>, N>;
            static constexpr std::size_t size = N;
        };
    }
}

ITER_ALIAS(unzip, unzip_<>)

template<template<class...> class CT, template<class> class AT, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_unzip, iter::detail::tag::unzip_<CT, AT>)(I&& iter) {
    using traits = iter::detail::unzipped<CT, AT, iter::value_t<I>>;
    typename traits::type containers{};

    if constexpr (iter::concepts::random_access_iter<I>) {
        apply([size = iter::traits::random_access::size(iter)](auto&... c) {
            (c.reserve(size), ...);
        }, containers);
    }
    while (auto val = iter::traits::next(iter)) {
        [&]<std::size_t... Is>(std::index_sequence<Is...>) {
            (get<Is>(containers).push_back(get<Is>(iter::detail::consume(val))), ...);
        }(std::make_index_sequence<traits::size>{});
    }
    return containers;
}

template<template<class...> class CT, template<class> class AT, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_unzip, iter::detail::tag::unzip_<CT, AT>)(I&& iter, std::size_t reserve) {
    using traits = iter::detail::unzipped<CT, AT, iter::value_t<I>>;
    typename traits::type containers{};

    if constexpr (iter::concepts::random_access_iter<I>)
        reserve = std::max(reserve, iter::traits::random_access::size(iter));

    apply([=](auto&... c) {
        (c.reserve(reserve), ...);
    }, containers);

    while (auto val = iter::traits::next(iter)) {
        [&]<std::size_t... Is>(std::index_sequence<Is...>) {
            (get<Is>(containers).push_back(get<Is>(iter::detail::consume(val))), ...);
        }(std::make_index_sequence<traits::size>{});
    }

    return containers;
}

#endif /* INCLUDE_ITER_UNZIP_HPP */

#ifndef INCLUDE_ITER_NTH_HPP
#define INCLUDE_ITER_NTH_HPP

#include "iter/core/core.hpp"

ITER_DECLARE(nth)

template<iter::concepts::random_access_iterable I>
constexpr auto ITER_IMPL(nth) (I&& iterable, std::size_t n) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    std::size_t size = iter::traits::random_access::size(iter);
    using get_t = decltype(iter::traits::random_access::get(iter, n));
    if constexpr (std::is_lvalue_reference_v<decltype(iter)> && std::is_reference_v<get_t>)
        return size > n ? MAKE_ITEM_AUTO(iter::traits::random_access::get(iter, n)) : iter::noitem;
    else
        return size > n ? MAKE_ITEM(iter::traits::random_access::get(iter, n)) : iter::noitem;
}

template<iter::concepts::random_access_iterable I, class T>
constexpr auto ITER_IMPL(nth) (I&& iterable, std::size_t n, T&& fallback) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    std::size_t size = iter::traits::random_access::size(iter);
    return size > n ? iter::traits::random_access::get(iter, n) : FWD(fallback);
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(nth) (I&& iterable, std::size_t n) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    auto result = iter::no_next<decltype(iter)>();
    while (iter::detail::emplace_next(result, iter) && n-- > 0);
    if constexpr (iter::concepts::owned_item<decltype(result)> || std::is_lvalue_reference_v<decltype(iter)>)
        return result;
    else
        return result ? iter::item(*result) : iter::noitem;
}

template<iter::assert_iterable I, class T>
constexpr auto ITER_IMPL(nth) (I&& iterable, std::size_t n, T&& fallback) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    auto result = iter::no_next<decltype(iter)>();
    while (iter::detail::emplace_next(result, iter) && n-- > 0);
    return result ? result.consume() : FWD(fallback);
}

#endif /* INCLUDE_ITER_NTH_HPP */

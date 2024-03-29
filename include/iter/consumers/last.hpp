#ifndef ITER_CONSUMERS_LAST_HPP
#define ITER_CONSUMERS_LAST_HPP

#include "iter/core/core.hpp"

ITER_DECLARE(last)

template<iter::concepts::random_access_iterable I>
constexpr auto ITER_IMPL(last) (I&& iterable) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    std::size_t size = iter::traits::random_access::size(iter);
    using get_t = iter::detail::stability_unwrap<decltype(iter::traits::random_access::get(iter, size - 1))>;
    if constexpr (std::is_lvalue_reference_v<decltype(iter)> && std::is_reference_v<get_t>)
        return size > 0 ? MAKE_ITEM_AUTO(iter::traits::random_access::get(iter, size - 1)) : iter::noitem;
    else
        return size > 0 ? iter::item([&] {
            if constexpr (std::is_reference_v<get_t>)
                return get(iter::traits::random_access::get(iter, size - 1));
            else
                return iter::traits::random_access::get(iter, size - 1);
        }) : iter::noitem;
}

template<iter::concepts::random_access_iterable I, class T>
constexpr auto ITER_IMPL(last) (I&& iterable, T&& fallback) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    std::size_t size = iter::traits::random_access::size(iter);
    return size > 0 ? get(iter::traits::random_access::get(iter, size - 1)) : FWD(fallback);
}

template<iter::iterable I>
requires (iter::concepts::owned_item<iter::next_t<I>>)
      && (!iter::concepts::random_access_iterable<I>)
constexpr auto ITER_IMPL(last) (I&& iterable) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    iter::next_t<decltype(iter)> results[2] = {iter::noitem, iter::noitem};
    char i = 0;
    while (true) {
        bool empty = !iter::detail::emplace_next(results[i], iter);
        i ^= 1;
        if (empty) [[unlikely]] return std::move(results[i]);
    }
}

template<iter::iterable I>
requires (!iter::concepts::owned_item<iter::next_t<I>>)
      && (!iter::concepts::random_access_iterable<I>)
constexpr auto ITER_IMPL(last) (I&& iterable) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    iter::item<iter::value_t<I>> result;
    while (auto val = iter::traits::next(iter)) {
        result.emplace(val.consume());
    }
    return result;
}

template<iter::iterable I, class T>
requires (iter::concepts::owned_item<iter::next_t<I>>)
      && (!iter::concepts::random_access_iterable<I>)
constexpr auto ITER_IMPL(last) (I&& iterable, T&& fallback) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    iter::next_t<decltype(iter)> results[2] = {iter::noitem, iter::noitem};
    char i = 0;
    while (true) {
        bool empty = !iter::detail::emplace_next(results[i], iter);
        i ^= 1;
        if (empty) [[unlikely]] {
            return results[i] ? std::move(*results[i]) : FWD(fallback);
        }
    }
}

template<iter::iterable I, class T>
requires (!iter::concepts::owned_item<iter::next_t<I>>)
      && (!iter::concepts::random_access_iterable<I>)
constexpr auto ITER_IMPL(last) (I&& iterable, T&& fallback) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    iter::value_t<I> result = FWD(fallback);
    while (auto val = iter::traits::next(iter)) {
        result = iter::detail::consume(val);
    }
    return result;
}

#endif /* ITER_CONSUMERS_LAST_HPP */

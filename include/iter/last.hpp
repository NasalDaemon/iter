#ifndef INCLUDE_ITER_LAST_HPP
#define INCLUDE_ITER_LAST_HPP

#include "iter/core.hpp"

ITER_DECLARE(last)

template<iter::concepts::random_access_iterable I>
constexpr auto ITER_IMPL(last) (I&& iterable) {
    decltype(auto) iter = iter::to_iter((I&&) iterable);
    std::size_t size = iter::unsafe::size(iter);
    using get_t = decltype(iter::unsafe::get(iter, size - 1));
    if constexpr (std::is_lvalue_reference_v<decltype(iter)> && std::is_reference_v<get_t>)
        return size > 0 ? std::addressof(iter::unsafe::get(iter, size - 1)) : nullptr;
    else
        return size > 0 ? MAKE_OPTIONAL(iter::unsafe::get(iter, size - 1)) : std::nullopt;
}

template<iter::concepts::random_access_iterable I, class T>
constexpr auto ITER_IMPL(last) (I&& iterable, T&& fallback) {
    decltype(auto) iter = iter::to_iter((I&&) iterable);
    std::size_t size = iter::unsafe::size(iter);
    return size > 0 ? iter::unsafe::get(iter, size - 1) : (T&&)fallback;
}

template<iter::concepts::optional_iterable I>
requires (!iter::concepts::random_access_iterable<I>)
constexpr auto ITER_IMPL(last) (I&& iterable) {
    decltype(auto) iter = iter::to_iter((I&&) iterable);
    iter::next_t<decltype(iter)> results[2] = {std::nullopt, std::nullopt};
    char i = 0;
    while (true) {
        bool empty = !iter::detail::emplace_next(results[i], iter);
        i ^= 1;
        if (empty) [[unlikely]] return std::move(results[i]);
    }
}

template<iter::concepts::pointer_iterable I>
requires (!iter::concepts::random_access_iterable<I>)
constexpr auto ITER_IMPL(last) (I&& iterable) {
    decltype(auto) iter = iter::to_iter((I&&) iterable);
    std::optional<iter::value_t<I>> result = std::nullopt;
    while (auto val = iter::next(iter)) {
        result = iter::detail::consume(val);
    }
    return result;
}

template<iter::concepts::optional_iterable I, class T>
requires (!iter::concepts::random_access_iterable<I>)
constexpr auto ITER_IMPL(last) (I&& iterable, T&& fallback) {
    decltype(auto) iter = iter::to_iter((I&&) iterable);
    iter::next_t<decltype(iter)> results[2] = {std::nullopt, std::nullopt};
    char i = 0;
    while (true) {
        bool empty = !iter::detail::emplace_next(results[i], iter);
        i ^= 1;
        if (empty) [[unlikely]] {
            return results[i] ? std::move(*results[i]) : (T&&) fallback;
        }
    }
}

template<iter::concepts::pointer_iterable I, class T>
requires (!iter::concepts::random_access_iterable<I>)
constexpr auto ITER_IMPL(last) (I&& iterable, T&& fallback) {
    decltype(auto) iter = iter::to_iter((I&&) iterable);
    iter::value_t<I> result = (T&&) fallback;
    while (auto val = iter::next(iter)) {
        result = iter::detail::consume(val);
    }
    return result;
}

#endif /* INCLUDE_ITER_LAST_HPP */

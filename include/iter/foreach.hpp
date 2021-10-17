#ifndef INCLUDE_ITER_FOREACH_HPP
#define INCLUDE_ITER_FOREACH_HPP

#include "iter/core.hpp"

ITER_DECLARE(foreach)
ITER_ALIAS(for_each, foreach)

template<iter::assert_iterable I, iter::concepts::inspector<iter::consume_t<I>> F>
constexpr void ITER_IMPL(foreach) (I&& iterable, F func) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (auto val = iter::detail::impl::next(iter)) {
        func(iter::detail::consume(val));
    }
}

template<iter::assert_iterable I>
constexpr void ITER_IMPL(foreach) (I&& iterable) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (iter::detail::impl::next(iter)) {}
}

#endif /* INCLUDE_ITER_FOREACH_HPP */

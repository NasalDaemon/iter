#ifndef INCLUDE_ITER_FOREACH_HPP
#define INCLUDE_ITER_FOREACH_HPP

#include "iter/core.hpp"

ITER_DECLARE(foreach)
ITER_ALIAS(foreach, for_each)

template<iter::iterable I, iter::concepts::inspector<iter::consume_t<I>> F>
constexpr void ITER_IMPL(foreach) (I&& iterable, F func) {
    decltype(auto) iter = iter::to_iter((I&&) iterable);
    ITER_FOR (val, iter) {
        func(iter::detail::consume(val));
    }
}

template<iter::iterable I>
constexpr void ITER_IMPL(foreach) (I&& iterable) {
    decltype(auto) iter = iter::to_iter((I&&) iterable);
    ITER_FOR (val, iter) {}
}

#endif /* INCLUDE_ITER_FOREACH_HPP */

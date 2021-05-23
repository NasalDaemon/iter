#ifndef INCLUDE_ITER_FOLD_HPP
#define INCLUDE_ITER_FOLD_HPP

#include "iter/core.hpp"

ITER_DECLARE(fold)
ITER_ALIAS(fold_left, fold)

template<iter::iterable I, class T, std::invocable<const T&, iter::consume_t<I>> F>
constexpr auto ITER_IMPL(fold) (I&& iterable, T&& init, F func) {
    decltype(auto) iter = iter::to_iter((I&&) iterable);
    auto acc = (T&&)init;
    while (auto val = iter::next(iter)) {
        acc = func(iter::as_const(acc), iter::detail::consume(val));
    }
    return acc;
}

#endif /* INCLUDE_ITER_FOLD_HPP */

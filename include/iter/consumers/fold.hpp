#ifndef ITER_COLLECTORS_FOLD_HPP
#define ITER_COLLECTORS_FOLD_HPP

#include "iter/core/core.hpp"

ITER_DECLARE(fold)
ITER_ALIAS(fold_left, fold)

template<iter::assert_iterable I, class T, std::invocable<const T&, iter::consume_t<I>> F>
constexpr auto ITER_IMPL(fold) (I&& iterable, T&& init, F func) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    auto acc = FWD(init);
    while (auto val = iter::detail::impl::next(iter)) {
        acc = func(iter::as_const(acc), iter::detail::consume(val));
    }
    return acc;
}

#endif /* ITER_COLLECTORS_FOLD_HPP */

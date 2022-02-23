#ifndef ITER_CONSUMERS_REDUCE_HPP
#define ITER_CONSUMERS_REDUCE_HPP

#include "iter/consumers/fold.hpp"

ITER_DECLARE(reduce)

template<iter::assert_iterable I, std::invocable<iter::ref_t<I>, iter::consume_t<I>> F>
constexpr iter::item<iter::value_t<I>> ITER_IMPL(reduce) (I&& iterable, F&& func) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    auto acc = iter::traits::next(iter);
    return acc
        ? MAKE_ITEM(iter::fold(iter, iter::detail::consume(acc), FWD(func)))
        : iter::noitem;
}

#endif /* ITER_CONSUMERS_REDUCE_HPP */

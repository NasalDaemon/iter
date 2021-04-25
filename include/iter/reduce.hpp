#ifndef INCLUDE_ITER_REDUCE_HPP
#define INCLUDE_ITER_REDUCE_HPP

#include "iter/fold.hpp"

ITER_DECLARE(reduce)

template<iter::iterable I, std::invocable<iter::ref_t<I>, iter::consume_t<I>> F>
constexpr std::optional<iter::value_t<I>> ITER_IMPL(reduce) (I&& iterable, F&& func) {
    decltype(auto) iter = iter::to_iter((I&&) iterable);
    auto acc = iter::next(iter);
    return acc
        ? MAKE_OPTIONAL(iter::fold(iter, iter::detail::consume(acc), (F&&)func))
        : std::nullopt;
}

#endif /* INCLUDE_ITER_REDUCE_HPP */

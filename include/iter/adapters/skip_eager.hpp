#ifndef ITER_ADAPTERS_SKIP_EAGER_HPP
#define ITER_ADAPTERS_SKIP_EAGER_HPP

#include "iter/core/core.hpp"

ITER_DECLARE(skip_eager)

template<iter::assert_iterable I>
constexpr iter::iter_t<I> ITER_IMPL(skip_eager) (I&& iterable, std::size_t n) {
    static_assert(!iter::concepts::random_access_iterable<I>,
        "iter::iter_eager is only for non-random-access iterables, use iter::skip.");
    auto iter = iter::to_iter(FWD(iterable));
    for(; n > 0 && iter::traits::next(iter).has_value(); --n);
    return iter;
}

#endif /* ITER_ADAPTERS_SKIP_EAGER_HPP */

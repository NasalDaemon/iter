#ifndef INCLUDE_ITER_SUM_HPP
#define INCLUDE_ITER_SUM_HPP

#include "iter/reduce.hpp"

ITER_DECLARE(sum)

template<iter::assert_iterable I>
requires std::is_arithmetic_v<iter::value_t<I>>
constexpr auto ITER_IMPL(sum) (I&& iterable) {
    std::remove_const_t<iter::value_t<I>> sum = 0;
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (auto val = iter::detail::impl::next(iter)) {
        sum += *val;
    }
    return sum;
}

#endif /* INCLUDE_ITER_SUM_HPP */

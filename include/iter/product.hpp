#ifndef INCLUDE_ITER_PRODUCT_HPP
#define INCLUDE_ITER_PRODUCT_HPP

#include "iter/core.hpp"

ITER_DECLARE(product)

template<iter::iterable I>
requires std::is_arithmetic_v<iter::value_t<I>>
constexpr auto ITER_IMPL(product) (I&& iterable) {
    std::remove_const_t<iter::value_t<I>> product = 1;
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (auto val = iter::next(iter)) {
        product *= *val;
    }
    return product;
}

#endif /* INCLUDE_ITER_PRODUCT_HPP */

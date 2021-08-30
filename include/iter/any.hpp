#ifndef INCLUDE_ITER_ANY_HPP
#define INCLUDE_ITER_ANY_HPP

#include "iter/core.hpp"

ITER_DECLARE(any)

template<iter::iterable I, std::predicate<iter::ref_t<I>> P>
constexpr auto ITER_IMPL(any) (I&& iterable, P&& predicate) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (auto val = iter::next(iter)) {
        if ((FWD(predicate))(*val)) {
            return true;
        }
    }

    return false;
}

#endif /* INCLUDE_ITER_ANY_HPP */

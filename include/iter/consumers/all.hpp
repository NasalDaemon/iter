#ifndef ITER_CONSUMERS_ALL_HPP
#define ITER_CONSUMERS_ALL_HPP

#include "iter/core/core.hpp"

ITER_DECLARE(all)

template<iter::assert_iterable I, std::predicate<iter::ref_t<I>> P>
constexpr auto ITER_IMPL(all) (I&& iterable, P&& predicate) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (auto val = iter::detail::impl::next(iter)) {
        if (!FWD(predicate)(*val)) {
            return false;
        }
    }

    return true;
}

#endif /* ITER_CONSUMERS_ALL_HPP */

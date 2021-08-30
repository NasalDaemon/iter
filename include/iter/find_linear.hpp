#ifndef INCLUDE_ITER_FIND_LINEAR_HPP
#define INCLUDE_ITER_FIND_LINEAR_HPP

#include "iter/core.hpp"

ITER_DECLARE(find_linear)

template<iter::assert_iterable I, std::predicate<iter::ref_t<I>> P>
constexpr auto ITER_IMPL(find_linear) (I&& iterable, P&& predicate) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    auto val = iter::no_next<decltype(iter)>();
    while (iter::detail::emplace_next(val, iter)) {
        if (std::invoke(FWD(predicate), *val)) {
            break;
        }
    }
    if constexpr (iter::concepts::optional_iterable<I> || std::is_lvalue_reference_v<I>)
        return val;
    else
        return val ? std::make_optional(*val) : std::nullopt;
}

#endif /* INCLUDE_ITER_FIND_LINEAR_HPP */

#ifndef INCLUDE_ITER_TAKE_WHILE_HPP
#define INCLUDE_ITER_TAKE_WHILE_HPP

#include "iter/core.hpp"

ITER_DECLARE(take_while)

namespace iter::detail {
    template<assert_iter I, std::predicate<ref_t<I>> P>
    struct take_while_iter {
        using this_t = take_while_iter;

        [[no_unique_address]] I i;
        [[no_unique_address]] P pred;

        constexpr next_t<I> ITER_IMPL_THIS(next) (this_t& self) {
            auto val = iter::next(self.i);
            return val && self.pred(*val) ? val : no_next<I>(); // TODO test RVO
        }
    };

    template<class I, class P>
    take_while_iter(I, P) -> take_while_iter<I, P>;
}

template<iter::assert_iterable I, std::predicate<iter::ref_t<I>> P>
constexpr auto ITER_IMPL(take_while) (I&& iterable, P&& predicate) {
    return iter::detail::take_while_iter<iter::iter_t<I>, std::remove_cvref_t<P>>{.i = iter::to_iter(FWD(iterable)), .pred = FWD(predicate)};
}

#endif /* INCLUDE_ITER_TAKE_WHILE_HPP */

#ifndef INCLUDE_ITER_FILTER_HPP
#define INCLUDE_ITER_FILTER_HPP

#include "iter/core.hpp"

ITER_DECLARE(filter)

namespace iter::detail {
    template<assert_iter I, std::predicate<ref_t<I>> P>
    struct [[nodiscard]] filter_iter {
        using this_t = filter_iter;

        [[no_unique_address]] I i;
        [[no_unique_address]] P pred;

        constexpr next_t<I> ITER_IMPL_THIS(next) (this_t& self) {
            auto val = no_next<I>();
            while (emplace_next(val, self.i)) [[unlikely]] {
                if (self.pred(*val)) {
                    return val;
                }
            }

            return val;
        }
    };

    template<class I, class P>
    filter_iter(I, P) -> filter_iter<I, P>;
}

template<iter::assert_iterable I, std::predicate<iter::ref_t<I>> P>
constexpr auto ITER_IMPL(filter) (I&& iterable, P&& pred) {
    return iter::detail::filter_iter{.i = iter::to_iter(FWD(iterable)), .pred = FWD(pred)};
}

#endif /* INCLUDE_ITER_FILTER_HPP */

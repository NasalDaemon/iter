#ifndef INCLUDE_ITER_SKIP_WHILE_HPP
#define INCLUDE_ITER_SKIP_WHILE_HPP

#include "iter/core.hpp"

ITER_DECLARE(skip_while)

namespace iter::detail {
    template<assert_iter I, std::predicate<cref_t<I>> P>
    struct skip_while_iter {
        using this_t = skip_while_iter;

        template<class T, std::predicate<cref_t<I>> U>
        constexpr skip_while_iter(T&& i, U&& pred) : i{FWD(i)}, pred{std::make_optional(FWD(pred))}
        {}

    private:
        I i;
        std::optional<P> pred;

        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
            auto next = no_next<I>();
            while (emplace_next(next, self.i)) {
                if (self.pred) [[unlikely]] {
                    if (std::invoke(*self.pred, as_const(*next))) {
                        continue;
                    }
                    self.pred.reset();
                }
                return next;
            }
            return next;
        }
    };

    template<class I, class P>
    skip_while_iter(I, P) -> skip_while_iter<I, P>;
}

template<iter::assert_iterable I, std::predicate<iter::cref_t<I>> P>
constexpr auto ITER_IMPL(skip_while) (I&& iterable, P&& pred) {
    return iter::detail::skip_while_iter(iter::to_iter(FWD(iterable)), FWD(pred));
}

#endif /* INCLUDE_ITER_SKIP_WHILE_HPP */

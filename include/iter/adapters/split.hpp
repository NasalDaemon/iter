#ifndef INCLUDE_ITER_SPLIT_HPP
#define INCLUDE_ITER_SPLIT_HPP

#include "iter/adapters/take.hpp"
#include "iter/adapters/map.hpp"

ITER_DECLARE(split)

namespace iter::detail {
    template<assert_iter I>
    struct [[nodiscard]] split_iter_inner {
        [[no_unique_address]] I i;
        value_t<I> delimiter;
        bool end = false;

        using this_t = split_iter_inner;
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            auto next = iter::no_next<I>();
            if (!emplace_next(next, self.i)) [[unlikely]] {
                self.end = true;
            } else if (*next == self.delimiter) [[unlikely]] {
                next.reset();
            }
            return next;
        }
    };

    template<assert_iter I>
    struct [[nodiscard]] split_iter : split_iter_inner<I> {
        using this_t = split_iter;
        constexpr item<split_iter_inner<I>&> ITER_IMPL_NEXT (this_t& self) {
            if (!self.end) [[likely]] {
                return self;
            }
            return noitem;
        }
    };
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(split) (I&& iterable, iter::value_t<I> delimiter) {
    return iter::detail::split_iter<iter::iter_t<I>>{
        {.i = iter::to_iter(FWD(iterable)), .delimiter = delimiter}};
}

#endif /* INCLUDE_ITER_SPLIT_HPP */

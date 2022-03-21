#ifndef INCLUDE_ITER_SPLIT_HPP
#define INCLUDE_ITER_SPLIT_HPP

#include "iter/adapters/take.hpp"
#include "iter/adapters/map.hpp"

ITER_DECLARE(split)

namespace iter::detail {
    // Book-keeping to ensure no infinite loop if inner iter is not iterated
    enum class inner_iter_status {
        created, inner_unfinished, inner_finished, outer_finished
    };

    template<assert_iter I>
    struct split_iter_inner {
        [[no_unique_address]] I i;
        value_t<I> delimiter;
        inner_iter_status status = inner_iter_status::created;

        using this_t = split_iter_inner;
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            auto next = iter::no_next<I>();
            if (!emplace_next(next, self.i)) [[unlikely]] {
                self.status = inner_iter_status::outer_finished;
            } else if (*next == self.delimiter) [[unlikely]] {
                self.status = inner_iter_status::inner_finished;
                next.reset();
            }
            return next;
        }
    };

    template<assert_iter I>
    struct [[nodiscard]] split_iter : split_iter_inner<I> {
        using this_t = split_iter;

        constexpr stable_item<split_iter_inner<I>&> ITER_IMPL_NEXT (this_t& self) {
            if (self.status == inner_iter_status::outer_finished)
                return noitem;
            if (self.status == inner_iter_status::inner_unfinished) [[unlikely]] {
                // Inner iter was not fully iterated, so we must complete the inner iter
                while (impl::next(static_cast<split_iter_inner<I>&>(self)));
                if (self.status == inner_iter_status::outer_finished)
                    return noitem;
            }
            self.status = inner_iter_status::inner_unfinished;
            return stable_ref<split_iter_inner<I>&>(self);
        }
    };
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(split) (I&& iterable, iter::value_t<I> delimiter) {
    return iter::detail::split_iter<iter::iter_t<I>>{
        {.i = iter::to_iter(FWD(iterable)), .delimiter = delimiter}};
}

#endif /* INCLUDE_ITER_SPLIT_HPP */

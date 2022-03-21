#ifndef INCLUDE_ITER_SPLIT_HPP
#define INCLUDE_ITER_SPLIT_HPP

#include "iter/adapters/take.hpp"
#include "iter/adapters/map.hpp"

ITER_DECLARE(split)

namespace iter::detail {
    // Book-keeping to ensure no infinite loop if inner iter is not iterated
    enum class inner_iter_status {
        created, unstarted, started, finished
    };

    template<assert_iter I>
    struct split_iter_inner {
        [[no_unique_address]] I i;
        value_t<I> delimiter;
        inner_iter_status status = inner_iter_status::created;

        using this_t = split_iter_inner;
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            self.status = inner_iter_status::started;
            auto next = iter::no_next<I>();
            if (!emplace_next(next, self.i)) [[unlikely]] {
                self.status = inner_iter_status::finished;
            } else if (*next == self.delimiter) [[unlikely]] {
                next.reset();
            }
            return next;
        }
    };

    template<assert_iter I>
    struct [[nodiscard]] split_iter : split_iter_inner<I> {
        using this_t = split_iter;

        constexpr stable_item<split_iter_inner<I>&> ITER_IMPL_NEXT (this_t& self) {
            if (self.status == inner_iter_status::finished) [[likely]] {
                return noitem;
            } else if (self.status == inner_iter_status::created) [[unlikely]] {
                self.status = inner_iter_status::unstarted;
            } else if (self.status == inner_iter_status::unstarted) [[unlikely]] {
                // Inner iter was not iterated since last time next was called,
                // so we must do it ourselves to avoid an infinite loop
                while (impl::next(static_cast<split_iter_inner<I>&>(self))) {}
                if (self.status == inner_iter_status::finished) [[unlikely]] {
                    return noitem;
                }
                self.status = inner_iter_status::unstarted;
            }
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

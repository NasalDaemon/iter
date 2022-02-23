#ifndef INCLUDE_ITER_SKIP_HPP
#define INCLUDE_ITER_SKIP_HPP

#include "iter/core/core.hpp"

ITER_DECLARE(skip)

namespace iter::detail {
    template<assert_iter I>
    struct skip_iter : enable_random_access<skip_iter<I>, I> {
        [[no_unique_address]] I i;
        std::size_t n;

        using this_t = skip_iter;
        constexpr auto ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            auto next = no_next<I>();
            while (emplace_next(next, self.i)) {
                if (self.n) [[unlikely]] {
                    --self.n;
                    continue;
                }
                return next;
            }
            return next;
        }

        constexpr auto ITER_IMPL_SIZE (this_t const& self)
            requires this_t::random_access
        {
            std::size_t size = impl::size(self.i);
            return size > self.n ? size - self.n : 0;
        }

        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return impl::get(self.i, index + self.n);
        }
    };

    template<class I>
    skip_iter(I, std::size_t) -> skip_iter<I>;
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(skip) (I&& iterable, std::size_t n) {
    return iter::detail::skip_iter<iter::iter_t<I>>{.i = iter::to_iter(FWD(iterable)), .n = n};
}

#endif /* INCLUDE_ITER_SKIP_HPP */

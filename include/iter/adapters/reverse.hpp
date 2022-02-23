#ifndef INCLUDE_ITER_REVERSE_HPP
#define INCLUDE_ITER_REVERSE_HPP

#include "iter/core/core.hpp"

ITER_DECLARE(reverse)

namespace iter::detail {
    template<assert_iter I>
    struct reverse_iter : enable_random_access<reverse_iter<I>, I> {
        static_assert(concepts::double_ended_iter<I>);
        [[no_unique_address]] I i;

        using this_t = reverse_iter;
        constexpr auto ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            return impl::next_back(self.i);
        }
        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self)
            requires (!this_t::random_access)
        {
            return impl::next(self.i);
        }

        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return impl::get(self.i, impl::size(self.i) - index - 1);
        }

        constexpr auto ITER_IMPL_THIS(reverse) (this_t&& self) { return std::move(self.i); }
        constexpr auto ITER_IMPL_THIS(reverse) (this_t const& self) { return self.i; }
    };
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(reverse) (I&& iterable) {
    static_assert(iter::concepts::double_ended_iter<iter::iter_t<I>>);
    return iter::detail::reverse_iter<iter::iter_t<I>>{.i = iter::to_iter(FWD(iterable))};
}

#endif /* INCLUDE_ITER_REVERSE_HPP */

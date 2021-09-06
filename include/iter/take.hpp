#ifndef INCLUDE_ITER_TAKE_HPP
#define INCLUDE_ITER_TAKE_HPP

#include "iter/core.hpp"

ITER_DECLARE(take)

namespace iter::detail {
    template<assert_iter I>
    struct take_iter : enable_random_access<take_iter<I>, I> {
        [[no_unique_address]] I i;
        std::size_t n;

        using this_t = take_iter;

        constexpr auto ITER_IMPL_THIS(next) (this_t& self)
            requires (!this_t::random_access)
        {
            return self.n-- > 0 ? iter::next(self.i) : no_next<I>();
        }

        constexpr auto ITER_UNSAFE_SIZE (this_t const& self)
            requires this_t::random_access
        {
            return std::min(self.n, iter::unsafe::size(self.i));
        }

        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return iter::unsafe::get(self.i, index);
        }
    };

    template<class I>
    take_iter(I, std::size_t) -> take_iter<I>;
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(take) (I&& iterable, std::size_t n) {
    return iter::detail::take_iter{.i = iter::to_iter(FWD(iterable)), .n = n};
}

#endif /* INCLUDE_ITER_TAKE_HPP */

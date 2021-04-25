#ifndef INCLUDE_ITER_TAKE_HPP
#define INCLUDE_ITER_TAKE_HPP

#include "iter/core.hpp"

ITER_DECLARE(take)

namespace iter::detail {
    template<iter I>
    struct take_iter : enable_random_access<take_iter<I>, I> {
        using this_t = take_iter;

        template<class T>
        constexpr take_iter(T&& i, std::size_t n)
            : this_t::base_t{(T&&) i}
            , n{n}
        {}

    private:
        std::size_t n;

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

template<iter::iterable I>
constexpr auto ITER_IMPL(take) (I&& iterable, std::size_t n) {
    return iter::detail::take_iter(iter::to_iter((I&&) iterable), n);
}

#endif /* INCLUDE_ITER_TAKE_HPP */

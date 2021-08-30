#ifndef INCLUDE_ITER_SKIP_HPP
#define INCLUDE_ITER_SKIP_HPP

#include "iter/core.hpp"

ITER_DECLARE(skip)

namespace iter::detail {
    template<iter I>
    struct skip_iter : enable_random_access<skip_iter<I>, I> {
        using this_t = skip_iter;

        template<class T>
        constexpr skip_iter(T&& i, std::size_t n)
            : this_t::base_t{FWD(i)}
            , n{n}
        {}

    private:
        std::size_t n;

        constexpr auto ITER_IMPL_THIS(next) (this_t& self)
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

        constexpr auto ITER_UNSAFE_SIZE (this_t const& self)
            requires this_t::random_access
        {
            std::size_t size = iter::unsafe::size(self.i);
            return size > self.n ? size - self.n : 0;
        }

        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return iter::unsafe::get(self.i, index + self.n);
        }
    };

    template<class I>
    skip_iter(I, std::size_t) -> skip_iter<I>;
}

template<iter::iterable I>
constexpr auto ITER_IMPL(skip) (I&& iterable, std::size_t n) {
    return iter::detail::skip_iter(iter::to_iter(FWD(iterable)), n);
}

#endif /* INCLUDE_ITER_SKIP_HPP */

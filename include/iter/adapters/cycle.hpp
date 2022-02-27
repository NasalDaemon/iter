#ifndef ITER_ADAPTERS_CYCLE_HPP
#define ITER_ADAPTERS_CYCLE_HPP

#include "iter/core/core.hpp"

namespace iter::detail {
    template<assert_iter I>
    struct [[nodiscard]] cycle_iter : enable_random_access<cycle_iter<I>, I> {
        using this_t = cycle_iter;
        [[no_unique_address]] I i;
        [[no_unique_address]] std::conditional_t<this_t::random_access, void_t, I> i_orig = i;

    private:
        constexpr std::size_t ITER_IMPL_SIZE (this_t const& self)
            requires this_t::random_access
        {
            return std::numeric_limits<std::size_t>::max();
        }

        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return impl::get(self.i, index % impl::size(self.i));
        }

        constexpr next_t<I> ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            auto val = impl::next(self.i);
            if (val) [[likely]] {
                return val;
            }
            // assume we are at the end of the underlying, so reinitialise
            EMPLACE_NEW(self.i, self.i_orig);
            emplace_next(val, self.i);
            return val; // if first value is empty, then this prevents an infinite loop
        }
    };

    template<class T>
    cycle_iter(T) -> cycle_iter<T>;
}

template<iter::iter I>
constexpr auto ITER_IMPL(cycle) (I&& iter) {
    return iter::detail::cycle_iter<std::remove_cvref_t<I>>{.i = FWD(iter)};
}

template<class I>
constexpr auto ITER_IMPL(cycle) (I&& iterable) {
    static_assert(iter::iterable<I>);
    return iter::cycle(iter::to_iter(FWD(iterable)));
}

#endif /* ITER_ADAPTERS_CYCLE_HPP */

#ifndef INCLUDE_ITER_CYCLE_HPP
#define INCLUDE_ITER_CYCLE_HPP

#include "iter/core.hpp"

namespace iter::detail {
    template<assert_iter I>
    struct cycle_iter : enable_random_access<cycle_iter<I>, I> {
        using this_t = cycle_iter;
        using base_t = typename this_t::base_t;

        template<class T>
        requires (!std::same_as<std::remove_cvref_t<T>, cycle_iter>)
        constexpr explicit cycle_iter(T&& i_) : base_t{FWD(i_)}, i_orig{this->i} {}

        constexpr cycle_iter(cycle_iter const& other)
            : base_t{static_cast<base_t const&>(other)}, i_orig{this->i} {}
        constexpr cycle_iter(cycle_iter&& other)
            : base_t{static_cast<base_t&&>(other)}, i_orig{this->i} {}

        [[no_unique_address]] std::conditional_t<this_t::random_access, void_t, I> i_orig;

        constexpr std::size_t ITER_UNSAFE_SIZE (this_t const& self)
            requires this_t::random_access
        {
            return std::numeric_limits<std::size_t>::max();
        }

        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return iter::unsafe::get(self.i, index % iter::unsafe::size(self.i));
        }

        constexpr next_t<I> ITER_IMPL_THIS(next) (this_t& self)
            requires (!this_t::random_access)
        {
            auto val = iter::next(self.i);
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
    return iter::detail::cycle_iter(FWD(iter));
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(cycle) (I&& iterable) {
    return iter::cycle(iter::to_iter(FWD(iterable)));
}

#endif /* INCLUDE_ITER_CYCLE_HPP */

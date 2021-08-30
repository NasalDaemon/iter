#ifndef INCLUDE_ITER_MOVE_HPP
#define INCLUDE_ITER_MOVE_HPP

#include "iter/core.hpp"

ITER_DECLARE(move)

namespace iter::detail {
    template<iter::assert_iter I>
    struct move_iter : enable_random_access<move_iter<I>, I> {
        using this_t = move_iter;

        template<class T> requires (!std::same_as<this_t, std::remove_cvref_t<T>>)
        constexpr explicit move_iter(T&& i) : this_t::base_t{FWD(i)} {}

        constexpr move_next<next_t<I>> ITER_IMPL_THIS(next) (this_t& self)
            requires (!this_t::random_access)
        {
            return move_next{iter::next(self.i)};
        }

        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            decltype(auto) item = iter::unsafe::get(self.i, index);
            if constexpr (std::is_reference_v<decltype(item)>)
                return std::move(item);
            else
                return item;
        }
    };

    template<class T>
    move_iter(T) -> move_iter<T>;
}

template<iter::assert_iterable I>
constexpr decltype(auto) ITER_IMPL(move) (I&& iterable) {
    if constexpr (iter::concepts::move_next<iter::next_t<I>>)
        return FWD(iterable);
    else
        return iter::detail::move_iter(iter::to_iter(FWD(iterable)));
}

#endif /* INCLUDE_ITER_MOVE_HPP */

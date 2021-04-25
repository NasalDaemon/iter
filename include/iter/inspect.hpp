#ifndef INCLUDE_ITER_INSPECT_HPP
#define INCLUDE_ITER_INSPECT_HPP

#include "iter/core.hpp"

ITER_DECLARE(inspect)

namespace iter::detail {
    template<iter I, concepts::inspector<ref_t<I>> F>
    struct [[nodiscard]] inspect_iter : enable_random_access<inspect_iter<I, F>, I> {
        using this_t = inspect_iter;

        template<class T, class U>
        constexpr inspect_iter(T&& i, U&& f)
            : this_t::base_t{(T&&)i}
            , func{(U&&)f}
        {}

    private:
        [[no_unique_address]] F func;

        constexpr next_t<I> ITER_IMPL_THIS(next) (this_t& self)
            requires (!this_t::random_access)
        {
            auto val = iter::next(self.i);
            if (val) {
                self.func(*val);
            }
            return val;
        }

        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            decltype(auto) val = iter::unsafe::get(self.i, index);
            self.func(val);
            return val;
        }
    };

    template<class I, class F>
    inspect_iter(I, F) -> inspect_iter<I, F>;
}

template<iter::iterable I, iter::concepts::inspector<iter::ref_t<I>> F>
constexpr auto ITER_IMPL(inspect) (I&& iterable, F func) {
    return iter::detail::inspect_iter{iter::to_iter((I&&) iterable), std::move(func)};
}

#endif /* INCLUDE_ITER_INSPECT_HPP */

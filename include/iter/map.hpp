#ifndef INCLUDE_ITER_MAP_HPP
#define INCLUDE_ITER_MAP_HPP

#include "iter/flatten.hpp"
#include "iter/flatmap.hpp"

ITER_DECLARE(map)

namespace iter::detail {
    template<iter I, std::invocable<consume_t<I>> F>
    struct [[nodiscard]] map_iter : enable_random_access<map_iter<I, F>, I> {
        using this_t = map_iter;

        template<class T, class U>
        constexpr map_iter(T&& i, U&& f)
            : this_t::base_t{(T&&) i}
            , func{(U&&) f}
        {}

    private:
        [[no_unique_address]] F func;

        constexpr std::optional<std::invoke_result_t<F, consume_t<I>>> ITER_IMPL_THIS(next) (this_t& self)
            requires (!this_t::random_access)
        {
            auto val = iter::next(self.i);
            return val ? MAKE_OPTIONAL(self.func(consume(val))) : std::nullopt;
        }

        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return self.func(iter::unsafe::get(self.i, index));
        }

        constexpr auto ITER_IMPL_THIS(flatten) (this_t&& self) {
            return iter::flatmap(std::move(self.i), std::move(self.func));
        }
        constexpr auto ITER_IMPL_THIS(flatten) (this_t const& self) {
            return iter::flatmap(self.i, self.func);
        }
    };

    template<class I, class F>
    map_iter(I, F) -> map_iter<I, F>;
}

template<iter::iterable I, std::invocable<iter::consume_t<I>> F>
constexpr auto ITER_IMPL(map) (I&& iterable, F&& func) {
    return iter::detail::map_iter{iter::to_iter((I&&) iterable), (F&&) func};
}

#endif /* INCLUDE_ITER_MAP_HPP */

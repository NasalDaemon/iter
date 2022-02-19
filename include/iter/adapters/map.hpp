#ifndef INCLUDE_ITER_MAP_HPP
#define INCLUDE_ITER_MAP_HPP

#include "iter/adapters/flatten.hpp"
#include "iter/adapters/flatmap.hpp"

ITER_DECLARE(map)

namespace iter::detail {
    template<assert_iter I, std::invocable<consume_t<I>> F>
    struct [[nodiscard]] map_iter : enable_random_access<map_iter<I, F>, I> {
        [[no_unique_address]] I i;
        [[no_unique_address]] F func;

    private:
        using this_t = map_iter;
        using result_t = std::invoke_result_t<F, consume_t<I>>;

        constexpr auto ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            auto val = impl::next(self.i);
            return val ? MAKE_ITEM_AUTO(self.func(consume(val))) : noitem;
        }

        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return self.func(impl::get(self.i, index));
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

template<iter::assert_iterable I, std::invocable<iter::consume_t<I>> F>
constexpr auto ITER_IMPL(map) (I&& iterable, F&& func) {
    return iter::detail::map_iter<iter::iter_t<I>, std::remove_cvref_t<F>>{.i = iter::to_iter(FWD(iterable)), .func = FWD(func)};
}

#endif /* INCLUDE_ITER_MAP_HPP */

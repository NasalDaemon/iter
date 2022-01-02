#ifndef INCLUDE_ITER_MAP_WHILE_HPP
#define INCLUDE_ITER_MAP_WHILE_HPP

#include "iter/core.hpp"

ITER_DECLARE(map_while)

namespace iter::detail {
    template<assert_iter I, std::invocable<consume_t<I>> F>
    struct [[nodiscard]] map_while_iter {
        using this_t = map_while_iter;
        using mapped_t = std::invoke_result_t<F, consume_t<I>>;
        static_assert(concepts::item<mapped_t>);

        [[no_unique_address]] I i;
        [[no_unique_address]] F func;

        constexpr mapped_t ITER_IMPL_NEXT (this_t& self) {
            auto val = impl::next(self.i);
            return val ? self.func(consume(val)) : noitem;
        }
    };

    template<class I, class P>
    map_while_iter(I, P) -> map_while_iter<I, P>;
}

template<iter::assert_iterable I, std::invocable<iter::consume_t<I>> F>
constexpr auto ITER_IMPL(map_while) (I&& iterable, F&& func) {
    return iter::detail::map_while_iter<iter::iter_t<I>, std::remove_cvref_t<F>>{.i = iter::to_iter(FWD(iterable)), .func = FWD(func)};
}

#endif /* INCLUDE_ITER_MAP_WHILE_HPP */

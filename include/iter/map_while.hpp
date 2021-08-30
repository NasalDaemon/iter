#ifndef INCLUDE_ITER_MAP_WHILE_HPP
#define INCLUDE_ITER_MAP_WHILE_HPP

#include "iter/core.hpp"

ITER_DECLARE(map_while)

namespace iter::detail {
    template<iter I, std::invocable<consume_t<I>> F>
    struct [[nodiscard]] map_while_iter {
        using this_t = map_while_iter;
        using mapped_t = std::invoke_result_t<F, ref_t<I>>;
        static_assert(concepts::optional_next<mapped_t> || concepts::pointer_next<mapped_t>);

        I i;
        [[no_unique_address]] F func;

        constexpr mapped_t ITER_IMPL_THIS(next) (this_t& self) {
            auto val = iter::next(self.i);
            return val ? self.func(consume(val)) : mapped_t{};
        }
    };

    template<class I, class P>
    map_while_iter(I, P) -> map_while_iter<I, P>;
}

template<iter::iterable I, std::invocable<iter::consume_t<I>> F>
constexpr auto ITER_IMPL(map_while) (I&& iterable, F&& func) {
    return iter::detail::map_while_iter{iter::to_iter(FWD(iterable)), FWD(func)};
}

#endif /* INCLUDE_ITER_MAP_WHILE_HPP */

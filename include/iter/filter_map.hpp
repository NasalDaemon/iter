#ifndef INCLUDE_ITER_FILTER_MAP_HPP
#define INCLUDE_ITER_FILTER_MAP_HPP

#include "iter/core.hpp"

ITER_DECLARE(filter_map)

namespace iter::detail {
    template<assert_iter I, std::invocable<consume_t<I>> F>
    struct [[nodiscard]] filter_map_iter {
        using this_t = filter_map_iter;
        using mapped_t = std::invoke_result_t<F, ref_t<I>>;
        static_assert(concepts::item<mapped_t>);

        [[no_unique_address]] I i;
        [[no_unique_address]] F func;

        constexpr mapped_t ITER_IMPL_NEXT (this_t& self) {
            mapped_t mapped;
            while (auto val = impl::next(self.i)) {
                if (EMPLACE_NEW(mapped, self.func(consume(val)))) {
                    return mapped;
                }
            }
            return mapped;
        }
    };

    template<class I, class P>
    filter_map_iter(I, P) -> filter_map_iter<I, P>;
}

template<iter::assert_iterable I, std::invocable<iter::consume_t<I>> F>
constexpr auto ITER_IMPL(filter_map) (I&& iterable, F&& func) {
    return iter::detail::filter_map_iter<iter::iter_t<I>, std::remove_cvref_t<F>>{.i = iter::to_iter(FWD(iterable)), .func = FWD(func)};
}

#endif /* INCLUDE_ITER_FILTER_MAP_HPP */

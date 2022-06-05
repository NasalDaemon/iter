#ifndef ITER_ADAPTERS_BATCHING_HPP
#define ITER_ADAPTERS_BATCHING_HPP

#include "iter/core/core.hpp"

ITER_DECLARE(batching)

namespace iter::detail {
    template<iter I, std::invocable<I&> F>
    struct [[nodiscard]] batching_iter {
        using this_t = batching_iter;

        static_assert(concepts::item<std::invoke_result_t<F, I&>>);

        [[no_unique_address]] I i;
        [[no_unique_address]] F func;

        constexpr auto ITER_IMPL_NEXT(this_t& self) {
            return std::invoke(self.func, self.i);
        }
    };

    template<class I, class F>
    batching_iter(I, F) -> batching_iter<I, F>;
}

template<iter::assert_iterable I, std::invocable<iter::iter_t<I>&> F>
constexpr auto ITER_IMPL(batching) (I&& iterable, F&& func) {
    return iter::detail::batching_iter{iter::to_iter(FWD(iterable)), FWD(func)};
}

#endif /* ITER_ADAPTERS_BATCHING_HPP */

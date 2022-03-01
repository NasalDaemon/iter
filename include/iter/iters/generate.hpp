#ifndef ITER_ITERS_GENERATE_HPP
#define ITER_ITERS_GENERATE_HPP

#include "iter/core/core.hpp"

namespace iter {
    template<std::invocable F>
    requires concepts::item<std::invoke_result_t<F>>
    struct [[nodiscard]] generate : F {
        using this_t = generate;
        constexpr decltype(auto) ITER_IMPL_NEXT (this_t& self) {
            return self();
        }
    };

    template<class F>
    generate(F) -> generate<F>;
}

#endif /* ITER_ITERS_GENERATE_HPP */

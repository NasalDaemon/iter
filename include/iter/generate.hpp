#ifndef INCLUDE_ITER_GENERATE_HPP
#define INCLUDE_ITER_GENERATE_HPP

#include "iter/core.hpp"

namespace iter {
    template<std::invocable<> F>
    requires concepts::optional_next<std::invoke_result_t<F>>
          || concepts::pointer_next<std::invoke_result_t<F>>
    struct generate : F {
        using this_t = generate;
        constexpr decltype(auto) ITER_IMPL_THIS(next) (this_t& self) {
            return self();
        }
    };

    template<class F>
    generate(F) -> generate<F>;
}

#endif /* INCLUDE_ITER_GENERATE_HPP */

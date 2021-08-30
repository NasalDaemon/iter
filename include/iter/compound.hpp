#ifndef INCLUDE_ITER_COMPOUND_HPP
#define INCLUDE_ITER_COMPOUND_HPP

#include "iter/core.hpp"

namespace iter {
    template<class A, class F>
    requires std::constructible_from<std::optional<A>, std::invoke_result_t<F, A const&>>
    struct compound {
        using this_t = compound;

        template<class T, class U>
        constexpr compound(T&& init, U&& func) : value{FWD(init)}, func{FWD(func)} {}

    private:
        std::optional<A> value;
        F func;

        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
            auto result = std::move(self.value);
            if (result) {
                EMPLACE_NEW(self.value, self.func(iter::as_const(*result)));
            }
            return result;
        }
    };

    template<class A, class F>
    compound(A, F) -> compound<A, F>;
}

#endif /* INCLUDE_ITER_COMPOUND_HPP */

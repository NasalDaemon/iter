#ifndef ITER_CORE_ASSERT_CONSTEVAL_HPP
#define ITER_CORE_ASSERT_CONSTEVAL_HPP

namespace iter::detail {
    class unconstructible { unconstructible(auto&&...); };
    template<class...>
    struct fail_compile_at_linker {
        // Impossible to define anywhere, so will always fail to link
        static unconstructible undefinable();
    };
    struct consteval_function_invoked_at_runtime;
    template<class... Ts>
    static constexpr void assert_consteval() {
        // Fail to compile at linker stage if this will be called at runtime.
        // Cannot fail any earlier, as it will prevent genuine constexpr calls.
        if (!std::is_constant_evaluated())
            fail_compile_at_linker<consteval_function_invoked_at_runtime, Ts...>::undefinable();
    }
}

#endif /* ITER_CORE_ASSERT_CONSTEVAL_HPP */

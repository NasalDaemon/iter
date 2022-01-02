#ifndef INCLUDE_ITER_EMPLACE_NEW_HPP
#define INCLUDE_ITER_EMPLACE_NEW_HPP

// Semantically equivalent to `current = expr`, but constructs expr
// directly inside current's memory without any intermediate moves
#define EMPLACE_NEW(current, ... /*expr*/) \
    ::iter::detail::emplace_new_impl(current, [&]<class T_UGLY>() { return T_UGLY(__VA_ARGS__); })

// Semantically equivalent to `std::make_optional(expr)`, but constructs expr
// directly inside the optional's payload without any intermediate moves
#define MAKE_OPTIONAL(... /*expr*/) \
    ::iter::detail::make_optional_impl([&]() -> decltype(auto) { return (__VA_ARGS__); })

namespace iter {
    struct void_t {
        template<class... Ts> constexpr void_t(Ts&&...) {}
    };
    namespace detail {
        struct constexpr_new_tag {};
    }
}

constexpr void* operator new(std::size_t, void* ptr, iter::detail::constexpr_new_tag) {
    return ptr;
}

namespace iter::detail {
    template<class T, class F>
    requires (!std::is_const_v<T>)
    static constexpr T& emplace_new_impl(T& current, F&& ctor) {
        current.~T();
        // if constexpr (std::constructible_from<T, T>) {
        //     if (std::is_constant_evaluated()) {
        //         // placement new not strictly speaking constexpr although GCC allows it
        //         return std::construct_at(std::addressof(current), FWD(ctor).template operator()<T>());
        //     }
        // }
        new (std::addressof(current), constexpr_new_tag{}) T(FWD(ctor).template operator()<T>());
        return current;
    }

    struct rvo_empty_base {};
    template<class T>
    struct rvo_ctor : rvo_empty_base {
        T value;
        template<class F>
        rvo_ctor(F&& f) : value{std::invoke(FWD(f))} {}
    };

    template<class F>
    std::optional<std::remove_cvref_t<std::invoke_result_t<F&&>>> make_optional_runtime_impl(F&& func) {
        using T = std::remove_cvref_t<std::invoke_result_t<F&&>>;
        using return_t = std::optional<T>;
        using emplace_t = std::optional<rvo_ctor<T>>;
        static_assert(sizeof(return_t) == sizeof(emplace_t));

        return_t option;
        new (std::addressof(option)) emplace_t(std::in_place, FWD(func));
        return option;
    }

    template<class F>
    constexpr auto make_optional_impl(F&& func) {
        return std::is_constant_evaluated()
            ? std::make_optional(std::invoke(FWD(func)))
            : make_optional_runtime_impl(FWD(func));
    }
}

#endif /* INCLUDE_ITER_EMPLACE_NEW_HPP */

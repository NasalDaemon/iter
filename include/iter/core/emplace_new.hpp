#ifndef INCLUDE_ITER_EMPLACE_NEW_HPP
#define INCLUDE_ITER_EMPLACE_NEW_HPP

// Semantically equivalent to `current = expr`, but constructs expr
// directly inside current's memory without any intermediate moves
#define EMPLACE_NEW(current, ... /*expr*/) \
    ::iter::detail::emplace_new_impl(current, [&]<class T_UGLY>() { return T_UGLY(__VA_ARGS__); })

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
        if (std::is_constant_evaluated()) {
            // placement new not strictly speaking constexpr although GCC allows it
            std::construct_at(std::addressof(current), FWD(ctor).template operator()<T>());
        } else {
            new (std::addressof(current), constexpr_new_tag{}) T(FWD(ctor).template operator()<T>());
        }
        return current;
    }
}

#endif /* INCLUDE_ITER_EMPLACE_NEW_HPP */

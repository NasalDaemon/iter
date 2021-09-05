#ifndef INCLUDE_ITER_ONCE_HPP
#define INCLUDE_ITER_ONCE_HPP

#include "iter/core.hpp"
#include "iter/repeat.hpp"

namespace iter {
    template<class T>
    struct once {
        using this_t = once;
        template<class... Ts>
        requires std::constructible_from<T, Ts...>
        constexpr once(Ts&&... ins) : value{T(FWD(ins)...)}, on{true} {}
    private:
        [[no_unique_address]] T value;
        bool on;
        constexpr std::size_t ITER_UNSAFE_SIZE (this_t const&) {
            return 1;
        }
        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t) {
            return (self.value);
        }
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
            return self.on ? (self.on = false, std::addressof(self.value)) : nullptr;
        }
        constexpr auto ITER_IMPL_THIS(cycle) (const this_t& self) {
            return repeat{self.value};
        }
        constexpr auto ITER_IMPL_THIS(cycle) (this_t&& self) {
            return repeat{std::move(self.value)};
        }
    };

    template<class F>
    once(F) -> once<F>;

    template<class T>
    struct once_ref {
        using this_t = once_ref;
        constexpr once_ref(T& in) : value{&in} {}
    private:
        T* value;
        constexpr std::size_t ITER_UNSAFE_SIZE (this_t const&) {
            return 1;
        }
        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t) {
            return *self.value;
        }
        constexpr decltype(auto) ITER_IMPL_THIS(next) (this_t& self) {
            return std::exchange(self.value, nullptr);
        }
        constexpr auto ITER_IMPL_THIS(cycle) (const this_t& self) {
            return repeat<T const&>{*self.value};
        }
    };

    template<class F>
    once_ref(F&) -> once_ref<F>;
}

#endif /* INCLUDE_ITER_ONCE_HPP */

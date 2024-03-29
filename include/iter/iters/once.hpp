#ifndef ITER_ITERS_ONCE_HPP
#define ITER_ITERS_ONCE_HPP

#include "iter/core/core.hpp"
#include "iter/iters/repeat.hpp"

namespace iter {
    template<class T>
    struct once {
        using this_t = once;
        [[no_unique_address]] T value;
        bool on = true;
        constexpr std::size_t ITER_IMPL_SIZE (this_t const&) {
            return 1;
        }
        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t) {
            return stable_ref(self.value);
        }
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            return std::exchange(self.on, false) ? item(stable_ref(self.value)) : noitem;
        }
        constexpr auto ITER_IMPL_THIS(cycle) (this_t const& self) {
            return repeat{self.value};
        }
        constexpr auto ITER_IMPL_THIS(cycle) (this_t&& self) {
            return repeat{std::move(self.value)};
        }
    };

    template<class T>
    once(T) -> once<T>;

    template<class T>
    struct once_ref {
        using this_t = once_ref;
        constexpr once_ref(T& in) : value{stable_ref(in)} {}
    private:
        stable_item<T&> value;
        constexpr std::size_t ITER_IMPL_SIZE (this_t const&) {
            return 1;
        }
        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t) {
            return stable_ref(*self.value);
        }
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            return std::exchange(self.value, noitem);
        }
        constexpr auto ITER_IMPL_THIS(cycle) (const this_t& self) {
            return repeat<T const&>{*self.value};
        }
    };

    template<class F>
    once_ref(F&) -> once_ref<F>;
}

#endif /* ITER_ITERS_ONCE_HPP */

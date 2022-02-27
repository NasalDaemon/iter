#ifndef ITER_ITERS_SPAN_HPP
#define ITER_ITERS_SPAN_HPP

#include "iter/core/core.hpp"
#include "iter/iters/random_access_container_iter.hpp"

namespace iter {
    template<class T>
    struct span {
        using this_t = span;
        constexpr span(T* data, std::size_t size) : data{data}, remaining{size} {}

        template<iter::concepts::random_access_container C>
        constexpr explicit span(C& container) : span{std::addressof(container[0]), std::size(container)} {}

        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            return self.remaining
                ? (--self.remaining, item_ref(*self.data++))
                : noitem;
        }
        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
            return self.remaining
                ? item_ref(self.data[--self.remaining])
                : noitem;
        }
        constexpr std::size_t ITER_IMPL_SIZE (this_t const& self) {
            return self.remaining;
        }
        constexpr decltype(auto) ITER_IMPL_GET (this_t const& self, std::size_t n) {
            return self.data[n];
        }
    private:
        T* data;
        std::size_t remaining;
    };

    template<class T>
    span(T*, std::size_t) -> span<T>;
    template<iter::concepts::random_access_container C>
    span(C&) -> span<std::remove_reference_t<decltype(std::declval<C&>()[0])>>;
}

#endif /* ITER_ITERS_SPAN_HPP */

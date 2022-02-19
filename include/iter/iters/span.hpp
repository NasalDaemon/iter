#ifndef INCLUDE_ITER_SPAN_HPP
#define INCLUDE_ITER_SPAN_HPP

#include "iter/core.hpp"

namespace iter {
    template<class T>
    struct span {
        using this_t = span;
        constexpr span(T* p, std::size_t n) : begin{p}, end{p + n} {}

        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            return item_from_pointer(self.begin < self.end ? self.begin++ : nullptr);
        }
        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
            return item_from_pointer(self.begin < self.end ? self.end-- : nullptr);
        }
        constexpr std::size_t ITER_IMPL_SIZE (this_t const& self) {
            return self.end - self.begin;
        }
        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t n) {
            return self.begin[n];
        }
    private:
        T* begin;
        T* end;
    };

    template<class T>
    span(T*) -> span<T>;
}

#endif /* INCLUDE_ITER_SPAN_HPP */

#ifndef INCLUDE_ITER_REPEAT_HPP
#define INCLUDE_ITER_REPEAT_HPP

#include "iter/core.hpp"

namespace iter {
    template<class T>
    struct repeat {
        using this_t = repeat;
        T value;
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            return std::addressof(std::as_const(self.value));
        }
        constexpr auto ITER_IMPL_SIZE (this_t const&) {
            return std::numeric_limits<std::size_t>::max();
        }
        constexpr auto ITER_IMPL_GET (this_t& self, size_t) -> auto& {
            return std::as_const(self.value);
        }
    };

    template<class T>
    repeat(T) -> repeat<T>;
}

#endif /* INCLUDE_ITER_REPEAT_HPP */

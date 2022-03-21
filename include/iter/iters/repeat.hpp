#ifndef INCLUDE_ITER_REPEAT_HPP
#define INCLUDE_ITER_REPEAT_HPP

#include "iter/core/core.hpp"

namespace iter {
    template<class T>
    struct repeat {
        using this_t = repeat;
        T value;
        constexpr auto ITER_IMPL_NEXT (this_t const& self) {
            return item(stable_ref(self.value));
        }
        constexpr auto ITER_IMPL_SIZE (this_t const&) {
            return std::numeric_limits<std::size_t>::max();
        }
        constexpr auto ITER_IMPL_GET (this_t const& self, size_t) {
            return stable_ref(self.value);
        }
    };

    template<class T>
    repeat(T) -> repeat<T>;
}

#endif /* INCLUDE_ITER_REPEAT_HPP */

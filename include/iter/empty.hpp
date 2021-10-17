#ifndef INCLUDE_ITER_EMPTY_HPP
#define INCLUDE_ITER_EMPTY_HPP

#include "iter/core.hpp"

namespace iter {
    namespace detail {
        template<class T>
        struct empty_iter {
            using this_t = empty_iter;
            constexpr auto ITER_IMPL_NEXT (this_t&) -> T* {
                return nullptr;
            }
            constexpr std::size_t ITER_IMPL_SIZE (this_t const&) {
                return 0;
            }
            constexpr auto ITER_IMPL_GET (this_t&, std::size_t) -> T& {
                ITER_UNREACHABLE();
                return reinterpret_cast<T&>(*((T*)0));
            }
        };
    }

    template<class T>
    detail::empty_iter<T> empty = {};
}

#endif /* INCLUDE_ITER_EMPTY_HPP */

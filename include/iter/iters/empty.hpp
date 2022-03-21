#ifndef INCLUDE_ITER_EMPTY_HPP
#define INCLUDE_ITER_EMPTY_HPP

#include "iter/core/core.hpp"

namespace iter {
    namespace detail {
        template<class T>
        struct empty_iter {
            using this_t = empty_iter;
            constexpr item<T&> ITER_IMPL_NEXT (this_t&) {
                return noitem;
            }
            constexpr std::size_t ITER_IMPL_SIZE (this_t const&) {
                return 0;
            }
            constexpr auto ITER_IMPL_GET (this_t const&, std::size_t) {
                ITER_UNREACHABLE();
                return stable_ref(reinterpret_cast<T&>(*((T*)0)));
            }
        };
    }

    template<class T>
    detail::empty_iter<T> empty = {};
}

#endif /* INCLUDE_ITER_EMPTY_HPP */

#ifndef INCLUDE_ITER_ITER_WRAPPER_HPP
#define INCLUDE_ITER_ITER_WRAPPER_HPP

#include "iter/core.hpp"

namespace iter::detail {
    template<class I>
    struct iter_wrapper {
        // static_assert(iterable<I&>);
        I iterable;
        using iter_t = iter::iter_t<I&>;
        iter_t iter = to_iter(iterable);
    };
    template<iter I>
    struct iter_wrapper<I> {
        using iter_t = I;
        I iter;
    };
    template<class I>
    iter_wrapper(I) -> iter_wrapper<I>;

    template<class T>
    struct optional_iter_wrapper {
        // static_assert(iterable<decltype(get_value_t(std::declval<T>()))&>);
        T optional_iterable;
        using iter_t = iter::iter_t<decltype(*optional_iterable)>;
        item<iter_t> optional_iter = optional_iterable ? MAKE_ITEM(to_iter(*optional_iterable)) : noitem;
    };
    template<class T>
    requires iter<decltype(get_value_t(std::declval<T>()))>
    struct optional_iter_wrapper<T> {
        using iter_t = decltype(get_value_t(std::declval<T>()));
        T optional_iter;
    };
    template<class T>
    optional_iter_wrapper(T) -> optional_iter_wrapper<T>;
}

#endif /* INCLUDE_ITER_ITER_WRAPPER_HPP */

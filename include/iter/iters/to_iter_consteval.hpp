#ifndef ITER_ITERS_TO_ITER_CONSTEVAL_HPP
#define ITER_ITERS_TO_ITER_CONSTEVAL_HPP

#include "iter/core/assert_consteval.hpp"
#include "iter/iters/to_iter.hpp"

namespace iter {
    // Owning iter that is expensive to use at runtime,
    // but is a necessary compile-time equivalent.
    // Therefore it is prevented from being used at runtime,
    // and will fail to compile at the linker stage.
    template<class T>
    struct to_iter_consteval;

    template<concepts::random_access_container T>
    struct to_iter_consteval<T> {
        using this_t = to_iter_consteval;

        T container;
        std::size_t pos = 0;

        constexpr auto ITER_IMPL_NEXT(this_t& self) {
            detail::assert_consteval<this_t, struct unused>();
            return self.pos < std::size(self.container)
                ? iter::item_ref(self.container[self.pos++])
                : iter::noitem;
        }

        constexpr auto ITER_IMPL_NEXT_BACK(this_t& self) {
            detail::assert_consteval<this_t, struct unused>();
            const auto size = std::size(self.container);
            return self.pos < size
                ? iter::item_ref(size - 1 - self.container[self.pos++])
                : iter::noitem;
        }

        constexpr decltype(auto) ITER_IMPL_GET(this_t& self, std::size_t index) {
            detail::assert_consteval<this_t, struct unused>();
            return self.container[index];
        }
        constexpr decltype(auto) ITER_IMPL_SIZE(this_t const& self) {
            detail::assert_consteval<this_t, struct unused>();
            return std::size(self.container);
        }
    };

    template<class T>
    to_iter_consteval(T) -> to_iter_consteval<T>;
}

#endif /* ITER_ITERS_TO_ITER_CONSTEVAL_HPP */

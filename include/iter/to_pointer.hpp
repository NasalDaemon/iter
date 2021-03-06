#ifndef INCLUDE_ITER_TO_POINTER_HPP
#define INCLUDE_ITER_TO_POINTER_HPP

#include "iter/core.hpp"

ITER_DECLARE(to_pointer_iter)

namespace iter::detail {
    template<concepts::optional_iter I>
    struct [[nodiscard]] to_pointer_iter {
        using this_t = to_pointer_iter;

        [[no_unique_address]] I i;
        next_t<I> store = std::nullopt;

        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
            emplace_next(self.store, self.i);
            return self.store ? std::addressof(*self.store) : nullptr;
        }
    };

    template<concepts::optional_iter I>
    requires concepts::random_access_iter<I>
    struct [[nodiscard]] to_pointer_iter<I> : enable_random_access<to_pointer_iter<I>, I> {
        using this_t = to_pointer_iter;

        // If iter::unsafe::get returns a value, then we need to store it to return a pointer to storage
        static constexpr bool get_val = !std::is_reference_v<unsafe::get_t<I>>;
        [[no_unique_address]] I i;
        [[no_unique_address]] std::conditional_t<get_val, next_t<I>, void_t> store;

        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t index) {
            if constexpr (get_val) {
                EMPLACE_NEW(self.store, MAKE_OPTIONAL(iter::unsafe::get(self.i, index)));
                return *self.store;
            } else
                return iter::unsafe::get(self.i, index);
        }
    };

    template<class I>
    to_pointer_iter(I) -> to_pointer_iter<I>;
}

template<iter::assert_iter I>
constexpr decltype(auto) ITER_IMPL(to_pointer_iter) (I&& iter) {
    if constexpr (iter::concepts::pointer_iter<I>) {
        return FWD(iter);
    } else {
        return iter::detail::to_pointer_iter<iter::iter_t<I>>{.i = FWD(iter)};
    }
}

#endif /* INCLUDE_ITER_TO_POINTER_HPP */

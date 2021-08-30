#ifndef INCLUDE_ITER_TO_POINTER_HPP
#define INCLUDE_ITER_TO_POINTER_HPP

#include "iter/core.hpp"

ITER_DECLARE(to_pointer_iter)

namespace iter::detail {
    template<concepts::optional_iter I>
    struct [[nodiscard]] to_pointer_iter {
        using this_t = to_pointer_iter;

        I i;
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

        template<class T> requires (!std::same_as<to_pointer_iter, std::remove_cvref_t<T>>)
        constexpr to_pointer_iter(T&& in) : this_t::base_t{FWD(in)}, store{} {}

    private:
        // If iter::unsafe::get returns a value, then we need to store it to return a pointer to storage
        static constexpr bool get_val = !std::is_reference_v<decltype(iter::unsafe::get(std::declval<I&>(), 0ul))>;
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

template<iter::iter I>
constexpr decltype(auto) ITER_IMPL(to_pointer_iter) (I&& iter) {
    if constexpr (iter::concepts::pointer_iter<I>) {
        return FWD(iter);
    } else {
        return iter::detail::to_pointer_iter{FWD(iter)};
    }
}

#endif /* INCLUDE_ITER_TO_POINTER_HPP */

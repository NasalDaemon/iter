#ifndef INCLUDE_ITER_OPTIONAL_HPP
#define INCLUDE_ITER_OPTIONAL_HPP

#include "iter/core.hpp"

namespace iter {
    template<class T>
    struct optional : item<T> {
        using this_t = optional;

        constexpr optional(auto&&... args) : item<T>(FWD(args)...) {}

        constexpr std::size_t ITER_IMPL_SIZE (this_t const& self) {
            return self ? 1 : 0;
        }
        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t) {
            return *self;
        }
        constexpr item<T> ITER_IMPL_NEXT (this_t& self) {
            item<T> result(std::move(self));
            self.reset();
            return result;
        }
        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
            return detail::impl::next(self);
        }
    };

    template<class T>
    optional(T) -> optional<T>;

    namespace concepts {
        template<class T>
        static constexpr bool is_iter_of_optional = false;
        template<class T>
        constexpr bool is_iter_of_optional<iter::optional<T>> = true;
        template<class T>
        concept iter_of_optional = is_iter_of_optional<T>;
    }
}

#endif /* INCLUDE_ITER_OPTIONAL_HPP */

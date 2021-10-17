#ifndef INCLUDE_ITER_RANGE_HPP
#define INCLUDE_ITER_RANGE_HPP

#include "iter/core.hpp"

ITER_DECLARE(until)
ITER_ALIAS(til, until)

namespace iter {
    template<std::integral T = int, bool Inclusive = false>
    struct [[nodiscard]] range {
        using this_t = range;
        T begin_;
        T end_ = std::numeric_limits<T>::max() - Inclusive;
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            if constexpr (Inclusive)
                return self.begin_ <= self.end_ ? std::optional(self.begin_++) : std::nullopt;
            else
                return self.begin_ < self.end_ ? std::optional(self.begin_++) : std::nullopt;
        }
        constexpr std::size_t ITER_IMPL_SIZE (this_t const& self) {
            if constexpr (Inclusive)
                return 1ul + self.end_ - self.begin_;
            else
                return self.end_ - self.begin_;
        }
        constexpr T ITER_IMPL_GET (this_t& self, std::size_t index) {
            return self.begin_ + index;
        }
        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
            if constexpr (Inclusive)
                return self.begin_ <= self.end_ ? std::optional(self.end_--) : std::nullopt;
            else
                return self.begin_ < self.end_ ? std::optional(--self.end_) : std::nullopt;
        }
    };

    template<class T>
    range(T) -> range<T>;
    template<class T>
    range(T, T) -> range<T>;

    template<class T>
    struct inclusive_range : range<T, true> {};

    template<class T>
    inclusive_range(T) -> inclusive_range<T>;
    template<class T>
    inclusive_range(T, T) -> inclusive_range<T>;

    namespace detail {
        struct [[nodiscard]] indices_iter {
            using this_t = indices_iter;
            indices_iter() = default;
        private:
            std::size_t i = 0;
            constexpr auto ITER_IMPL_NEXT (this_t& self) {
                return std::optional(self.i++);
            }
            constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
                return std::optional(std::numeric_limits<std::size_t>::max() - self.i++);
            }
            constexpr std::size_t ITER_IMPL_SIZE (this_t const&) {
                return std::numeric_limits<std::size_t>::max();
            }
            constexpr std::size_t ITER_IMPL_GET (this_t&, std::size_t index) {
                return index;
            }
        };
    }

    static constexpr struct {} indices;
    constexpr auto ITER_IMPL(to_iter) (decltype(indices)) {
        return detail::indices_iter{};
    }
}

template<std::integral T>
constexpr auto ITER_IMPL(until) (T begin, T end) {
    return iter::range{begin, end};
}

#endif /* INCLUDE_ITER_RANGE_HPP */

#ifndef INCLUDE_ITER_RANGE_HPP
#define INCLUDE_ITER_RANGE_HPP

#include "iter/core/core.hpp"

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
                return self.begin_ <= self.end_ ? item(self.begin_++) : noitem;
            else
                return self.begin_ < self.end_ ? item(self.begin_++) : noitem;
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
                return self.begin_ <= self.end_ ? item(self.end_--) : noitem;
            else
                return self.begin_ < self.end_ ? item(--self.end_) : noitem;
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
        template<std::integral T = std::size_t>
        struct [[nodiscard]] indices_iter {
            using this_t = indices_iter;
            indices_iter() = default;
        private:
            T i = 0;
            constexpr auto ITER_IMPL_NEXT (this_t& self) {
                return item{self.i++};
            }
            constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
                return item{std::numeric_limits<T>::max() - self.i++};
            }
            constexpr std::size_t ITER_IMPL_SIZE (this_t const&) {
                return std::numeric_limits<T>::max();
            }
            constexpr T ITER_IMPL_GET (this_t&, std::size_t index) {
                return index;
            }
        };

        template<std::integral T = std::size_t>
        struct indices_tag {};
        template<class T>
        constexpr auto ITER_IMPL(to_iter) (detail::indices_tag<T>) {
            return indices_iter<T>{};
        }
    }

    template<class T = std::size_t>
    static constexpr detail::indices_tag<T> indices_ = {};
    static constexpr auto indices = indices_<>;
}

template<std::integral T>
constexpr auto ITER_IMPL(until) (T begin, T end) {
    return iter::range{begin, end};
}

#endif /* INCLUDE_ITER_RANGE_HPP */

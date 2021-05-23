#ifndef INCLUDE_ITER_RANGE_HPP
#define INCLUDE_ITER_RANGE_HPP

#include "iter/core.hpp"

ITER_DECLARE(until)
ITER_ALIAS(til, until)

namespace iter {
    template<std::integral T = int>
    struct [[nodiscard]] range {
        using this_t = range;
        constexpr range(T begin = 0, T end = std::numeric_limits<T>::max()) : begin_{begin}, end_{end} {}
    private:
        T begin_;
        T end_;
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
            return self.begin_ < self.end_ ? std::optional(self.begin_++) : std::nullopt;
        }
        constexpr std::size_t ITER_UNSAFE_SIZE (this_t const& self) {
            return self.end_ - self.begin_;
        }
        constexpr T ITER_UNSAFE_GET (this_t& self, std::size_t index) {
            return self.begin_ + index;
        }
    };

    template<class T>
    range(T) -> range<T>;

    namespace detail {
        struct [[nodiscard]] indices_iter {
            using this_t = indices_iter;
            indices_iter() = default;
        private:
            int i = 0;
            constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
                return std::optional(self.i++);
            }
            constexpr std::size_t ITER_UNSAFE_SIZE (this_t const&) {
                return std::numeric_limits<int>::max();
            }
            constexpr int ITER_UNSAFE_GET (this_t&, std::size_t index) {
                return (int)index;
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

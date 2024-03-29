#ifndef ITER_COLLECTORS_TO_INPUT_RANGE_HPP
#define ITER_COLLECTORS_TO_INPUT_RANGE_HPP

#include "iter/core/core.hpp"

ITER_DECLARE(to_input_range)

namespace iter::detail {
    template<iter I>
    struct input_range {
        constexpr explicit input_range(auto&& it)
            : i{FWD(it)}
            , current{impl::next(i)}
        {}

        struct iterator : iterator_traits<I> {
            explicit iterator(input_range* outer)
                : outer{outer}
            {}

            iterator() = default;

            auto operator<=>(const iterator&) const = delete;
            constexpr bool operator==(const iterator& other) const { return outer == other.outer; }

            constexpr bool operator!=(sentinel_t) const { return outer && outer->current.has_value(); }
            constexpr bool operator==(sentinel_t) const { return !operator!=(sentinel); }
            constexpr auto& operator*() const { return *outer->current; }
            constexpr auto* operator->() const { return std::addressof(*outer->current); }
            constexpr auto& operator++() {
                if (outer)
                    emplace_next(outer->current, outer->i);
                return *this;
            }
            constexpr void operator++(int) {
                if (outer)
                    emplace_next(outer->current, outer->i);
            };

        private:
            input_range* outer;
        };

        constexpr auto begin() { return iterator(this); }
        constexpr auto end() const { return sentinel; }

    private:
        [[no_unique_address]] I i;
        next_t<I> current;
    };

    template<class I>
    input_range(I) -> input_range<I>;
}

template<iter::iter I>
constexpr auto ITER_IMPL(to_input_range)(I&& i) {
    return iter::detail::input_range{FWD(i)};
}

#endif /* ITER_COLLECTORS_TO_INPUT_RANGE_HPP */

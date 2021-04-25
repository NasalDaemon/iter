#ifndef INCLUDE_ITER_FLATMAP_HPP
#define INCLUDE_ITER_FLATMAP_HPP

#include "iter/core.hpp"

ITER_DECLARE(flatmap)
ITER_ALIAS(flatmap, flat_map)

namespace iter::detail {
    template<iter I, std::invocable<consume_t<I>> F>
    struct [[nodiscard]] flatmap_iter {
        template<class T, class U>
        constexpr flatmap_iter(T&& i, U&& f) : i{(T&&)i}, func{(U&&)f} {}

    private:
        using this_t = flatmap_iter;
        using invoke_result = std::invoke_result_t<F, consume_t<I>>;
        static_assert(iterable<invoke_result>);
        using inner_iter_t = iter_t<invoke_result>;

        I i;
        [[no_unique_address]] F func;
        std::optional<inner_iter_t> current;

        constexpr auto get_current() {
            auto next = iter::next(i);
            if constexpr (iter<invoke_result>) {
                return next
                    ? MAKE_OPTIONAL(func(consume(next)))
                    : std::nullopt;
            } else {
                return next
                    ? MAKE_OPTIONAL(iter::to_iter(func(consume(next))))
                    : std::nullopt;
            }
        }

        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
            auto val = no_next<inner_iter_t>();
            do {
                if (self.current)
                    if (emplace_next(val, *self.current))
                        return val;
            } while (EMPLACE_NEW(self.current, self.get_current()));
            return val;
        }
    };

    template<class I, class F>
    flatmap_iter(I, F) -> flatmap_iter<I, F>;
}

template<iter::iterable I, std::invocable<iter::consume_t<I>> F>
constexpr auto ITER_IMPL(flatmap) (I&& iterable, F&& func) {
    return iter::detail::flatmap_iter{iter::to_iter((I&&) iterable), (F&&)func};
}

#include "iter/filter_map.hpp"

// flatmap on std::optional is equivalent to the specially optimised filter_map
template<iter::iterable I, std::invocable<iter::consume_t<I>> F>
requires iter::concepts::optional_next<std::invoke_result_t<F, iter::consume_t<I>>>
      || iter::concepts::pointer_next<std::invoke_result_t<F, iter::consume_t<I>>>
constexpr auto ITER_IMPL(flatmap) (I&& iterable, F&& func) {
    return iter::filter_map((I&&) iterable, (F&&) func);
}

#endif /* INCLUDE_ITER_FLATMAP_HPP */

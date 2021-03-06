#ifndef INCLUDE_ITER_FLATMAP_HPP
#define INCLUDE_ITER_FLATMAP_HPP

#include "iter/core.hpp"

ITER_DECLARE(flatmap)
ITER_ALIAS(flat_map, flatmap)

namespace iter::detail {
    template<assert_iter I, std::invocable<consume_t<I>> F>
    struct [[nodiscard]] flatmap_iter {
        using this_t = flatmap_iter;
        using invoke_result = std::invoke_result_t<F, consume_t<I>>;
        static_assert(iterable<invoke_result>);
        using inner_iter_t = iter_t<invoke_result>;

        std::optional<inner_iter_t> current = std::nullopt;
        [[no_unique_address]] I i;
        [[no_unique_address]] F func;

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

template<iter::assert_iterable I, std::invocable<iter::consume_t<I>> F>
constexpr auto ITER_IMPL(flatmap) (I&& iterable, F&& func) {
    return iter::detail::flatmap_iter<iter::iter_t<I>, std::remove_cvref_t<F>>{.i = iter::to_iter(FWD(iterable)), .func = FWD(func)};
}

#include "iter/filter_map.hpp"

// flatmap on std::optional is equivalent to the specially optimised filter_map
template<iter::assert_iterable I, std::invocable<iter::consume_t<I>> F>
requires iter::concepts::optional_next<std::invoke_result_t<F, iter::consume_t<I>>>
      || iter::concepts::pointer_next<std::invoke_result_t<F, iter::consume_t<I>>>
constexpr auto ITER_IMPL(flatmap) (I&& iterable, F&& func) {
    return iter::filter_map(FWD(iterable), FWD(func));
}

#endif /* INCLUDE_ITER_FLATMAP_HPP */

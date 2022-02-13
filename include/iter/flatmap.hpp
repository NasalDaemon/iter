#ifndef INCLUDE_ITER_FLATMAP_HPP
#define INCLUDE_ITER_FLATMAP_HPP

#include "iter/core.hpp"
#include "iter/iter_wrapper.hpp"

ITER_DECLARE(flatmap)
ITER_ALIAS(flat_map, flatmap)

namespace iter::detail {
    template<assert_iter I, std::invocable<consume_t<I>> F>
    struct [[nodiscard]] flatmap_iter {
        using this_t = flatmap_iter;
        using invoke_result = std::invoke_result_t<F, consume_t<I>>;
        static_assert(!concepts::iter_of_optional<invoke_result>,
            "Do not return iter::optional in iter::flatmap, instead return std::optional in iter::filter_map.");
        using wrapped_inner_iter_t = iter_wrapper<invoke_result>;
        using inner_iter_t = typename wrapped_inner_iter_t::iter_t;

        item<wrapped_inner_iter_t> current{};
        [[no_unique_address]] I i;
        [[no_unique_address]] F func;

        constexpr auto get_current() {
            auto next = impl::next(i);
            return next
                ? MAKE_ITEM(iter_wrapper{func(consume(next))})
                : noitem;
        }

        constexpr next_t<inner_iter_t> ITER_IMPL_NEXT (this_t& self) {
            auto val = no_next<inner_iter_t>();
            do {
                if (self.current) [[likely]]
                    if (emplace_next(val, self.current->iter)) [[likely]]
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

// flatmap on iter::item is equivalent to the specially optimised filter_map
template<iter::assert_iterable I, std::invocable<iter::consume_t<I>> F>
requires iter::concepts::item<std::invoke_result_t<F, iter::consume_t<I>>>
constexpr auto ITER_IMPL(flatmap) (I&& iterable, F&& func) {
    return iter::filter_map(FWD(iterable), FWD(func));
}

#endif /* INCLUDE_ITER_FLATMAP_HPP */

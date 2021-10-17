#ifndef INCLUDE_ITER_FLATTEN_HPP
#define INCLUDE_ITER_FLATTEN_HPP

#include "iter/core.hpp"

ITER_DECLARE(flatten)

namespace iter::detail {
    template<assert_iter I>
    struct [[nodiscard]] flatten_iter {
        using this_t = flatten_iter;
        using inner_t = value_t<I>;
        static_assert(iterable<consume_t<I>>);

        constexpr static auto get_current(I& i) {
            if constexpr (iter<inner_t>)
                return impl::next(i);
            else {
                auto val = impl::next(i);
                return val ? MAKE_OPTIONAL(iter::to_iter(consume(val))) : std::nullopt;
            }
        }

        [[no_unique_address]] I i;
        decltype(this_t::get_current(std::declval<I&>())) current{};

        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            auto val = no_next<iter_t<inner_t>>();
            do {
                if (self.current)
                    if (emplace_next(val, *self.current))
                        return val;
            } while (EMPLACE_NEW(self.current, get_current(self.i)));
            return val;
        }
    };

    template<class I>
    flatten_iter(I) -> flatten_iter<I>;
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(flatten) (I&& iterable) {
    return iter::detail::flatten_iter<iter::iter_t<I>>{.i = iter::to_iter(FWD(iterable))};
}

#endif /* INCLUDE_ITER_FLATTEN_HPP */

#ifndef INCLUDE_ITER_FLATTEN_HPP
#define INCLUDE_ITER_FLATTEN_HPP

#include "iter/core.hpp"
#include "iter/iter_wrapper.hpp"

ITER_DECLARE(flatten)

namespace iter::detail {
    template<assert_iter I>
    struct [[nodiscard]] flatten_iter {
        using this_t = flatten_iter;

        constexpr static auto get_current(I& i) {
            return optional_iter_wrapper{impl::next(i)};
        }

        [[no_unique_address]] I i;
        decltype(this_t::get_current(std::declval<I&>())) current{};

        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            using inner_iter_t = typename decltype(current)::iter_t;
            auto val = no_next<inner_iter_t>();
            do {
                if (self.current.optional_iter) [[likely]]
                    if (emplace_next(val, *self.current.optional_iter)) [[likely]]
                        return val;
            } while (EMPLACE_NEW(self.current, get_current(self.i)).optional_iter);
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

#ifndef INCLUDE_ITER_FLATTEN_HPP
#define INCLUDE_ITER_FLATTEN_HPP

#include "iter/core.hpp"

ITER_DECLARE(flatten)

namespace iter::detail {
    template<iter I>
    struct [[nodiscard]] flatten_iter {
        template<iter T>
        requires (!std::same_as<std::remove_cvref_t<T>, flatten_iter>)
        constexpr explicit flatten_iter(T&& i_) : i{FWD(i_)}, current{} {}

    private:
        using this_t = flatten_iter;
        using inner_t = value_t<I>;
        static_assert(iterable<consume_t<I>>);

        constexpr static auto get_current(I& i) {
            if constexpr(iter<inner_t>)
                return iter::next(i);
            else {
                auto val = iter::next(i);
                return val ? MAKE_OPTIONAL(iter::to_iter(consume(val))) : std::nullopt;
            }
        }

        I i;
        decltype(this_t::get_current(std::declval<I&>())) current;

        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
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

template<iter::iterable I>
constexpr auto ITER_IMPL(flatten) (I&& iterable) {
    return iter::detail::flatten_iter{iter::to_iter(FWD(iterable))};
}

#endif /* INCLUDE_ITER_FLATTEN_HPP */

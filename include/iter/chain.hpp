#ifndef INCLUDE_ITER_CHAIN_HPP
#define INCLUDE_ITER_CHAIN_HPP

#include "iter/core.hpp"

ITER_DECLARE(chain)

namespace iter::detail {
    template<assert_iter I1, assert_iter I2>
    struct [[nodiscard]] chain_iter : enable_random_access<chain_iter<I1, I2>, I1, I2> {
        static_assert(std::same_as<value_t<I1>, value_t<I2>>);

        std::optional<I1> i1;
        [[no_unique_address]] I2 i2;

    private:
        using this_t = chain_iter;

        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            std::size_t i1s = impl::size(*self.i1);
            return index < i1s ? impl::get(*self.i1, index) : impl::get(self.i2, index - i1s);
        }

        static constexpr bool optional_next = concepts::optional_iter<I1> || concepts::optional_iter<I2>;
        using next_t = std::conditional_t<optional_next, std::optional<value_t<I1>>, value_t<I1>*>;

        constexpr next_t ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            auto val = next_t{};
            if (self.i1) {
                if constexpr (optional_next && !concepts::optional_iter<I1>) {
                    if (auto pval = impl::next(*self.i1)) {
                        val.emplace(*pval);
                        return val;
                    }
                } else {
                    if (emplace_next(val, *self.i1))
                        return val;
                }
                // If we haven't returned by this point, we reached the end of I1
                self.i1.reset();
            }
            if constexpr (optional_next && !concepts::optional_iter<I2>) {
                if (auto pval = impl::next(self.i2))
                    val.emplace(*pval);
            } else {
                emplace_next(val, self.i2);
            }
            return val;
        }
    };

    template<class I1, class I2>
    chain_iter(std::optional<I1>, I2) -> chain_iter<I1, I2>;
}

template<iter::assert_iterable I1, iter::assert_iterable I2>
constexpr auto ITER_IMPL(chain) (I1&& iterable1, I2&& iterable2) {
    using chain_t = iter::detail::chain_iter<iter::iter_t<I1>, iter::iter_t<I2>>;
    if constexpr (chain_t::random_access) {
        auto chain = chain_t{.i1 = MAKE_OPTIONAL(iter::to_iter(FWD(iterable1))), .i2 = iter::to_iter(FWD(iterable2))};
        chain.size = iter::detail::impl::size(*chain.i1) + iter::detail::impl::size(chain.i2);
        return chain;
    } else {
        return chain_t{.i1 = MAKE_OPTIONAL(iter::to_iter(FWD(iterable1))), .i2 = iter::to_iter(FWD(iterable2))};
    }
}

#endif /* INCLUDE_ITER_CHAIN_HPP */

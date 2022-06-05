#ifndef ITER_ADAPTERS_CHAIN_HPP
#define ITER_ADAPTERS_CHAIN_HPP

#include "iter/core/core.hpp"

ITER_DECLARE(chain)

namespace iter::detail {
    template<assert_iter I1, assert_iter I2>
    struct [[nodiscard]] chain_iter : enable_random_access<chain_iter<I1, I2>, I1, I2> {
        static_assert(std::same_as<value_t<I1>, value_t<I2>>);

        item<I1> i1;
        [[no_unique_address]] I2 i2;

    private:
        using this_t = chain_iter;

        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            using stability = common_stability<detail::get_t<I1>, detail::get_t<I2>>;
            std::size_t i1s = impl::size(*self.i1);
            return index < i1s
                ? stability{impl::get(*self.i1, index)}
                : stability{impl::get(self.i2, index - i1s)};
        }

        static constexpr bool owned_next = concepts::owned_item<next_t<I1>> || concepts::owned_item<next_t<I2>>;
        static constexpr bool stable = concepts::stable_iter<I1> && concepts::stable_iter<I2>;
        using item_t = std::conditional_t<owned_next, item<value_t<I1>, stable>, item<value_t<I1>&, stable>>;

        constexpr item_t ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            item_t item;
            if (self.i1) {
                if constexpr (owned_next && !concepts::owned_item<next_t<I1>>) {
                    if (auto val = impl::next(self.i1.value())) {
                        item.emplace(val.value());
                        return item;
                    }
                } else {
                    if (emplace_next(item, self.i1.value()))
                        return item;
                }
                // If we haven't returned by this point, we reached the end of I1
                self.i1.reset();
            }
            if constexpr (owned_next && !concepts::owned_item<next_t<I2>>) {
                if (auto val = impl::next(self.i2))
                    item.emplace(val.value());
            } else {
                emplace_next(item, self.i2);
            }
            return item;
        }
    };

    template<class I1, class I2>
    chain_iter(item<I1>, I2) -> chain_iter<I1, I2>;
}

template<iter::assert_iterable I1, iter::assert_iterable I2>
constexpr auto ITER_IMPL(chain) (I1&& iterable1, I2&& iterable2) {
    using chain_t = iter::detail::chain_iter<iter::iter_t<I1>, iter::iter_t<I2>>;
    if constexpr (chain_t::random_access) {
        auto chain = chain_t{.i1 = MAKE_ITEM(iter::to_iter(FWD(iterable1))), .i2 = iter::to_iter(FWD(iterable2))};
        chain.size = iter::traits::random_access::size(*chain.i1) + iter::traits::random_access::size(chain.i2);
        return chain;
    } else {
        return chain_t{.i1 = MAKE_ITEM(iter::to_iter(FWD(iterable1))), .i2 = iter::to_iter(FWD(iterable2))};
    }
}

#endif /* ITER_ADAPTERS_CHAIN_HPP */

#ifndef ITER_ADAPTERS_CONSTEVAL_ONLY_HPP
#define ITER_ADAPTERS_CONSTEVAL_ONLY_HPP

#include "iter/core/core.hpp"
#include "iter/core/assert_consteval.hpp"

ITER_DECLARE(consteval_only)

namespace iter::detail {
    template<iter I>
    struct [[nodiscard]] consteval_only_iter {
        using this_t = consteval_only_iter;
        [[no_unique_address]] I i;

        constexpr decltype(auto) ITER_IMPL_NEXT(this_t& self) {
            assert_consteval<this_t, struct next>();
            return impl::next(self.i);
        }

        constexpr decltype(auto) ITER_IMPL_NEXT_BACK(this_t& self)
            requires concepts::double_ended_iter<I>
        {
            assert_consteval<this_t, struct next_back>();
            return impl::next_back(self.i);
        }

        constexpr decltype(auto) ITER_IMPL_GET(this_t& self, std::size_t index)
            requires concepts::random_access_iter<I>
        {
            assert_consteval<this_t, struct get>();
            return impl::get(self.i, index);
        }

        constexpr decltype(auto) ITER_IMPL_SIZE(this_t const& self)
            requires concepts::random_access_iter<I>
        {
            assert_consteval<this_t, struct size>();
            return impl::size(self.i);
        }
    };

    template<class I>
    consteval_only_iter(I) -> consteval_only_iter<I>;
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(consteval_only) (I&& iterable) {
    return iter::detail::consteval_only_iter{iter::to_iter(FWD(iterable))};
}

#endif /* ITER_ADAPTERS_CONSTEVAL_ONLY_HPP */

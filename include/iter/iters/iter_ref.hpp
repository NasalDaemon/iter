#ifndef ITER_ITERS_ITER_REF_HPP
#define ITER_ITERS_ITER_REF_HPP

#include "iter/core/core.hpp"

namespace iter::detail {

// Use this to avoid copying when an iter returns an item referring to its own instance
template<iter I>
struct iter_ref {
    using this_t = iter_ref;
    constexpr explicit iter_ref(I& i) : i{i} {}

    constexpr auto ITER_IMPL_NEXT(this_t& self) {
        return impl::next(self.i);
    }

    constexpr auto ITER_IMPL_NEXT_BACK(this_t& self)
        requires concepts::double_ended_iter<I>
    {
        return impl::next_back(self.i);
    }

    constexpr std::size_t ITER_IMPL_SIZE(this_t const& self)
        requires concepts::random_access_iter<I>
    {
        return impl::size(self.i);
    }

    constexpr decltype(auto) ITER_IMPL_GET(this_t& self, std::size_t n)
        requires concepts::random_access_iter<I>
    {
        return impl::get(self.i, n);
    }

private:
    I& i;
};

template<class I>
iter_ref(I&) -> iter_ref<I>;

}

#endif /* ITER_ITERS_ITER_REF_HPP */

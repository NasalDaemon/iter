#ifndef ITER_ITERS_ITER_REF_HPP
#define ITER_ITERS_ITER_REF_HPP

#include "iter/core/core.hpp"

namespace iter::detail {

// Use this to avoid copying when an iter returns an item referring to its own instance
template<iter I>
struct iter_ref {
    using this_t = iter_ref;
    constexpr explicit iter_ref(I& i) : i{std::addressof(i)} {}

    constexpr auto ITER_IMPL_NEXT(this_t& self) {
        return impl::next(*self.i);
    }

private:
    I* i;
};

template<class I>
iter_ref(I&) -> iter_ref<I>;

}

#endif /* ITER_ITERS_ITER_REF_HPP */

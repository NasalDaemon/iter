#ifndef INCLUDE_ITER_PARTITION_HPP
#define INCLUDE_ITER_PARTITION_HPP

#include "iter/core.hpp"

XTD_INVOKER(iter_partition)

namespace iter {
    namespace tag {
        template<std::size_t N = 2>
        struct partition_ : xtd::tagged_bindable<partition_<N>, xtd::invokers::iter_partition> {};
    }

    template<std::size_t N = 2>
    static constexpr tag::partition_<N> partition_;

    template<std::size_t I>
    struct index_t : index_t<I+1> {
        template<std::size_t J>
        requires (J < I)
        constexpr index_t(index_t<J> j) : index_t<I+1>{j} {}
        constexpr index_t() : index_t<I+1>{I} {}
    protected:
        constexpr index_t(size_t i) : index_t<I+1>{i} {}
    };

    template<>
    struct index_t<12> {
        constexpr std::size_t value() const { return index; }
    protected:
        constexpr index_t(size_t i) : index{i} {}
        std::size_t const index;
    };

    template<std::size_t I>
    static constexpr auto index = index_t<I>{};

    template<std::size_t I>
    struct maximum {
        static constexpr auto values = []<std::size_t... Is>(std::index_sequence<Is...>) {
            return std::array<index_t<I>, I+1>{index_t<Is>{}...};
        }(std::make_index_sequence<I+1>{});
    };
}

ITER_ALIAS(partition, partition_<>)

template<size_t N, iter::assert_iterable I, class F>
constexpr decltype(auto) XTD_IMPL_TAG_(iter_partition, iter::tag::partition_<N>) (I&& iterable, F&& func) {
    return iter::partition_<N>(iter::to_iter(FWD(iterable)), FWD(func));
}

template<size_t N, iter::iter I, class F>
requires (N > 1)
constexpr decltype(auto) XTD_IMPL_TAG_(iter_partition, iter::tag::partition_<N>) (I&& iter, F&& func) {
    auto out = std::array<std::vector<iter::value_t<std::decay_t<I>>>, N>{};

    if constexpr (iter::concepts::random_access_iter<I>) {
        std::size_t size = iter::unsafe::size(iter) / N;
        apply([=](auto&&... outs) { (outs.reserve(size), ...); }, out);
    }

    while (auto val = iter::next(iter)) {
        auto slot = std::invoke(FWD(func), iter::as_const(*val));
        std::size_t index;
        if constexpr (std::is_same_v<bool, decltype(slot)>) {
            static_assert(N == 2, "Boolean predicate function only permitted with iter::partition<2>.");
            index = slot ? 0 : 1;
        } else {
            static_assert(std::is_same_v<iter::index_t<N-1>, decltype(slot)>,
                "Function called in iter::partition<N> must return iter::index_t<N-1>.");
            index = slot.value();
        }

        out[index].emplace_back(iter::detail::consume(val));
    }
    return out;
}

#endif /* INCLUDE_ITER_PARTITION_HPP */

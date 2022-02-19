#ifndef INCLUDE_ITER_PARTITION_HPP
#define INCLUDE_ITER_PARTITION_HPP

#include "iter/core.hpp"

ITER_DECLARE(partition)

namespace iter {
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
    struct index_t<6> {
        constexpr std::size_t value() const { return index; }
    protected:
        constexpr index_t(size_t i) : index{i} {}
        std::size_t const index;
    };

    template<std::size_t I>
    static constexpr auto index = index_t<I>{};

    namespace concepts {
        template<class T>
        static constexpr bool is_partition_index = false;
        template<std::size_t I>
        constexpr bool is_partition_index<index_t<I>> = true;

        template<class T>
        concept partition_index = is_partition_index<T>;
    }

    template<std::size_t I>
    struct maximum {
        static constexpr auto values = []<std::size_t... Is>(std::index_sequence<Is...>) {
            return std::array<index_t<I>, I+1>{index_t<Is>{}...};
        }(std::make_index_sequence<I+1>{});
    };
}

template<iter::assert_iterable I, class F>
constexpr decltype(auto) ITER_IMPL(partition) (I&& iterable, F&& func) {
    return iter::partition(iter::to_iter(FWD(iterable)), FWD(func));
}

template<iter::iter I, class F>
constexpr decltype(auto) ITER_IMPL(partition) (I&& iter, F&& func) {
    using index_t = std::invoke_result_t<F, iter::consume_t<I>>;
    constexpr std::size_t N = []{
        if constexpr (std::is_same_v<bool, index_t>)
            return 2;
        else {
            static_assert(iter::concepts::partition_index<index_t>);
            return 1 + index_t{}.value();
        }
    }();
    static_assert(N > 1, "Must partition at least 2 ways");
    auto out = std::array<std::vector<iter::value_t<std::decay_t<I>>>, N>{};

    if constexpr (iter::concepts::random_access_iter<I>) {
        std::size_t size = iter::detail::impl::size(iter) / N;
        apply([=](auto&&... outs) { (outs.reserve(size), ...); }, out);
    }

    while (auto val = iter::detail::impl::next(iter)) {
        auto slot = std::invoke(FWD(func), iter::as_const(*val));
        std::size_t index;
        if constexpr (std::is_same_v<bool, index_t>) {
            index = slot ? 0 : 1;
        } else {
            index = slot.value();
        }

        out[index].emplace_back(iter::detail::consume(val));
    }
    return out;
}

#endif /* INCLUDE_ITER_PARTITION_HPP */

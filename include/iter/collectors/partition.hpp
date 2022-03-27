#ifndef ITER_COLLECTORS_PARTITION_HPP
#define ITER_COLLECTORS_PARTITION_HPP

#include "iter/core/core.hpp"

ITER_DECLARE(partition)

namespace iter {
    template<std::size_t I>
    struct part_t : part_t<I+1> {
        constexpr part_t() : part_t<I+1>{I} {}
    protected:
        constexpr part_t(size_t i) : part_t<I+1>{i} {}
    };

    template<>
    struct part_t<6> {
        constexpr std::size_t value() const { return index; }
    protected:
        constexpr part_t(size_t i) : index{i} {}
        std::size_t const index;
    };

    template<std::size_t I>
    inline constexpr auto part = part_t<I>{};

    namespace concepts {
        template<class T>
        inline constexpr bool is_partition_index = false;
        template<std::size_t I>
        inline constexpr bool is_partition_index<part_t<I>> = true;

        template<class T>
        concept partition_index = is_partition_index<T>;
    }

    template<std::size_t I>
    static constexpr auto parts = []<std::size_t... Is>(std::index_sequence<Is...>) {
        return std::array<part_t<I>, I+1>{part_t<Is>{}...};
    }(std::make_index_sequence<I+1>{});
}

template<iter::assert_iterable I, class F>
constexpr decltype(auto) ITER_IMPL(partition) (I&& iterable, F&& func) {
    using part_t = std::invoke_result_t<F, iter::consume_t<I>>;
    constexpr std::size_t N = []{
        if constexpr (std::is_same_v<bool, part_t>)
            return 2;
        else {
            static_assert(iter::concepts::partition_index<part_t>);
            return 1 + part_t{}.value();
        }
    }();
    static_assert(N > 1, "Must partition at least 2 ways");
    auto out = std::array<std::vector<iter::value_t<I>>, N>{};

    decltype(auto) iter = iter::to_iter(FWD(iterable));

    if constexpr (iter::concepts::random_access_iter<decltype(iter)>) {
        std::size_t size = iter::traits::random_access::size(iter) / N;
        apply([=](auto&&... outs) { (outs.reserve(size), ...); }, out);
    }

    while (auto val = iter::traits::next(iter)) {
        auto slot = std::invoke(FWD(func), iter::as_const(*val));
        std::size_t index;
        if constexpr (std::is_same_v<bool, part_t>) {
            index = slot ? 0 : 1;
        } else {
            index = slot.value();
        }

        out[index].emplace_back(iter::detail::consume(val));
    }
    return out;
}

#endif /* ITER_COLLECTORS_PARTITION_HPP */

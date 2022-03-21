#ifndef INCLUDE_ITER_WINDOW_HPP
#define INCLUDE_ITER_WINDOW_HPP

#include "iter/adapters/cycle.hpp"
#include "iter/adapters/map.hpp"
#include "iter/adapters/skip.hpp"
#include "iter/adapters/take.hpp"

XTD_INVOKER(iter_window)

namespace iter {
    namespace detail::tag {
        template<std::size_t N>
        struct window : xtd::tagged_bindable<window<N>, xtd::invokers::iter_window> {};
    }
    template<std::size_t N = 2>
    static constexpr detail::tag::window<N> window;
}

namespace iter::detail {
    template<concepts::stable_item T, std::size_t N>
    struct window_iter_storage {
        std::array<T, N> buffer = {};
        std::size_t size = 0;
        std::size_t end = 0;
        constexpr auto to_iter() {
            using namespace xtd::literals;
            return cycle(buffer)
                | map(_, [](auto& item) { return stable_ref(*item); })
                | skip(_, end)
                | take(_, size--);
        }
    };

    template<assert_iter I, std::size_t N>
    struct [[nodiscard]] window_iter : window_iter_storage<next_t<I>, N> {
        static_assert(N > 1, "Window must be of at least size 2");
        [[no_unique_address]] I i;

        using this_t = window_iter;
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            while (self.size < N) [[likely]] {
                if (emplace_next(self.buffer[self.end], self.i)) [[likely]] {
                    ++self.size;
                    self.end = (self.end + 1) % N;
                } else break;
            }
            return self.size == N ? MAKE_ITEM(unstable{self.to_iter()}) : noitem;
        }
    };
}

template<std::size_t N, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_window, iter::detail::tag::window<N>) (I&& iterable) {
    return iter::detail::window_iter<std::remove_reference_t<I>, N>{{}, {FWD(iterable)}};
}

#endif /* INCLUDE_ITER_WINDOW_HPP */

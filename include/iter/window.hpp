#ifndef INCLUDE_ITER_WINDOW_HPP
#define INCLUDE_ITER_WINDOW_HPP

#include "iter/cycle.hpp"
#include "iter/skip.hpp"
#include "iter/take.hpp"

XTD_INVOKER(iter_window)

namespace iter {
    namespace tag {
        template<std::size_t N>
        struct window : xtd::tagged_bindable<window<N>, xtd::invokers::iter_window> {};
    }
    template<std::size_t N = 2>
    static constexpr tag::window<N> window;
}

namespace iter::detail {
    template<class T, std::size_t N>
    struct window_iter_storage {
        std::array<T, N> buffer = {};
        std::size_t size = 0;
        std::size_t end = 0;
        constexpr auto to_iter() {
            using namespace xtd::literals;
            return cycle(buffer) | skip(_, end) | take(_, size--);
        }
    };

    template<iter I, std::size_t N>
    struct [[nodiscard]] window_iter : window_iter_storage<value_t<I>, N> {
        static_assert(N > 1, "Window must be of at least size 2");
        I i;

        using this_t = window_iter;
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
            while (self.size < N) [[likely]] {
                if (auto next = iter::next(self.i)) [[likely]] {
                    self.buffer[self.end] = consume(next);
                    ++self.size;
                    self.end = (self.end + 1) % N;
                } else break;
            }
            return self.size == N ? MAKE_OPTIONAL(self.to_iter()) : std::nullopt;
        }
    };
}

template<std::size_t N, iter::iter I>
constexpr auto XTD_IMPL_TAG_(iter_window, iter::tag::window<N>) (I&& iterable) {
    return iter::detail::window_iter<std::remove_reference_t<I>, N>{{}, {(I&&)iterable}};
}

#endif /* INCLUDE_ITER_WINDOW_HPP */

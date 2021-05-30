#ifndef INCLUDE_ITER_CHUNKS_HPP
#define INCLUDE_ITER_CHUNKS_HPP

#include "iter/take.hpp"

XTD_INVOKER(iter_chunks)

namespace iter {
    namespace tag {
        template<std::size_t N>
        struct chunks_ : xtd::tagged_bindable<chunks_<N>, xtd::invokers::iter_chunks> {};
    }
    template<std::size_t N = 0>
    static constexpr tag::chunks_<N> chunks_;
}

ITER_ALIAS(chunks, chunks_<>)

namespace iter::detail {
    template<iter I, std::size_t N>
    struct [[nodiscard]] chunks_iter {
        I i;
        std::array<value_t<I>, N> data;

        using this_t = chunks_iter;
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
            std::size_t n = 0;
            while (auto next = iter::next(self.i)) {
                self.data[n++] = consume(next);
                if (n == N) [[unlikely]] break;
            }
            return n > 0 ? MAKE_OPTIONAL(self.data | take | n) : std::nullopt;
        }
    };

    template<iter I>
    struct [[nodiscard]] lazy_chunk_iter {
        std::uint32_t size;
        std::uint32_t remaining;
        I i;

        using this_t = lazy_chunk_iter;
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
            auto next = iter::no_next<I>();
            if (self.remaining--) [[likely]] {
                if (!emplace_next(next, self.i)) [[unlikely]] {
                    self.size = 0;
                }
            }
            return next;
        }
    };

    template<iter I>
    struct [[nodiscard]] chunks_iter<I, 0> : lazy_chunk_iter<I> {
        using this_t = chunks_iter;
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) -> lazy_chunk_iter<I>* {
            if (self.size) [[likely]] {
                self.remaining = self.size;
                return std::addressof(self);
            }
            return nullptr;
        }
    };
}

template<iter::iter I>
constexpr auto XTD_IMPL_TAG_(iter_chunks, iter::tag::chunks_<0>) (I&& iterable, std::uint32_t size) {
    return iter::detail::chunks_iter<std::remove_reference_t<I>, 0>{{size, size, (I&&)iterable}};
}

template<std::size_t N, iter::iter I>
constexpr auto XTD_IMPL_TAG_(iter_chunks, iter::tag::chunks_<N>) (I&& iterable) {
    return iter::detail::chunks_iter<std::remove_reference_t<I>, N>{(I&&)iterable, {}};
}

#endif /* INCLUDE_ITER_CHUNKS_HPP */

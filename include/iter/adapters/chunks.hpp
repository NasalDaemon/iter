#ifndef ITER_ADAPTERS_CHUNKS_HPP
#define ITER_ADAPTERS_CHUNKS_HPP

#include "iter/adapters/take.hpp"
#include "iter/iters/span.hpp"

XTD_INVOKER(iter_chunks)

namespace iter {
    namespace detail::tag {
        template<std::size_t N>
        struct chunks_ : xtd::tagged_bindable<chunks_<N>, xtd::invokers::iter_chunks> {};
    }
    template<std::size_t N = 0>
    static constexpr detail::tag::chunks_<N> chunks_;
}

ITER_ALIAS(chunks, chunks_<>)

namespace iter::detail {
    template<class T, std::size_t N>
    struct chunks_iter_storage;

    template<class T, std::size_t N>
    requires std::is_trivially_default_constructible_v<T>
    struct chunks_iter_storage<T, N>
    {
        T buffer[N]{};
    protected:
        template<class V>
        constexpr void assign(std::size_t n, V&& value) {
            EMPLACE_NEW(buffer[n], FWD(value));
        }
        constexpr auto to_iter(std::size_t n) { return span{buffer, n}; }
    };

    template<class T, std::size_t N>
    requires (!std::is_trivially_default_constructible_v<T>)
    struct chunks_iter_storage<T, N>
    {
        chunks_iter_storage() = default;
        constexpr chunks_iter_storage(chunks_iter_storage const& other) : buffer{}, size{other.size} {
            std::copy_n(other.array(), size, array());
        }
        constexpr ~chunks_iter_storage() {
            auto ptr = array();
            while (size) std::destroy_at(ptr + (--size));
        }
    protected:
        template<class V>
        constexpr void assign(std::size_t n, V&& value) {
            T* ptr = array() + n;
            if (n == size) [[unlikely]] {
                size++;
                std::construct_at(ptr, FWD(value));
            } else {
                *ptr = FWD(value);
            }
        }
        constexpr auto to_iter(std::size_t n) {
            return span{array(), n};
        }
    private:
        std::aligned_union_t<0, T> buffer[N]{};
        std::size_t size = 0;

        using array_t = T[N];
        constexpr array_t& array() {
            return *std::launder(reinterpret_cast<array_t*>(std::addressof(buffer)));
        }
        constexpr array_t const& array() const {
            return *std::launder(reinterpret_cast<array_t const*>(std::addressof(buffer)));
        }
    };

    template<assert_iter I, std::size_t N>
    struct [[nodiscard]] chunks_iter : chunks_iter_storage<value_t<I>, N> {
        [[no_unique_address]] I i;

        using this_t = chunks_iter;
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            std::size_t n = 0;
            while (auto next = impl::next(self.i)) {
                self.assign(n++, consume(next));
                if (n == N) [[unlikely]] break;
            }
            return n > 0 ? MAKE_ITEM(self.to_iter(n)) : noitem;
        }
    };

    template<assert_iter I>
    struct [[nodiscard]] lazy_chunk_iter {
        std::uint32_t size;
        std::uint32_t remaining;
        [[no_unique_address]] I i;

        using this_t = lazy_chunk_iter;
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            auto next = iter::no_next<I>();
            if (self.remaining) [[likely]] {
                --self.remaining;
                if (!emplace_next(next, self.i)) [[unlikely]] {
                    self.size = 0;
                }
            }
            return next;
        }
    };

    template<assert_iter I>
    struct [[nodiscard]] chunks_iter<I, 0> : lazy_chunk_iter<I> {
        using this_t = chunks_iter;
        constexpr unstable_item<lazy_chunk_iter<I>&> ITER_IMPL_NEXT (this_t& self) {
            if (self.size) [[likely]] {
                if (self.remaining) [[unlikely]] {
                    // Deal with the case where inner iter is not fully iterated
                    while (impl::next(static_cast<lazy_chunk_iter<I>&>(self)));
                    if (self.size == 0)
                        return noitem;
                }
                self.remaining = self.size;
                return unstable_ref<lazy_chunk_iter<I>&>(self);
            }
            return noitem;
        }
    };
}

template<iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_chunks, iter::detail::tag::chunks_<0>) (I&& iterable, std::uint32_t size) {
    return iter::detail::chunks_iter<std::remove_reference_t<I>, 0>{
        {.size = size, .remaining = 0, .i = FWD(iterable)}};
}

template<std::size_t N, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_chunks, iter::detail::tag::chunks_<N>) (I&& iterable) {
    return iter::detail::chunks_iter<std::remove_reference_t<I>, N>{{}, FWD(iterable)};
}

#endif /* ITER_ADAPTERS_CHUNKS_HPP */

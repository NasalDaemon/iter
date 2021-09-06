#ifndef INCLUDE_ITER_CHUNKS_HPP
#define INCLUDE_ITER_CHUNKS_HPP

#include "iter/take.hpp"
#include "iter/map.hpp"

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
    template<class T, std::size_t N>
    struct chunks_iter_storage;

    template<class T, std::size_t N>
    requires std::is_trivially_default_constructible_v<T>
    struct chunks_iter_storage<T, N>
    {
        std::array<T, N> buffer = {};
        template<class V>
        constexpr void assign(std::size_t n, V&& value) {
            EMPLACE_NEW(buffer[n], FWD(value));
        }
        constexpr auto to_iter(std::size_t n) { return take(buffer, n); }
    };

    template<class T, std::size_t N>
    requires (!std::is_trivially_default_constructible_v<T>)
    struct chunks_iter_storage<T, N>
    {
        std::array<std::aligned_union_t<0, T>, N> buffer = {};
        std::size_t size = 0;

        template<class V>
        constexpr void assign(std::size_t n, V&& value) {
            if (n == size) [[unlikely]]
                size++;
            else
                array()[n].~T();
            new (std::addressof(buffer[n]), constexpr_new_tag{}) T(FWD(value));
        }
        constexpr auto to_iter(std::size_t n) {
            return take(array(), n);
        }
        chunks_iter_storage() = default;
        constexpr chunks_iter_storage(chunks_iter_storage const& other) : buffer{}, size{other.size} {
            auto& ours = array(); auto& theirs = other.array();
            for (std::size_t i = 0; i < size; ++i)
                ours[i] = theirs[i];
        }
        constexpr chunks_iter_storage(chunks_iter_storage&& other) : buffer{}, size{other.size} {
            auto& ours = array(); auto& theirs = other.array();
            for (std::size_t i = 0; i < size; ++i)
                ours[i] = std::move(theirs[i]);
        }
        constexpr ~chunks_iter_storage() {
            auto& arr = array();
            while (size--) (arr[size]).~T();
        }
    private:
        constexpr auto& array() {
            using array_t = std::array<T, N>;
            static_assert(sizeof(buffer) == sizeof(array_t));
            return reinterpret_cast<array_t&>(buffer);
        }
    };

    template<assert_iter I, std::size_t N>
    struct [[nodiscard]] chunks_iter : chunks_iter_storage<value_t<I>, N> {
        [[no_unique_address]] I i;

        using this_t = chunks_iter;
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
            std::size_t n = 0;
            while (auto next = iter::next(self.i)) {
                self.assign(n++, consume(next));
                if (n == N) [[unlikely]] break;
            }
            return n > 0 ? MAKE_OPTIONAL(self.to_iter(n)) : std::nullopt;
        }
    };

    template<assert_iter I>
    struct [[nodiscard]] lazy_chunk_iter {
        std::uint32_t size;
        std::uint32_t remaining;
        [[no_unique_address]] I i;

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

    template<assert_iter I>
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

template<iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_chunks, iter::tag::chunks_<0>) (I&& iterable, std::uint32_t size) {
    return iter::detail::chunks_iter<std::remove_reference_t<I>, 0>{
        {.size = size, .remaining = size, .i = FWD(iterable)}};
}

template<std::size_t N, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_chunks, iter::tag::chunks_<N>) (I&& iterable) {
    return iter::detail::chunks_iter<std::remove_reference_t<I>, N>{{}, .i = FWD(iterable)};
}

#endif /* INCLUDE_ITER_CHUNKS_HPP */

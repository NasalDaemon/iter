#ifndef INCLUDE_ITER_CHUNKS_HPP
#define INCLUDE_ITER_CHUNKS_HPP

#include "iter/core.hpp"

ITER_DECLARE(chunks)

namespace iter::detail {
    template<iter I>
    struct [[nodiscard]] chunks_iter {
        struct chunk {
            std::uint32_t remaining;
            using this_t = chunk;
            constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
                auto next = iter::no_next<I>();
                if (self.remaining--) [[likely]] {
                    if (!emplace_next(next, self.outer().i)) [[unlikely]] {
                        self.outer().size = 0;
                    }
                }
                return next;
            }
            chunks_iter& outer() {
                return reinterpret_cast<chunks_iter&>(*this);
            }
        } chunk_;
        std::uint32_t size;
        I i;

        using this_t = chunks_iter;
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) -> chunk* {
            if (self.size) [[likely]] {
                self.chunk_.remaining = self.size;
                return std::addressof(self.chunk_);
            }
            return nullptr;
        }
    };
}

template<iter::iterable I>
constexpr auto ITER_IMPL(chunks) (I&& iterable, std::uint32_t size) {
    return iter::detail::chunks_iter<std::remove_reference_t<I>>{{size}, size, (I&&)iterable};
}

#endif /* INCLUDE_ITER_CHUNKS_HPP */

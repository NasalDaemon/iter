#ifndef INCLUDE_ITER_TO_ITER_HPP
#define INCLUDE_ITER_TO_ITER_HPP

#include "iter/core.hpp"
#include "iter/std_fwd.hpp"

namespace iter::detail {
    template<class T>
    struct [[nodiscard]] random_access_container_iter {
    protected:
        using this_t = random_access_container_iter;
        std::span<T> span;
        std::size_t pos;

    public:
        template<class Container>
        constexpr explicit random_access_container_iter(Container& container)
            : span{container}
            , pos{0}
        {}

        random_access_container_iter(const random_access_container_iter& other) = default;
        random_access_container_iter& operator=(const random_access_container_iter& other) = default;

        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index) {
            return self.span[index];
        }

        constexpr auto ITER_IMPL_SIZE (this_t const& self) {
            return self.span.size();
        }

        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            return self.pos != self.span.size()
                ? std::addressof(self.span[self.pos++])
                : nullptr;
        }

        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
            auto const size = self.span.size();
            return self.pos != size
                ? std::addressof(self.span[(size - 1 - self.pos++)])
                : nullptr;
        }

        struct cycle;

        constexpr auto ITER_IMPL_THIS(cycle) (this_t&& self) {
            return cycle{FWD(self)};
        }
    };

    template<class T>
    random_access_container_iter(T&) -> random_access_container_iter<std::remove_reference_t<decltype(std::declval<T&>()[0])>>;

    template<class T>
    struct random_access_container_iter<T>::cycle : random_access_container_iter<T> {
        using this_t = cycle;

        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index) -> auto& {
            const auto size = self.span.size();
            return self.span[index % size];
        }

        constexpr auto ITER_IMPL_SIZE (this_t const&) {
            return std::numeric_limits<std::size_t>::max();
        }

        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            self.pos = self.pos == self.span.size() ? 0 : self.pos;
            return std::addressof(self.span[self.pos++]);
        }

        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
            const auto size = self.span.size();
            self.pos = self.pos == size ? 0 : self.pos;
            return std::addressof(self.span[(size - 1 - self.pos++)]);
        }
     };
}

namespace iter::concepts {
    template<class T>
    static constexpr bool is_random_access_container = false;

    template<class T, std::size_t N>
    constexpr bool is_random_access_container<std::array<T, N>> = true;
    template<class T, class A>
    constexpr bool is_random_access_container<std::vector<T, A>> = true;
    template<class T, class U, class A>
    constexpr bool is_random_access_container<std::basic_string<T, U, A>> = true;

    template<class T>
    concept random_access_container = is_random_access_container<std::remove_cvref_t<T>>;

    template<class T>
    concept container = random_access_container<T>;
}

template<iter::concepts::random_access_container T>
constexpr auto ITER_IMPL(to_iter) (T& container) {
    return iter::detail::random_access_container_iter{container};
}

namespace iter {
    template<class T>
    struct pointer_to_iter {
        using this_t = pointer_to_iter;
        T* ptr;

        constexpr std::size_t ITER_IMPL_SIZE (this_t const& self) {
            return self.ptr ? 1 : 0;
        }
        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t) {
            return *self.ptr;
        }
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            return std::exchange(self.ptr, nullptr);
        }
    };

    template<class T>
    pointer_to_iter(T*) -> pointer_to_iter<T>;
}

#endif /* INCLUDE_ITER_TO_ITER_HPP */

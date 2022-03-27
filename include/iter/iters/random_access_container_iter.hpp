#ifndef ITER_ITERS_RANDOM_ACCESS_CONTAINER_ITER_HPP
#define ITER_ITERS_RANDOM_ACCESS_CONTAINER_ITER_HPP

#include "iter/core/core.hpp"
#include "iter/core/std_fwd.hpp"

namespace iter::detail {
    template<class Container>
    struct [[nodiscard]] random_access_container_iter {
    protected:
        using this_t = random_access_container_iter;
        Container* container;
        std::size_t pos;

    public:
        constexpr explicit random_access_container_iter(Container& under)
            : container{std::addressof(under)}
            , pos{0}
        {}

        random_access_container_iter(const random_access_container_iter& other) = default;
        random_access_container_iter& operator=(const random_access_container_iter& other) = default;

        constexpr auto ITER_IMPL_GET (this_t const& self, std::size_t index) {
            return stable_ref((*self.container)[index]);
        }

        constexpr auto ITER_IMPL_SIZE (this_t const& self) {
            return std::size(*self.container);
        }

        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            return self.pos != std::size(*self.container)
                ? item(stable_ref((*self.container)[self.pos++]))
                : noitem;
        }

        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
            auto const size = std::size(*self.container);
            return self.pos != size
                ? item(stable_ref((*self.container)[(size - 1 - self.pos++)]))
                : noitem;
        }

        struct cycle;

        constexpr auto ITER_IMPL_THIS(cycle) (this_t const& self) {
            return cycle{self};
        }
    };

    template<class T>
    random_access_container_iter(T&) -> random_access_container_iter<T>;

    template<class T>
    struct random_access_container_iter<T>::cycle : random_access_container_iter<T> {
        using this_t = cycle;

        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index) {
            const auto size = std::size(*self.container);
            return stable_ref((*self.container)[index % size]);
        }

        constexpr auto ITER_IMPL_SIZE (this_t const&) {
            return std::numeric_limits<std::size_t>::max();
        }

        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            self.pos = self.pos == std::size(*self.container) ? 0 : self.pos;
            return item(stable_ref((*self.container)[self.pos++]));
        }

        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
            const auto size = std::size(*self.container);
            self.pos = self.pos == size ? 0 : self.pos;
            return item(stable_ref((*self.container)[(size - 1 - self.pos++)]));
        }
     };
}

namespace iter::concepts {
    template<class T>
    inline constexpr bool is_random_access_container = false;

    template<class T, std::size_t N>
    inline constexpr bool is_random_access_container<T[N]> = true;
    template<class T, std::size_t N>
    inline constexpr bool is_random_access_container<std::array<T, N>> = true;
    template<class T, class A>
    inline constexpr bool is_random_access_container<std::vector<T, A>> = true;
    template<class T, class U, class A>
    inline constexpr bool is_random_access_container<std::basic_string<T, U, A>> = true;

    template<class T>
    concept random_access_container = is_random_access_container<std::remove_cvref_t<T>>;

    template<class T>
    concept container = random_access_container<T>;
}

// Could also use iter::span, but GCC performs better with random_access_container_iter
template<iter::concepts::random_access_container T>
constexpr auto ITER_IMPL(to_iter) (T& container) {
    return iter::detail::random_access_container_iter{container};
}

#endif /* ITER_ITERS_RANDOM_ACCESS_CONTAINER_ITER_HPP */

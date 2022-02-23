#ifndef ITER_CORE_TUPLE_HPP
#define ITER_CORE_TUPLE_HPP

/**
 * iter::tuple is a tuple that can only be initialized by aggregate,
 * making this class much more efficient for constructing tuple
 * elements by prvalue (eliding a move).
 */

namespace iter {
    namespace detail {
        template<std::size_t I, class T>
        struct tuple_element {
            [[no_unique_address]] T value;
            auto operator<=>(tuple_element const&) const = default;
        };

        template<class...>
        struct tuple_impl;
        template<std::size_t... Is, class... Ts>
        struct tuple_impl<std::index_sequence<Is...>, Ts...> : tuple_element<Is, Ts>... {
            auto operator<=>(tuple_impl const&) const = default;
        };

        template<std::size_t I, class T> T& get(tuple_element<I, T>& el) { return el.value; }
        template<std::size_t I, class T> T const& get(tuple_element<I, T> const& el) { return el.value; }
        template<std::size_t I, class T> T&& get(tuple_element<I, T>&& el) { return static_cast<T&&>(el.value); }
        template<std::size_t I, class T> T const&& get(tuple_element<I, T> const&& el) { return static_cast<T const&&>(el.value); }
        template<std::size_t I, class T> T element_type(tuple_element<I, T>*) { static_assert(I != I, "never to be invoked"); }
    }

    template<class... Ts>
    struct tuple : detail::tuple_impl<std::index_sequence_for<Ts...>, Ts...> {
        auto operator<=>(tuple const&) const = default;
        static constexpr std::size_t size() { return sizeof...(Ts); }
    };

    template<class... Ts>
    tuple(Ts...) -> tuple<Ts...>;

    namespace concepts {
        template<class T> static constexpr bool is_tuple = false;
        template<class... Ts> constexpr bool is_tuple<iter::tuple<Ts...>> = true;
        template<class T>
        concept tuple = is_tuple<T>;
        template<class T>
        concept decays_to_tuple = is_tuple<std::remove_cvref_t<T>>;
    }

    template<std::size_t I, concepts::decays_to_tuple Tuple>
    auto&& get(Tuple&& tuple) {
        static_assert(I < std::remove_cvref_t<Tuple>::size(), "Tuple index out of bounds");
        return detail::get<I>(FWD(tuple));
    }

    // Make a tuple with element types exactly the same as those returned from lazy_values()
    template<std::invocable... Fs>
    tuple<std::invoke_result_t<Fs>...> make_tuple_lazy(Fs&&... lazy_values) {
        static_assert((!std::same_as<void, std::invoke_result_t<Fs>> && ...));
        return {std::invoke(FWD(lazy_values))...};
    }

    template<class... Ts>
    tuple<Ts&&...> forward_as_tuple(Ts&&... values) {
        return {FWD(values)...};
    }

    template<class... Ts>
    tuple<Ts&...> tie(Ts&... values) {
        return {values...};
    }

    template<class F, concepts::decays_to_tuple Tuple>
    decltype(auto) apply(F&& func, Tuple&& tuple) {
        return [&]<std::size_t... Is>(std::index_sequence<Is...>) {
            return std::invoke(FWD(func), get<Is>(FWD(tuple))...);
        }(std::make_index_sequence<std::remove_cvref_t<Tuple>::size()>{});
    }

    template<class T, concepts::decays_to_tuple Tuple>
    decltype(auto) make_from_tuple(Tuple&& tuple) {
        return [&]<std::size_t... Is>(std::index_sequence<Is...>) {
            return T(get<Is>(FWD(tuple))...);
        }(std::make_index_sequence<std::remove_cvref_t<Tuple>::size()>{});
    }
}

// Implement tuple customization points in std namespace
template<std::size_t I, class... Ts>
struct std::tuple_element<I, iter::tuple<Ts...>> {
    static_assert(I < iter::tuple<Ts...>::size(), "Tuple index out of bounds.");
    using type = decltype(iter::detail::element_type<I>(std::declval<iter::tuple<Ts...>*>()));
};
template<class... Ts>
struct std::tuple_size<iter::tuple<Ts...>> {
    static constexpr std::size_t value = iter::tuple<Ts...>::size();
};

#endif /* ITER_CORE_TUPLE_HPP */

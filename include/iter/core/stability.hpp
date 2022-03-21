#ifndef ITER_CORE_STABILITY_HPP
#define ITER_CORE_STABILITY_HPP

namespace iter {

template<class T>
struct stable {
    T value;
    constexpr auto operator<=>(stable const& other) const {
        return std::compare_three_way{}(value, other.value);
    }
    constexpr bool operator==(stable const& other) const {
        return value == other.value;
    }
    constexpr operator T const&() const { return value; }
};
template<class T> stable(T) -> stable<T>;
template<class T>
struct unstable {
    auto operator<=>(unstable const&) const = default;
    constexpr operator T const&() const { return unstable.value; }
    stable<T> unstable;
};
template<class T> unstable(T) -> unstable<T>;
template<class T> unstable(stable<T>) -> unstable<T>;

template<bool Stable>
struct stability_wrapper {
    template<class T>
    using type = stable<T>;
};
template<>
struct stability_wrapper<false> {
    template<class T>
    using type = unstable<T>;
};

template<class T>
static constexpr stable<T&&> stable_ref(T&& ref) {
    return stable<T&&>{FWD(ref)};
}
template<class T>
static constexpr unstable<T&&> unstable_ref(T&& ref) {
    return unstable<T&&>{FWD(ref)};
}

namespace detail {
    template<class T>
    struct stability_traits {
        // types are stable unless otherwise specified
        static constexpr bool stable = true;
        static constexpr bool wrapper = false;
        using type = T;
    };
    template<class T>
    struct stability_traits<stable<T>> {
        static constexpr bool stable = true;
        static constexpr bool wrapper = true;
        using type = T;
    };
    template<class T>
    struct stability_traits<unstable<T>> {
        static constexpr bool stable = false;
        static constexpr bool wrapper = true;
        using type = T;
    };

    template<class T>
    using stability_unwrap = typename stability_traits<T>::type;

    template<class T, class U> struct forward_like { using type = U; };
    template<class T, class U> struct forward_like<T const, U> { using type = U const; };
    template<class T, class U> struct forward_like<T&, U> { using type = U&; };
    template<class T, class U> struct forward_like<T const&, U> { using type = U const&; };
    template<class T, class U> struct forward_like<T&&, U> { using type = U&&; };
    template<class T, class U> struct forward_like<T const&&, U> { using type = U const&&; };

} // namespace detail

namespace concepts {
    template<class T>
    concept stability_wrapper = iter::detail::stability_traits<std::remove_cvref_t<T>>::wrapper;

    template<class T>
    concept stable = iter::detail::stability_traits<std::remove_cvref_t<T>>::stable;

    template<class T>
    concept stable_wrapper = stability_wrapper<T> && stable<T>;
}

template<class T, class U>
using forward_like = typename detail::forward_like<T, U>::type;

template<concepts::stability_wrapper T>
static constexpr auto&& get(T&& stability) {
    using type = forward_like<T&&, detail::stability_unwrap<std::remove_cvref_t<T>>>;
    if constexpr (concepts::stable<T>)
        return static_cast<type>(FWD(stability).value);
    else
        return static_cast<type>(FWD(stability).unstable.value);
}

template<concepts::stability_wrapper... Ts>
using common_stability = typename stability_wrapper<(concepts::stable<Ts> && ...)>::template type<std::common_reference_t<detail::stability_unwrap<Ts>...>>;

template<bool Stable, std::invocable F>
static constexpr auto make_stability(F&& f) {
    using result_t = std::invoke_result_t<F>;
    using stability = typename stability_wrapper<Stable>::template type<result_t>;
    return stability{std::invoke(FWD(f))};
}

template<std::invocable F>
static constexpr auto make_stability(F&& f) {
    using result_t = std::invoke_result_t<F>;
    using stability = typename stability_wrapper<concepts::stable<result_t>>::template type<result_t>;
    return stability{std::invoke(FWD(f))};
}

} // namespace iter

#define MAKE_STABILITY(stable, ...) ::iter::make_stability<stable>([&]() -> decltype(auto) { return (__VA_ARGS__); })

#endif /* ITER_CORE_STABILITY_HPP */

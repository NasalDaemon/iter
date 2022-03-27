#ifndef ITER_CORE_ITEM_HPP
#define ITER_CORE_ITEM_HPP

#include "iter/core/stability.hpp"

// GCC cannot deal with empty payloads in debug
#if defined(NDEBUG) || !defined(ITER_COMPILER_GCC)
#  define ITER_ITEM_ABI_PAYLOAD_ATTRIBUTE [[no_unique_address]]
#  define ITER_ITEM_ABI_NAMESPACE iter
#  define ITER_ITEM_ABI_IMPORT
#else
#  define ITER_ITEM_ABI_PAYLOAD_ATTRIBUTE
#  define ITER_ITEM_ABI_NAMESPACE iter::detail::item_abi
#  define ITER_ITEM_ABI_IMPORT using ITER_ITEM_ABI_NAMESPACE::item;
#endif

namespace iter {

inline constexpr struct noitem_t {} noitem;

namespace detail {
    template<class T>
    union optional_payload {
        [[no_unique_address]] T value;
        [[no_unique_address]] void_t dummy{};
        constexpr ~optional_payload() {}
    };

// GCC can't do constexpr comparison with nullptr
// and it can't do much in constexpr before 12 anyway
#if defined(ITER_COMPILER_GCC) && __GNUC__ >= 12
#  define ITER_CONSTEXPR_NULLPTR ::iter::detail::constexpr_nullptr
    inline constexpr struct constexpr_nullptr_t {
        template<class T>
        constexpr operator T*() const {
            return const_cast<T*>(&null<T>.value);
        }

    private:
        template<class T>
        static constexpr optional_payload<T> null{};
    } constexpr_nullptr;
#else
#  define ITER_CONSTEXPR_NULLPTR nullptr
#endif
} // namespace detail

} // namespace iter

namespace ITER_ITEM_ABI_NAMESPACE {

template<class T, bool Stable = iter::concepts::stable<T>>
struct item {
    using wrapped_type = T;
    using value_type = T;
    using reference = T&;
    using pointer = T*;
    static constexpr bool owner = true;
    static constexpr bool stable = Stable;
    using stability_t = typename stability_wrapper<stable>::template type<T>;

    constexpr explicit item(std::invocable auto&& f) : engaged{true}, payload{make_payload(FWD(f))} {}
    constexpr item(auto&&... args) : engaged{true}, payload{make_payload(FWD(args)...)} {}

    constexpr item() = default;
    constexpr item(noitem_t) : item() {}

    constexpr item(item const& other)
    : engaged{other.engaged}
    , payload{[&] {
        if (other.engaged)
            return make_payload(other.value());
        else
            return payload_t{};
    }()}
    {}
    constexpr item(item&& other)
    : engaged{other.engaged}
    , payload{[&] {
        if (other.engaged)
            return make_payload(std::move(other.value()));
        else
            return payload_t{};
    }()}
    {}

    constexpr item& operator=(noitem_t) {
        reset();
        return *this;
    }
    constexpr item& operator=(item const& other) {
        if (std::exchange(engaged, other.engaged)) {
            if (other.engaged)
                payload.value = other.payload.value;
            else
                destroy();
        } else if (other.engaged) {
            std::construct_at(std::addressof(payload.value), other.payload.value);
        }
        return *this;
    }
    constexpr item& operator=(item&& other) {
        if (std::exchange(engaged, other.engaged)) {
            if (other.engaged)
                payload.value = std::move(other.payload.value);
            else
                destroy();
        } else if (other.engaged) {
            std::construct_at(std::addressof(payload.value), std::move(other.payload.value));
        }
        return *this;
    }

    constexpr bool has_value() const { return engaged; }
    constexpr operator bool() const { return has_value(); }

    template<std::three_way_comparable_with<T> U, bool S>
    constexpr auto operator<=>(item<U, S> const& other) const {
        if (auto comp = engaged <=> other.engaged; comp != 0)
            return comp;
        if (engaged)
            return std::compare_three_way{}(value(), other.value());
        return std::strong_ordering::equal;
    }
    template<std::equality_comparable_with<T> U, bool S>
    constexpr bool operator==(item<U, S> const& other) const {
        if (engaged != other.engaged)
            return false;
        if (engaged)
            return value() == other.value();
        return true;
    }

    constexpr auto operator<=>(noitem_t) const { return engaged <=> false; }
    constexpr bool operator==(noitem_t) const { return engaged == false; }

    constexpr auto& value() & { return get_value(*this); }
    constexpr auto& value() const & { return get_value(*this); }
    constexpr auto&& value() && { return std::move(get_value(*this)); }
    constexpr auto&& value() const && { return std::move(get_value(*this)); }

    constexpr auto* operator->() { return std::addressof(value()); }
    constexpr auto* operator->() const { return std::addressof(value()); }
    constexpr auto& operator*() { return value(); }
    constexpr auto& operator*() const { return value(); }

    constexpr auto&& consume() { return std::move(value()); }

    template<class V>
    constexpr item& operator=(V&& value) {
        if (std::exchange(engaged, true))
            this->value() = FWD(value);
        else
            std::construct_at(std::addressof(value()), FWD(value));
        return *this;
    }

    template<class... As>
    // requires std::constructible_from<stability_t, As...>
    constexpr item& emplace(As&&... args) {
        if (std::exchange(engaged, true))
            destroy();
        std::construct_at(std::addressof(value()), FWD(args)...);
        return *this;
    }

    template<std::invocable F>
    // requires std::constructible_from<stability_t, std::invoke_result_t<F>>
    constexpr item& operator=(F&& f) {
        return emplace(FWD(f));
    }

    template<std::invocable F>
    // requires std::constructible_from<stability_t, std::invoke_result_t<F>>
    constexpr item& emplace(F&& f) {
        if (std::exchange(engaged, true))
            destroy();
        new (std::addressof(payload.value), detail::constexpr_new_tag{}) stability_t{std::invoke(FWD(f))};
        return *this;
    }

    constexpr void reset() {
        if (std::exchange(engaged, false))
            destroy();
    }

    constexpr ~item() { if (engaged) destroy(); }

private:
    using payload_t = iter::detail::optional_payload<stability_t>;
    bool engaged = false;
    ITER_ITEM_ABI_PAYLOAD_ATTRIBUTE
    payload_t payload{};

    static constexpr auto&& get_value(auto&& self) {
        return get(FWD(self).payload.value);
    }

    // Workaround for GCC not dealing well with empty payloads
    template<std::invocable F>
    // requires std::constructible_from<stability_t, std::invoke_result_t<F>>
    static constexpr payload_t make_payload(F&& f) {
        return {.value{std::invoke(FWD(f))}};
    }
    template<class... Ts>
    // requires std::constructible_from<stability_t, Ts...>
    static constexpr payload_t make_payload(Ts&&... args) {
        return {.value{FWD(args)...}};
    }

    constexpr void destroy() { payload.value.~stability_t(); }
};

namespace detail {
    auto* addressof(auto&& ref) { return std::addressof(ref); }
}

template<class T, bool Stable>
requires std::is_reference_v<T>
struct item<T, Stable> {
    using wrapped_type = T;
    using value_type = std::remove_reference_t<T>;
    using reference = T;
    using pointer = value_type*;
    static constexpr bool owner = false;
    static constexpr bool stable = Stable;

    using stability_wrapper = std::conditional_t<stable, iter::stable<T>, iter::unstable<T>>;

    constexpr item(stability_wrapper ref) : ptr(std::addressof(get(ref))) {}
    template<std::invocable F>
    requires std::constructible_from<stability_wrapper, std::invoke_result_t<F>>
    constexpr explicit item(F&& f) : ptr(detail::addressof(get(stability_wrapper{std::invoke(FWD(f))}))) {}

    item() = default;
    constexpr item(noitem_t) : item() {}

    item(item const&) = default;
    item& operator=(item const&) = default;

    constexpr bool has_value() const { return ptr != ITER_CONSTEXPR_NULLPTR; }
    constexpr operator bool() const { return has_value(); }
    bool operator==(item const&) const = default;
    auto operator<=>(item const&) const = delete;

    item& operator=(T&& ref) { emplace(ref); return *this; }
    item& emplace(T&& ref) { ptr = std::addressof(ref); return *this; }

    template<std::invocable F>
    requires std::constructible_from<stability_wrapper, std::invoke_result_t<F>>
    item& operator=(F&& f) { return emplace(FWD(f)); }
    template<std::invocable F>
    requires std::constructible_from<stability_wrapper, std::invoke_result_t<F>>
    item& emplace(F&& f) { ptr = std::addressof(get(stability_wrapper{std::invoke(FWD(f))})); return *this; }

    constexpr T&& value() const { return static_cast<T&&>(*ptr); }

    constexpr auto* operator->() const { return ptr; }
    constexpr T&& operator*() const { return value(); }

    constexpr T&& consume() const { return value(); }

    constexpr void reset() { ptr = ITER_CONSTEXPR_NULLPTR; }

protected:
    pointer ptr = ITER_CONSTEXPR_NULLPTR;
    constexpr explicit item(pointer p) : ptr(p) {}
};

template<class T>
item(T) -> item<T>;
template<class T>
item(stable<T>) -> item<T, true>;
template<class T>
item(unstable<T>) -> item<T, false>;
template<std::invocable F>
item(F) -> item<iter::detail::stability_unwrap<std::invoke_result_t<F>>, concepts::stable<std::invoke_result_t<F>>>;

} // namespace ITER_ITEM_ABI_NAMESPACE

namespace iter {

ITER_ITEM_ABI_IMPORT

template<class T>
using stable_item = item<T, true>;
template<class T>
using unstable_item = item<T, false>;

template<class T>
struct move_item;

template<class T, bool Stable>
struct move_item<item<T, Stable>> : item<T, Stable> {
    using base = item<T, Stable>;
    bool operator==(move_item const&) const = default;
    auto operator<=>(move_item const&) const = default;

    constexpr auto&& value() { return std::move(this->base::value()); }
    constexpr auto&& value() const { return std::move(this->base::value()); }

    constexpr auto&& operator*() { return std::move(this->base::operator*()); }
    constexpr auto&& operator*() const { return std::move(this->base::operator*()); }

    constexpr auto&& consume() { return std::move(this->base::consume()); }
    constexpr auto&& consume() const { return std::move(this->base::consume()); }
};

template<class T>
move_item(T) -> move_item<T>;

namespace concepts {
    template<class T>
    inline constexpr bool is_move_item = false;
    template<class T, bool S>
    inline constexpr bool is_move_item<move_item<item<T, S>>> = true;
    template<class T>
    concept move_item = is_move_item<std::remove_cvref_t<T>>;

    template<class T>
    inline constexpr bool is_item = false;
    template<class T, bool S>
    inline constexpr bool is_item<iter::item<T, S>> = true;
    template<class T>
    concept item = move_item<T> || is_item<std::remove_cvref_t<T>>;

    template<class T>
    concept owned_item = item<T> && std::remove_cvref_t<T>::owner;
    template<class T>
    concept stable_item = item<T> && std::remove_cvref_t<T>::stable;
}

} // namespace iter

#define MAKE_ITEM(...) ::iter::item{[&] { return (__VA_ARGS__); }}
#define MAKE_ITEM_AUTO(...) ::iter::item([&]() -> decltype(auto) { return (__VA_ARGS__); })

#endif /* ITER_CORE_ITEM_HPP */

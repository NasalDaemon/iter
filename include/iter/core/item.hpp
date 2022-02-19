#ifndef INCLUDE_ITER_ITEM_HPP
#define INCLUDE_ITER_ITEM_HPP

namespace iter {

static constexpr struct noitem_t {} noitem;

template<class T>
struct item {
    using wrapped_type = T;
    using value_type = T;
    using reference = T&;
    using pointer = T*;
    static constexpr bool owner = true;

    constexpr explicit item(std::invocable auto&& f) : engaged{true}, payload{.value{std::invoke(FWD(f))}} {}
    constexpr item(auto&&... args) : engaged{true}, payload{.value{FWD(args)...}} {}

    constexpr item() = default;
    constexpr item(noitem_t) : item() {}

    constexpr item(item const& other)
    : engaged{other.engaged}
    , payload{[&] {
        if (other.engaged)
            return payload_t{.value{other.payload.value}};
        else
            return payload_t{};
    }()}
    {}
    constexpr item(item&& other)
    : engaged{other.engaged}
    , payload{[&] {
        if (other.engaged)
            return payload_t{.value{std::move(other.payload.value)}};
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
                payload.value.~T();
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
                payload.value.~T();
        } else if (other.engaged) {
            std::construct_at(std::addressof(payload.value), std::move(other.payload.value));
        }
        return *this;
    }

    constexpr bool has_value() const { return engaged; }
    constexpr operator bool() const { return has_value(); }

    constexpr auto operator<=>(item const& other) const {
        if (auto comp = engaged <=> other.engaged; comp != 0)
            return comp;
        if (engaged)
            return payload.value <=> other.payload.value;
        return std::strong_ordering::equal;
    }
    constexpr bool operator==(item const& other) const {
        if (engaged != other.engaged)
            return false;
        if (engaged)
            return payload.value == other.payload.value;
        return true;
    }

    constexpr auto& value() & { return payload.value; }
    constexpr auto& value() const & { return payload.value; }
    constexpr auto&& value() && { return std::move(payload.value); }
    constexpr auto&& value() const && { return std::move(payload.value); }

    constexpr auto* operator->() { return std::addressof(value()); }
    constexpr auto* operator->() const { return std::addressof(value()); }
    constexpr auto& operator*() { return value(); }
    constexpr auto& operator*() const { return value(); }

    constexpr auto&& consume() { return std::move(payload.value); }

    template<class V>
    constexpr item& operator=(V&& value) {
        if (std::exchange(engaged, true))
            payload.value = FWD(value);
        else
            std::construct_at(std::addressof(payload.value), FWD(value));
        return *this;
    }

    template<class... As>
    requires std::constructible_from<T, As...>
    constexpr item& emplace(As&&... args) {
        if (std::exchange(engaged, true))
            payload.value.~T();
        std::construct_at(std::addressof(payload.value), FWD(args)...);
        return *this;
    }

    template<std::invocable F>
    requires std::convertible_to<std::invoke_result_t<F>, T>
    constexpr item& operator=(F&& f) {
        return emplace(FWD(f));
    }

    template<std::invocable F>
    requires std::convertible_to<std::invoke_result_t<F>, T>
    constexpr item& emplace(F&& f) {
        if (std::exchange(engaged, true))
            payload.value.~T();
        new (std::addressof(payload.value), detail::constexpr_new_tag{}) T(std::invoke(FWD(f)));
        return *this;
    }

    constexpr void reset() {
        if (std::exchange(engaged, false))
            payload.value.~T();
    }

    constexpr ~item() { destroy(); }

private:
    bool engaged = false;
    [[no_unique_address]]
    union payload_t {
        [[no_unique_address]] void_t dummy{};
        [[no_unique_address]] T value;
        constexpr ~payload_t() {}
    } payload{};

    constexpr void destroy() {
        if (engaged)
            payload.value.~T();
    }
};

namespace detail {
auto* addressof(auto&& ref) { return std::addressof(ref); }
}

template<class T>
requires std::is_reference_v<T>
struct item<T> {
    using wrapped_type = T;
    using value_type = std::remove_reference_t<T>;
    using reference = T;
    using pointer = value_type*;
    static constexpr bool owner = false;

    constexpr item(T&& ref) : ptr(std::addressof(ref)) {}
    constexpr explicit item(std::invocable auto&& f) : ptr(detail::addressof(std::invoke(FWD(f)))) {}

    item() = default;
    constexpr item(noitem_t) : item() {}
    constexpr item(std::nullptr_t) : item() {}

    item(item const&) = default;
    item& operator=(item const&) = default;

    constexpr bool has_value() const { return ptr != nullptr; }
    constexpr operator bool() const { return has_value(); }
    bool operator==(item const&) const = default;
    auto operator<=>(item const&) const = delete;

    item& operator=(T&& ref) { emplace(ref); return *this; }
    item& emplace(T&& ref) { ptr = std::addressof(ref); return *this; }

    template<std::invocable F>
    requires std::same_as<std::invoke_result_t<F>, T>
    item& operator=(F&& f) { return emplace(FWD(f)); }
    template<std::invocable F>
    requires std::same_as<std::invoke_result_t<F>, T>
    item& emplace(F&& f) { ptr = detail::addressof(std::invoke(FWD(f))); return *this; }

    constexpr T&& value() const { return static_cast<T&&>(*ptr); }

    constexpr auto* operator->() const { return ptr; }
    constexpr T&& operator*() const { return value(); }

    constexpr T&& consume() const { return value(); }

    constexpr void reset() { ptr = nullptr; }

private:
    pointer ptr = nullptr;
    template<class TT>
    friend constexpr item<TT&> item_from_pointer(TT*);
    constexpr explicit item(pointer p) : ptr(p) {}
};

template<class T>
item(T) -> item<T>;
template<std::invocable F>
item(F) -> item<std::invoke_result_t<F>>;

template<class T>
static constexpr item<T&&> forward_as_item(T&& value) {
    return item<T&&>{FWD(value)};
}
template<class T>
static constexpr item<T&> item_from_pointer(T* ptr) {
    return item<T&>(ptr);
}

template<class T>
struct move_item;

template<class T>
struct move_item<item<T>> : item<T> {
    bool operator==(move_item const&) const = default;
    auto operator<=>(move_item const&) const = default;

    constexpr auto&& value() { return std::move(this->item<T>::value()); }
    constexpr auto&& value() const { return std::move(this->item<T>::value()); }

    constexpr auto&& operator*() { return std::move(this->item<T>::operator*()); }
    constexpr auto&& operator*() const { return std::move(this->item<T>::operator*()); }

    constexpr auto&& consume() { return std::move(this->item<T>::consume()); }
    constexpr auto&& consume() const { return std::move(this->item<T>::consume()); }
};

template<class T>
move_item(T) -> move_item<T>;

namespace concepts {
    template<class T>
    static constexpr bool is_move_item = false;
    template<class T>
    constexpr bool is_move_item<iter::move_item<T>> = true;
    template<class T>
    concept move_item = is_move_item<std::remove_cvref_t<T>>;

    template<class T>
    static constexpr bool is_item = false;
    template<class T>
    constexpr bool is_item<iter::item<T>> = true;
    template<class T>
    concept item = move_item<T> || is_item<std::remove_cvref_t<T>>;

    template<class T>
    concept owned_item = item<T> && std::remove_cvref_t<T>::owner;
}

}

#define MAKE_ITEM(...) ::iter::item{[&] { return (__VA_ARGS__); }}
#define MAKE_ITEM_AUTO(...) ::iter::item([&]() -> decltype(auto) { return (__VA_ARGS__); })

#endif /* INCLUDE_ITER_ITEM_HPP */

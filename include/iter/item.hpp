#ifndef INCLUDE_ITER_ITEM_HPP
#define INCLUDE_ITER_ITEM_HPP

namespace iter {

static constexpr struct noitem_t {} noitem;

template<class T>
struct item {
    using value_type = T;
    using reference = T&;
    using pointer = T*;
    static constexpr bool owner = true;

    constexpr explicit item(std::invocable auto&& f) : inner{.payload{.value{std::invoke(FWD(f))}}, .engaged{true}} {}
    constexpr item(auto&&... args) : inner{.payload{.value{FWD(args)...}}, .engaged{true}} {}

    constexpr item() : inner{.payload{.dummy{0}}, .engaged{false}} {}
    constexpr item(noitem_t) : item() {}

    constexpr item(item const& other) : inner{
        .payload = [&] {
            if (other.inner.engaged)
                return payload_t{.value{other.inner.payload.value}};
            else
                return payload_t{};
        }(),
        .engaged = other.inner.engaged
    } {}
    constexpr item(item&& other) : inner{
        .payload = [&] {
            if (other.inner.engaged)
                return payload_t{.value{std::move(other.inner.payload.value)}};
            else
                return payload_t{};
        }(),
        .engaged = other.inner.engaged
    } {}

    constexpr item& operator=(item const& other) {
        if (std::exchange(inner.engaged, other.inner.engaged)) {
            if (other.inner.engaged)
                inner.payload.value = other.inner.payload.value;
            else
                inner.payload.value.~T();
        } else if (other.inner.engaged) {
            std::construct_at(std::addressof(inner.payload.value), other.inner.payload.value);
        }
        return *this;
    }
    constexpr item& operator=(item&& other) {
        if (std::exchange(inner.engaged, other.inner.engaged)) {
            if (other.inner.engaged)
                inner.payload.value = std::move(other.inner.payload.value);
            else
                inner.payload.value.~T();
        } else if (other.inner.engaged) {
            std::construct_at(std::addressof(inner.payload.value), std::move(other.inner.payload.value));
        }
        return *this;
    }

    constexpr bool has_value() const { return inner.engaged; }
    constexpr operator bool() const { return has_value(); }

    constexpr auto operator<=>(item const& other) const {
        if (auto comp = inner.engaged <=> other.inner.engaged; comp != 0)
            return comp;
        if (inner.engaged)
            return inner.payload.value <=> other.inner.payload.value;
        return std::strong_ordering::equal;
    }
    constexpr bool operator==(item const& other) const {
        if (inner.engaged != other.inner.engaged)
            return false;
        if (inner.engaged)
            return inner.payload.value == other.inner.payload.value;
        return true;
    }

    constexpr auto& value() & { return inner.payload.value; }
    constexpr auto& value() const & { return inner.payload.value; }
    constexpr auto&& value() && { return std::move(inner.payload.value); }
    constexpr auto&& value() const && { return std::move(inner.payload.value); }

    constexpr auto* operator->() { return std::addressof(value()); }
    constexpr auto* operator->() const { return std::addressof(value()); }
    constexpr auto& operator*() { return value(); }
    constexpr auto& operator*() const { return value(); }

    constexpr auto&& consume() { return std::move(inner.payload.value); }

    template<class V>
    constexpr item& operator=(V&& value) {
        if (std::exchange(inner.engaged, true))
            inner.payload.value = FWD(value);
        else
            std::construct_at(std::addressof(inner.payload.value), FWD(value));
        return *this;
    }

    template<class... As>
    requires std::constructible_from<T, As...>
    constexpr void emplace(As&&... args) {
        if (std::exchange(inner.engaged, true))
            inner.payload.value.~T();
        std::construct_at(std::addressof(inner.payload.value), FWD(args)...);
    }

    template<std::invocable F>
    requires std::convertible_to<std::invoke_result_t<F>, T>
    constexpr item& operator=(F&& f) {
        emplace(FWD(f));
        return *this;
    }

    template<std::invocable F>
    requires std::convertible_to<std::invoke_result_t<F>, T>
    constexpr void emplace(F&& f) {
        if (std::exchange(inner.engaged, true))
            inner.payload.value.~T();
        new (std::addressof(inner.payload.value), detail::constexpr_new_tag{}) T(std::invoke(FWD(f)));
    }

    constexpr void reset() {
        if (std::exchange(inner.engaged, false))
            inner.payload.value.~T();
    }

    constexpr ~item() { destroy(); }

private:
    struct inner_t {
        union Payload {
            T value;
            char dummy;
            constexpr ~Payload() {}
        } payload;
        bool engaged;
    } inner;

    using payload_t = typename inner_t::Payload;

    constexpr void destroy() {
        if (inner.engaged)
            inner.payload.value.~T();
    }
};

namespace detail {
auto* addressof(auto&& ref) { return std::addressof(ref); }
}

template<class T>
requires std::is_reference_v<T>
struct item<T> {
    using value_type = std::remove_reference_t<T>;
    using reference = T;
    using pointer = value_type*;
    static constexpr bool owner = false;

    constexpr item(T&& ref) : ptr(std::addressof(ref)) {}
    constexpr explicit item(std::invocable auto&& f) : ptr(detail::addressof(std::invoke(FWD(f)))) {}

    constexpr item() : ptr(nullptr) {}
    constexpr item(noitem_t) : item() {}
    constexpr item(std::nullptr_t) : item() {}

    item(item const&) = default;
    item& operator=(item const&) = default;

    constexpr bool has_value() const { return ptr != nullptr; }
    constexpr operator bool() const { return has_value(); }
    bool operator==(item const&) const = default;
    auto operator<=>(item const&) const = delete;

    item& operator=(T&& ref) { emplace(ref); return *this; }
    void emplace(T&& ref) { ptr = std::addressof(ref); }

    template<std::invocable F>
    requires std::same_as<std::invoke_result_t<F>, T>
    item& operator=(F&& f) { emplace(FWD(f)); return *this; }
    template<std::invocable F>
    requires std::same_as<std::invoke_result_t<F>, T>
    void emplace(F&& f) { ptr = detail::addressof(std::invoke(FWD(f))); }

    constexpr T&& value() const { return static_cast<T&&>(*ptr); }

    constexpr auto* operator->() const { return ptr; }
    constexpr T&& operator*() const { return value(); }

    constexpr T&& consume() const { return value(); }

    constexpr void reset() { ptr = nullptr; }

private:
    pointer ptr;
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

    constexpr decltype(auto) value() { return std::move(this->item<T>::value()); }
    constexpr decltype(auto) value() const { return std::move(this->item<T>::value()); }

    constexpr auto&& operator*() { return std::move(this->item<T>::operator*()); }
    constexpr auto&& operator*() const { return std::move(this->item<T>::operator*()); }

    constexpr decltype(auto) consume() { return std::move(this->item<T>::consume()); }
    constexpr decltype(auto) consume() const { return std::move(this->item<T>::consume()); }
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

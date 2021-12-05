/*
BSD 3-Clause License

Copyright (c) 2021, NasalDaemon
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef ITER_INCLUDE_ITER_HPP
#define ITER_INCLUDE_ITER_HPP

#ifndef INCLUDE_ITER_CORE_HPP
#define INCLUDE_ITER_CORE_HPP

#ifndef ITER_LIBRARY_VERSION
#  define ITER_LIBRARY_VERSION 20211205
#endif

#ifndef EXTEND_INCLUDE_EXTEND_HPP
#define EXTEND_INCLUDE_EXTEND_HPP

#include <type_traits>
#include <concepts>
#include <functional>
#include <string_view>

#ifndef FWD
#  define FWD(arg) static_cast<decltype(arg)&&>(arg)
#endif

inline namespace xtd_adl {

struct xtd_free_t {};
template<class... T> struct xtd_method_t;
template<> struct xtd_method_t<> {};
template<class T> struct xtd_method_t<T> : xtd_method_t<> {
    constexpr xtd_method_t(xtd_method_t<>){}
    constexpr xtd_method_t() = default;
};

}

namespace {
static constexpr bool xtd_invoker_defined_in_root_namespace = false;
}

namespace xtd {

namespace detail {
    template<auto U = []{}>
    using unique_t = decltype(U);
}

namespace invokers {
    static constexpr bool xtd_invoker_defined_in_root_namespace = true;

    struct main {
        template<class... Ts>
        static constexpr auto invoke(Ts&&... in)
            noexcept(noexcept(xtd_invoke_main(FWD(in)...)))
            -> decltype(xtd_invoke_main(FWD(in)...))
        {
            return xtd_invoke_main(FWD(in)...);
        }
    };
}

template<
    class U = detail::unique_t<>,
    class Invoker = invokers::main,
    class Interface = void,
    class... Fs>
struct bindable;

template<size_t I = 0>
struct bind_placeholder {
    static constexpr std::size_t index = I;
};

template<class F, std::size_t Arity = 0, bool Owner = false>
struct bound;

template<class F, bool O>
struct [[nodiscard]] bound<F, 0, O> : F {
    static constexpr std::size_t arity = 0;
    static constexpr bool owner = O;
};

template<size_t A = 0, bool O = false, class F>
static constexpr bound<std::decay_t<F>, A, O> make_bound(F&& f) {
    return { FWD(f) };
}

namespace concepts {
    template<class>
    constexpr bool is_bindable = false;

    template<class U, class... Fs>
    constexpr bool is_bindable<bindable<U, Fs...>> = true;

    template<class T>
    concept Bindable = is_bindable<typename std::decay_t<T>::bindable_t>;

    template<class T>
    struct check_untagged {
        static constexpr bool value = !std::decay_t<T>::is_tagged();
        static_assert(value, "Must use impl_tag/impl_tag_this");
    };

    template<class T>
    concept UntaggedBindable = Bindable<T> && check_untagged<T>::value;

    template<class T>
    concept SpecializableBindable = Bindable<T> && std::decay_t<T>::is_specializable;

    template<class>
    constexpr bool is_bind_placeholder = false;
    template<size_t I>
    constexpr bool is_bind_placeholder<bind_placeholder<I>> = true;

    template<class T>
    concept BindPlaceholder = is_bind_placeholder<std::decay_t<T>>;

    template<class T, std::size_t I>
    concept BindPlaceholderAt = BindPlaceholder<T> && std::decay_t<T>::index == I;

    template<class CP, class... Ts>
    concept CustomisedFree = requires (const CP& bindable, Ts&&... in) {
        CP::invoker_t::invoke(xtd_free_t{}, bindable.self(), FWD(in)...);
    };
    template<class CP, class... Ts>
    concept CustomisedMethod = requires (const CP& bindable, Ts&&... in) {
        CP::invoker_t::invoke(xtd_method_t<>{}, bindable.self(), FWD(in)...);
    };
    template<class CP, class... Ts>
    concept Customised = CustomisedMethod<CP, Ts...> || CustomisedFree<CP, Ts...>;

    template<class T, class R>
    concept CompatInterface = std::constructible_from<R, T>;

    template<class CP, class... Ts>
    concept AllowedFree = requires (const CP& bindable, Ts&&... in) {
        { CP::invoker_t::invoke(xtd_free_t{}, bindable.self(), FWD(in)...) } ->
            CompatInterface<decltype(typename CP::interface_t{}.apply(FWD(in)...))>;
    };

    template<class CP, class... Ts>
    concept AllowedMethod = requires (const CP& bindable, Ts&&... in) {
        { CP::invoker_t::invoke(xtd_method_t<>{}, bindable.self(), FWD(in)...) } ->
            CompatInterface<decltype(typename CP::interface_t{}.apply(FWD(in)...))>;
    };

    template<class T>
    constexpr bool is_bound = false;
    template<class F, std::size_t A, bool O>
    constexpr bool is_bound<bound<F, A, O>> = true;

    template<class T, std::size_t A = 0>
    concept Bound = is_bound<std::decay_t<T>> && std::decay_t<T>::arity == A;
}

template<auto& obj>
requires (concepts::UntaggedBindable<decltype(obj)> && concepts::SpecializableBindable<decltype(obj)>)
using tag_of = const std::remove_cvref_t<decltype(obj)>&;

template<class L, concepts::Bound R>
requires (!concepts::Bound<L>)
constexpr decltype(auto) operator ->* (L&& l, const R& r) {
    return r(FWD(l));
}

template<class L, concepts::Bindable R>
requires (!concepts::Bound<L>)
constexpr decltype(auto) operator ->* (L&& l, const R& r) {
    return make_bound([&](auto&&... in) {
        return r(FWD(l), FWD(in)...);
    });
}

template<concepts::Bound L, concepts::Bound R>
constexpr auto operator ->* (L&& l, R&& r) {
    static_assert(std::decay_t<L>::owner && std::decay_t<R>::owner,
        "When building a chain of bindables that is not immediately invoked, "
        "please call bindable.capture(...) instead of bindable(...)");
    return make_bound<0, true>(
        [l = FWD(l), r = FWD(r)](auto&& in) {
            return l(FWD(in)) ->* r; });
}

template<concepts::Bound L, class R>
requires (!concepts::Bound<R>)
constexpr decltype(auto) operator ->* (const L& l, R&& r) {
    return l(FWD(r));
}

template<concepts::Bound L>
constexpr decltype(auto) operator ->* (const L& l, bind_placeholder<>) {
    return l();
}

constexpr decltype(auto) operator | (auto&& l, auto&& r) {
    return FWD(l) ->* FWD(r);
}

namespace detail {
    template<class... Fs>
    struct overload : Fs... {
        using Fs::operator()...;
    };
    template<class... Fs>
    overload(Fs...) -> overload<Fs...>;

    template<class R>
    constexpr decltype(auto) select(auto&& l, R&& r) {
        if constexpr (concepts::BindPlaceholder<R>)
            return FWD(l);
        else
            return FWD(r);
    }

    template<std::size_t>
    struct sink {
        constexpr sink(auto&&...) {}
    };

    template<std::size_t... Is>
    constexpr decltype(auto) get_value(sink<Is>..., auto&& value, auto&&...) {
        return FWD(value);
    }

    template<std::size_t I, class... Ts>
    constexpr decltype(auto) get_i(Ts&&... args) {
        return [&]<std::size_t... Is>(std::index_sequence<Is...>) -> decltype(auto) {
            return get_value<(0*Is)...>(FWD(args)...);
        }(std::make_index_sequence<sizeof...(Ts)-1>{});
    }

    template<size_t BI, std::size_t I>
    constexpr decltype(auto) select_i(auto&& l, auto&&... r) {
        if constexpr (I == BI)
            return FWD(l);
        else if constexpr (I > BI)
            return get_i<I - 1>(FWD(r)...);
        else
            return get_i<I>(FWD(r)...);
    }
}

template<class U, class Invoker, class Interface, class... Fs>
struct bindable : detail::overload<Fs...> {
    using bindable_t = bindable;
    using invoker_t = Invoker;
    using interface_t = Interface;
    static constexpr bool is_tagged() { return std::is_base_of_v<bindable, U>; }
    static constexpr bool is_specializable = sizeof...(Fs) == 0;

    template<class... Ts>
    // This call operator is only enabled if it can be specialised
    requires (is_specializable && sizeof...(Ts) > 0)
          && (!concepts::BindPlaceholder<Ts> && ...)
          && concepts::Customised<bindable, Ts...>
    constexpr decltype(auto) operator()(Ts&&... ins) const {
        // Prioritise the method overload
        using dispatcher = std::conditional_t<concepts::CustomisedMethod<bindable, Ts...>, xtd_method_t<>, xtd_free_t>;
        if constexpr (std::is_same_v<void, Interface>)
            return invoker_t::invoke(dispatcher{}, self(), FWD(ins)...);
        else {
            if constexpr (concepts::CustomisedMethod<bindable, Ts...>)
                static_assert(concepts::AllowedMethod<bindable, Ts...>,
                    "No implementation for this parameter set follows the interface");
            else
                static_assert(concepts::AllowedFree<bindable, Ts...>,
                    "No implementation for this parameter set follows the interface");
            using return_t = decltype(Interface{}.apply(FWD(ins)...));
            if constexpr (std::is_same_v<void, return_t>)
                return invoker_t::invoke(dispatcher{}, self(), FWD(ins)...);
            else
                return return_t(invoker_t::invoke(dispatcher{}, self(), FWD(ins)...));
        }
    }

    template<class... Ts>
    // This call operator is only enabled if it has its own function calls
    requires (!is_specializable && sizeof...(Ts) > 0)
          && (!concepts::BindPlaceholder<Ts> && ...)
          && (std::invocable<Fs const, Ts...> || ...)
    constexpr decltype(auto) operator()(Ts&&... ins) const {
        return detail::overload<Fs...>::operator()(FWD(ins)...);
    }

    [[nodiscard]] constexpr auto operator()() const {
        return operator()(bind_placeholder<0>{});
    }

    template<class... Ts>
    requires (sizeof...(Ts) > 0) && (concepts::BindPlaceholderAt<Ts, 0> || ...)
    [[nodiscard]] constexpr auto operator()(Ts&&... in) const {
        // Being an owner is free if there is nothing to bind
        return capture<sizeof...(Ts) == 1>(FWD(in)...);
    }

    // Placeholder as first argument is common case
    template<bool Store = true, class... Ts>
    requires (!concepts::BindPlaceholder<Ts> && ...)
    [[nodiscard]] constexpr auto capture(const bind_placeholder<0>&, Ts&&... in) const {
        if constexpr (Store) {
            return make_bound<0, Store>([this, ...in = FWD(in)](auto&& l) {
                return (*this)(FWD(l), in...);
            });
        } else {
            return make_bound([&, this](auto&& l) {
                return (*this)(FWD(l), FWD(in)...);
            });
        }
    }

    // Placeholder in any position general case
    template<bool Store = true, class... Ts>
    requires (sizeof...(Ts) > 0) && (concepts::BindPlaceholderAt<Ts, 0> || ...)
    [[nodiscard]] constexpr auto capture(Ts&&... in) const {
        constexpr std::size_t count = ((concepts::BindPlaceholder<Ts> ? 1 : 0) + ...);
        static_assert(count == 1, "May specify only one placeholder");
        if constexpr (Store) {
            return make_bound<0, Store>([this, ...in = FWD(in)](auto&& l) {
                return (*this)(detail::select(FWD(l), in)...);
            });
        } else {
            return make_bound([&, this](auto&& l) {
                return (*this)(detail::select(FWD(l), FWD(in))...);
            });
        }
    }

    template<size_t I, class... Ts>
    requires (I > 0) && (!concepts::BindPlaceholder<Ts> && ...)
    [[nodiscard]] constexpr auto operator()(const bind_placeholder<I>&, Ts&&... ins) const {
        static_assert(I <= sizeof...(Ts), "Placeholder index is out of bounds");
        // Being an owner is free if there is nothing to bind
        return bind_n<I, sizeof...(Ts) == 0>(std::make_index_sequence<1 + sizeof...(Ts)>{}, FWD(ins)...);
    }

    template<size_t I, class... Ts>
    requires (I > 0) && (!concepts::BindPlaceholder<Ts> && ...)
    [[nodiscard]] constexpr auto capture(const bind_placeholder<I>&, Ts&&... ins) const {
        static_assert(I <= sizeof...(Ts), "Placeholder index is out of bounds");
        return bind_n<I, true>(std::make_index_sequence<1 + sizeof...(Ts)>{}, FWD(ins)...);
    }

    constexpr auto& self() const {
        if constexpr (is_tagged())
            return static_cast<U const&>(*this);
        else
            return *this;
    }

private:
    template<size_t BI, bool Store, std::size_t... Is>
    constexpr decltype(auto) bind_n(std::index_sequence<Is...>, auto&&... ins) const {
        if constexpr (Store) {
            return make_bound<0, Store>([this, ...ins = FWD(ins)](auto&& l) {
                return (*this)(detail::select_i<BI, Is>(FWD(l), ins...)...);
            });
        } else {
            return make_bound([&, this](auto&& l) {
                return (*this)(detail::select_i<BI, Is>(FWD(l), FWD(ins)...)...);
            });
        }
    }
};

template<class... F>
bindable(F...) -> bindable<void, invokers::main, void, F...>;

template<class Tag, class Invoker = xtd::invokers::main, class Interface = void>
using tagged_bindable = bindable<Tag, Invoker, Interface>;

constexpr auto apply(auto&& func) {
    return [func = FWD(func)](auto&& tuple) mutable {
        return apply(func, FWD(tuple));
    };
}

namespace literals {
    [[maybe_unused]] static constexpr bind_placeholder _;

    template<char... C>
    requires (sizeof...(C) == 1 && ((C >= '0') && ...) && ((C <= '9') && ...))
    constexpr auto operator ""_() {
        return bind_placeholder<(C - '0')...>{};
    }
}

} // namespace xtd

// OVERLOAD MACROS

#define XTD_FUNCTION static constexpr ::xtd::bindable<::xtd::detail::unique_t<>, ::xtd::invokers::main, void>

#define XTD_INVOKER(name) \
    namespace xtd::invokers {\
        static_assert(xtd_invoker_defined_in_root_namespace,\
            "Invokers must be declared in root namespace");\
        struct name {\
            template<class... Ts>\
            static constexpr auto invoke(Ts&&... in)\
                noexcept(noexcept(xtd_invoke_ ## name(FWD(in)...)))\
                -> decltype(xtd_invoke_ ## name(FWD(in)...))\
            {\
                return xtd_invoke_ ## name(FWD(in)...);\
            }\
        };\
    }

#ifdef _MSC_VER
#  define XTD_FUNCTION_(invoker, ... /*interface*/) \
    static constexpr ::xtd::bindable<::xtd::detail::unique_t<>, ::xtd::invokers::invoker, __VA_ARGS__>
#else
#  define XTD_FUNCTION_(invoker, ... /*interface*/) \
    static constexpr ::xtd::bindable<::xtd::detail::unique_t<>, ::xtd::invokers::invoker __VA_OPT__(, __VA_ARGS__)>
#endif

#ifdef _MSC_VER
#  define XTD_IMPL_TAIL(...) , __VA_ARGS__)
#else
#  define XTD_IMPL_TAIL(...) __VA_OPT__(,) __VA_ARGS__)
#endif

namespace xtd::detail {
    template<class T, class Bindable, std::same_as<typename std::remove_cvref_t<Bindable>::invoker_t> Invoker>
    // Ensure that the right invoker is being used for the tag
    using assert_invoker_t = T;
}

#define XTD_IMPL_(scope, ... /*tag*/) \
    extern /* to disable in class */ xtd_invoke_ ## scope(\
        ::xtd::detail::assert_invoker_t<::xtd_free_t, decltype(__VA_ARGS__), ::xtd::invokers::scope>,\
        ::xtd::tag_of<__VA_ARGS__> XTD_IMPL_TAIL
#define XTD_IMPL_TAG_(scope, ... /*tag*/) \
    extern /* to disable in class */ xtd_invoke_ ## scope(\
        ::xtd::detail::assert_invoker_t<::xtd_free_t, __VA_ARGS__, ::xtd::invokers::scope>,\
        const __VA_ARGS__& XTD_IMPL_TAIL
#define XTD_IMPL_THIS_(scope, ... /*tag*/) \
    friend /* allow only in class */ xtd_invoke_ ## scope(\
        ::xtd::detail::assert_invoker_t<::xtd_method_t<this_t>, decltype(__VA_ARGS__), ::xtd::invokers::scope>,\
        ::xtd::tag_of<__VA_ARGS__> XTD_IMPL_TAIL
#define XTD_IMPL_TAG_THIS_(scope, ... /*tag*/) \
    friend /* allow only in class */ xtd_invoke_ ## scope(\
        ::xtd::detail::assert_invoker_t<::xtd_method_t<this_t>, __VA_ARGS__, ::xtd::invokers::scope>,\
        const __VA_ARGS__& XTD_IMPL_TAIL

#define XTD_IMPL(... /*tag*/)          XTD_IMPL_(main,__VA_ARGS__)
#define XTD_IMPL_TAG(... /*tag*/)      XTD_IMPL_TAG_(main,__VA_ARGS__)
#define XTD_IMPL_THIS(... /*tag*/)     XTD_IMPL_THIS_(main,__VA_ARGS__)
#define XTD_IMPL_TAG_THIS(... /*tag*/) XTD_IMPL_TAG_THIS_(main,__VA_ARGS__)

// EVIL MACROS

#define XTD_IMPORT_SPECIALIZED_OVL(t, tag, alias, qualifier) \
    template<class... _T_UGLY_>\
    constexpr auto alias(_T_UGLY_&&... ins) qualifier \
        noexcept(noexcept(xtd_invoke_main(xtd_method_t<t>{}, tag, std::declval<t qualifier>(), std::forward<decltype(ins)>(ins)...)))\
        -> decltype(xtd_invoke_main(xtd_method_t<t>{}, tag, std::declval<t qualifier>(), std::forward<decltype(ins)>(ins)...))\
    {\
        return xtd_invoke_main(xtd_method_t<t>{}, tag, static_cast<t qualifier>(*this), std::forward<decltype(ins)>(ins)...);\
    }\

#define XTD_IMPORT_SPECIALIZED(type, tag, alias) \
    XTD_IMPORT_SPECIALIZED_OVL(type, tag, alias, &)\
    XTD_IMPORT_SPECIALIZED_OVL(type, tag, alias, const &)\
    XTD_IMPORT_SPECIALIZED_OVL(type, tag, alias, &&)\
    XTD_IMPORT_SPECIALIZED_OVL(type, tag, alias, const &&)

#define XTD_USING_IMPL_1(nam)                 XTD_IMPORT_SPECIALIZED(this_t, nam, nam)
#define XTD_USING_IMPL_2(namspace, nam)       XTD_IMPORT_SPECIALIZED(this_t, namspace::nam, nam)
#define XTD_USING_IMPL_3(type, namspace, nam) XTD_IMPORT_SPECIALIZED(type, namspace::nam, nam)

#define XTD_IMPL_TRY_METHOD(tag, func) \
    constexpr auto impl(tag) (auto&& obj, auto&&... in)\
        noexcept(noexcept(FWD(obj).func(FWD(in)...)))\
        -> decltype(FWD(obj).func(FWD(in)...))\
    {\
        return FWD(obj).func(FWD(in)...);\
    }

#define XTD_IMPL_TRY_FORWARD(tag, func) \
    template<class... Ts>\
    constexpr auto impl(tag) (Ts&&... in)\
        noexcept(noexcept(func(FWD(in)...)))\
        -> decltype(func(FWD(in)...))\
    {\
        return func(FWD(in)...);\
    }

#endif /* EXTEND_INCLUDE_EXTEND_HPP */

#include <optional>
#include <memory>
#include <limits>

#ifndef INCLUDE_ITER_EMPLACE_NEW_HPP
#define INCLUDE_ITER_EMPLACE_NEW_HPP

// Semantically equivalent to `current = expr`, but constructs expr
// directly inside current's memory without any intermediate moves
#define EMPLACE_NEW(current, ... /*expr*/) \
    ::iter::detail::emplace_new_impl(current, [&]<class T_UGLY>() { return T_UGLY(__VA_ARGS__); })

// Semantically equivalent to `std::make_optional(expr)`, but constructs expr
// directly inside the optional's payload without any intermediate moves
#define MAKE_OPTIONAL(... /*expr*/) \
    ::iter::detail::make_optional_impl([&]() -> decltype(auto) { return (__VA_ARGS__); })

namespace iter {
    struct void_t {
        template<class... Ts> constexpr void_t(Ts&&...) {}
    };
    namespace detail {
        struct constexpr_new_tag {};
    }
}

constexpr void* operator new(std::size_t, void* ptr, iter::detail::constexpr_new_tag) {
    return ptr;
}

namespace iter::detail {
    template<class T, class F>
    requires (!std::is_const_v<T>)
    static constexpr T& emplace_new_impl(T& current, F&& ctor) {
        if constexpr (std::assignable_from<T&, T>)
            if (std::is_constant_evaluated())
                // placement new not strictly speaking constexpr although GCC allows it
                return current = (FWD(ctor)).template operator()<T>();
        current.~T();
        new (std::addressof(current), constexpr_new_tag{}) T(FWD(ctor).template operator()<T>());
        return current;
    }

    struct rvo_empty_base {};
    template<class T>
    struct rvo_ctor : rvo_empty_base {
        T value;
        template<class F>
        rvo_ctor(F&& f) : value{std::invoke(FWD(f))} {}
    };

    template<class F>
    std::optional<std::remove_cvref_t<std::invoke_result_t<F&&>>> make_optional_runtime_impl(F&& func) {
        using T = std::remove_cvref_t<std::invoke_result_t<F&&>>;
        using return_t = std::optional<T>;
        using emplace_t = std::optional<rvo_ctor<T>>;
        static_assert(sizeof(return_t) == sizeof(emplace_t));

        return_t option;
        new (std::addressof(option)) emplace_t(std::in_place, FWD(func));
        return option;
    }

    template<class F>
    constexpr auto make_optional_impl(F&& func) {
        return std::is_constant_evaluated()
            ? std::make_optional(std::invoke(FWD(func)))
            : make_optional_runtime_impl(FWD(func));
    }
}

#endif /* INCLUDE_ITER_EMPLACE_NEW_HPP */

#ifndef INCLUDE_ITER_TUPLE_HPP
#define INCLUDE_ITER_TUPLE_HPP

#include <utility>

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
        template<std::size_t I, class T> T element_type(tuple_element<I, T>*) { static_assert(I == I, "never to be invoked"); }
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

    // Make a tuple with element types exactly the same as those returned from lazy_values
    template<std::invocable<>... Fs>
    tuple<std::invoke_result_t<Fs>...> make_tuple_lazy(Fs&&... lazy_values) {
        static_assert((!std::same_as<void, std::invoke_result_t<Fs>> && ...));
        return {std::invoke(FWD(lazy_values))...};
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

// Implement tuple-like customization points in std namespace
namespace std {
    template<std::size_t I, class... Ts>
    struct tuple_element<I, iter::tuple<Ts...>> {
        static_assert(I < iter::tuple<Ts...>::size(), "Tuple index out of bounds.");
        using type = decltype(iter::detail::element_type<I>(std::declval<iter::tuple<Ts...>*>()));
    };
    template<class... Ts>
    struct tuple_size<iter::tuple<Ts...>> {
        static constexpr std::size_t value = iter::tuple<Ts...>::size();
    };
}

#endif /* INCLUDE_ITER_TUPLE_HPP */

#ifndef ITER_GLOBAL_INVOKER
#  define ITER_INVOKER(name)          XTD_INVOKER(iter_ ## name)
#  define ITER_FUNCTION(fun)          XTD_FUNCTION_(iter_ ## fun) fun
#  define ITER_IMPL(name)             XTD_IMPL_(iter_ ## name, iter::name)
#  define ITER_IMPL_THIS(name)        XTD_IMPL_THIS_(iter_ ## name, iter::name)
#  define ITER_DETAIL_IMPL(name)      XTD_IMPL_(iter_ ## name, iter::detail::impl::name)
#  define ITER_DETAIL_IMPL_THIS(name) XTD_IMPL_THIS_(iter_ ## name, iter::detail::impl::name)
#else
#  warning Overload resolution is more complex with ITER_GLOBAL_INVOKER. \
           Any failing invocations may return an endless list of candidates.
#  define ITER_INVOKER(name)
#  define ITER_FUNCTION(fun)          XTD_FUNCTION fun
#  define ITER_IMPL(name)             XTD_IMPL(iter::name)
#  define ITER_IMPL_THIS(name)        XTD_IMPL_THIS(iter::name)
#  define ITER_DETAIL_IMPL(name)      XTD_IMPL(iter::detail::impl::name)
#  define ITER_DETAIL_IMPL_THIS(name) XTD_IMPL_THIS(iter::detail::impl::name)
#endif

#define ITER_IMPL_NEXT      ITER_DETAIL_IMPL_THIS(next)
#define ITER_IMPL_NEXT_BACK ITER_DETAIL_IMPL_THIS(next_back)
#define ITER_IMPL_GET       ITER_DETAIL_IMPL_THIS(get)
#define ITER_IMPL_SIZE      ITER_DETAIL_IMPL_THIS(size)

#define ITER_DECLARE(fun) \
    ITER_INVOKER(fun)\
    namespace iter {\
        ITER_FUNCTION(fun);\
    }

#define ITER_ALIAS(alias, ... /*of*/) \
    namespace iter {\
        static constexpr auto& alias = __VA_ARGS__;\
    }

#if defined(__clang__)
#  define ITER_ASSUME(condition) __builtin_assume(!!(condition))
#  define ITER_UNREACHABLE() __builtin_unreachable()
#elif defined(__GNUC__) || defined (__GNUG__)
#  define ITER_ASSUME(condition) do { if(!(condition)) __builtin_unreachable(); } while(0)
#  define ITER_UNREACHABLE() __builtin_unreachable()
#elif defined(_MSC_VER)
#  define ITER_ASSUME(condition) __assume(!!(condition))
#  define ITER_UNREACHABLE() __assume(0)
#else
#  define ITER_ASSUME(condition) do { } while(0)
#  define ITER_UNREACHABLE() do { } while(0)
#endif

ITER_DECLARE(to_iter)
ITER_DECLARE(cycle)

ITER_INVOKER(next)
ITER_INVOKER(next_back)
ITER_INVOKER(get)
ITER_INVOKER(size)

namespace iter {
    namespace detail::impl {
        // basic iter customisation points
        ITER_FUNCTION(next);
        ITER_FUNCTION(next_back);
        ITER_FUNCTION(get);
        ITER_FUNCTION(size);
    }

    namespace concepts {
        template<class T>
        concept pointer = std::is_pointer_v<T>;
        template<class T>
        static constexpr bool is_optional = false;
        template<class T>
        constexpr bool is_optional<std::optional<T>> = true;
        template<class T>
        concept optional = is_optional<std::remove_cvref_t<T>>;
    }

    namespace detail {
        template<concepts::optional T>
        auto get_value_t(T) -> typename T::value_type;
        template<concepts::pointer T>
        auto get_value_t(T) -> typename std::iterator_traits<T>::value_type;

        template<class T>
        requires concepts::pointer<T> || concepts::optional<T>
        struct move_next {
            T next = {};
            using value_type = decltype(get_value_t(next));
            constexpr auto&& operator*() {
                return std::move(*next);
            }
            constexpr auto&& operator*() const {
                return std::move(*next);
            }
            constexpr operator bool() const {
                return !!next;
            }
            void reset() requires concepts::optional<T> {
                next.reset();
            }
            constexpr move_next& operator=(std::nullptr_t) requires concepts::pointer<T> {
                next = nullptr;
                return *this;
            }
            constexpr move_next& operator=(std::nullopt_t) requires concepts::optional<T> {
                next.reset();
                return *this;
            }
        };

        template<class T>
        move_next(T) -> move_next<T>;

        template<class T>
        auto get_value_t(move_next<T>) -> typename move_next<T>::value_type;
    }

    using detail::move_next;

    namespace concepts {
        template<class T>
        static constexpr bool is_pointer_next = false;
        template<pointer T>
        constexpr bool is_pointer_next<T> = true;
        template<pointer T>
        constexpr bool is_pointer_next<move_next<T>> = true;
        template<class T>
        concept pointer_next = is_pointer_next<std::remove_cvref_t<T>>;

        template<class T>
        static constexpr bool is_optional_next = false;
        template<optional T>
        constexpr bool is_optional_next<T> = true;
        template<optional T>
        constexpr bool is_optional_next<move_next<T>> = true;
        template<class T>
        concept optional_next = is_optional_next<std::remove_cvref_t<T>>;

        template<class T>
        concept next = pointer_next<T> || optional_next<T>;

        template<class T>
        static constexpr bool is_move_next = false;
        template<class T>
        constexpr bool is_move_next<iter::detail::move_next<T>> = true;
        template<class T>
        concept move_next = is_move_next<std::remove_cvref_t<T>>;

        template<class T>
        concept pointer_iter = requires(T it) {
            { iter::detail::impl::next(it) } -> pointer_next;
        };
        template<class T>
        concept optional_iter = requires(T it) {
            { iter::detail::impl::next(it) } -> optional_next;
        };
        template<class T>
        concept iter = pointer_iter<T> || optional_iter<T>;

        template<class T>
        concept random_access_iter = iter<T> && requires (T it, std::size_t index) {
            iter::detail::impl::get(it, index);
            { iter::detail::impl::size(it) } -> std::same_as<std::size_t>;
        };

        template<class T>
        concept double_ended_iter = iter<T> && requires (T it, std::size_t index) {
            { iter::detail::impl::next_back(it) } -> std::same_as<decltype(iter::detail::impl::next(it))>;
        };

        template<class T>
        concept pointer_iterable = pointer_iter<T> || requires (T&& it) {
            { iter::to_iter(FWD(it)) } -> pointer_iter;
        };
        template<class T>
        concept optional_iterable = optional_iter<T> || requires (T&& it) {
            { iter::to_iter(FWD(it)) } -> optional_iter;
        };
        template<class T>
        concept iterable = iter<T> || pointer_iterable<T> || optional_iterable<T>;

        namespace assert {
            template<class T>
            struct iter {
                static constexpr bool value = concepts::iter<T>;
                static_assert(concepts::iter<T>, "iter constraint not satisfied");
            };
            template<class T>
            struct iterable {
                static constexpr bool value = concepts::iterable<T>;
                static_assert(concepts::iterable<T>, "iterable constraint not satisfied");
            };
        }

        template<class T>
        concept assert_iter = assert::iter<T>::value || iter<T>;
        template<class T>
        concept assert_iterable = assert::iterable<T>::value || iterable<T>;
    }

    using concepts::iter;
    using concepts::iterable;
    using concepts::assert_iter;
    using concepts::assert_iterable;

    namespace detail {
        template<class T>
        struct force_iter;

        template<iterable I>
        struct force_iter<I> {
            using type = std::remove_cvref_t<decltype(iter::to_iter(std::declval<I>()))>;
        };

        template<iter I>
        struct force_iter<I> {
            using type = std::remove_cvref_t<I>;
        };
    }

    template<class I>
    using iter_t = typename detail::force_iter<I>::type;

    namespace concepts {
        template<class T>
        concept random_access_iterable = iterable<T> && random_access_iter<iter_t<T>>;
    }

    template<iterable I>
    using next_t = decltype(iter::detail::impl::next(std::declval<iter_t<I>&>()));

    template<iter I>
    struct iterator_traits {
        using value_type = decltype(detail::get_value_t(std::declval<next_t<I>>()));
        using reference = decltype(*std::declval<next_t<I>&>());
        using pointer = std::remove_reference_t<reference>*;
        using difference_type = std::ptrdiff_t; // only to fulfill ranges concept
        using iterator_category = std::input_iterator_tag;
        using iter_t = I;
    };

    namespace detail {
        template<class T, iter I>
        constexpr T& emplace_next(T& current, I& it) {
            return EMPLACE_NEW(current, impl::next(it));
        }

        static constexpr struct sentinel_t {} sentinel;

        // C++ style iterator_wrapper wrapper (sniff...)
        template<iter I>
        requires (!std::is_const_v<I>)
        struct iterator_wrapper : iterator_traits<I> {
            using traits = iterator_traits<I>;
            using typename traits::value_type;

            explicit constexpr iterator_wrapper(I& it) : it{std::addressof(it)}
            {}
            constexpr iterator_wrapper() = default;

            I* it;
            next_t<I> current;

            auto operator<=>(const iterator_wrapper&) const = delete;

            // This would need to be const to follow std::ranges::range concept,
            // but do we actually need ranges interop?
            // Use the const_cast if desperate for ranges iterop. Atchooo.
            constexpr bool operator!=(sentinel_t) /*const*/ {
                return !!detail::emplace_next(/*const_cast<next_t<I>&>*/current, *it);
            }
            constexpr bool operator==(sentinel_t) /*const*/ {
                return !operator==(sentinel);
            }
            constexpr auto& operator*() {
                return *current;
            }
            constexpr auto& operator*() const {
                return *current;
            }
            constexpr auto operator->() {
                return std::addressof(*current);
            }
            constexpr auto operator->() const {
                return std::addressof(*current);
            }
            constexpr auto& operator++() {
                return *this;
            }
            constexpr void operator++(int) {}
        };

        template<class T>
        iterator_wrapper(T&) -> iterator_wrapper<T>;

        template<iter T>
        constexpr auto begin(T& iter) {
            return detail::iterator_wrapper{iter};
        }

        template<iter T>
        constexpr auto end(T&) {
            return detail::sentinel;
        }
    }

    using detail::begin;
    using detail::end;

    template<class T>
    static constexpr auto&& as_const(T&& in) {
        if constexpr (std::is_lvalue_reference_v<T>)
            return std::as_const(in);
        else
            return (T const&&) in;
    }

    namespace concepts {
        template<class T>
        static constexpr bool is_iterator_v = false;
        template<class I>
        constexpr bool is_iterator_v<detail::iterator_wrapper<I>> = true;

        template<class T>
        concept iterator = is_iterator_v<std::decay_t<T>>;
    }

    namespace detail {
        constexpr auto consume = []<concepts::next N>(N& next) -> auto&& {
            if constexpr (concepts::optional_next<N>)
                return std::move(*next);
            else
                return *next;
        };

        template<class T>
        struct iter_traits;

        template<concepts::iterator I>
        struct iter_traits<I> {
            using value_t = typename I::value_type;
            using ref_t = typename I::reference;
            using cref_t = decltype(iter::as_const(std::declval<ref_t>()));
            using consume_t = decltype(consume(std::declval<next_t<typename I::iter_t>&>()));
        };
        template<iter I>
        struct iter_traits<I> {
            using value_t = typename iterator_traits<I>::value_type;
            using ref_t = typename iterator_traits<I>::reference;
            using cref_t = decltype(iter::as_const(std::declval<ref_t>()));
            using consume_t = decltype(consume(std::declval<next_t<I>&>()));
        };
        template<iterable I>
        requires (!iter<I>)
        struct iter_traits<I> : iter_traits<iter_t<I>> {};
    }

    template<class T>
    using value_t = typename detail::iter_traits<T>::value_t;
    template<class T>
    using ref_t = typename detail::iter_traits<T>::ref_t;
    template<class T>
    using cref_t = typename detail::iter_traits<T>::cref_t;
    template<class T>
    using consume_t = typename detail::iter_traits<T>::consume_t;

    template<iterable I>
    static constexpr next_t<I> no_next() {
        if constexpr(concepts::pointer_iterable<I>)
            return next_t<I>{nullptr};
        else
            return next_t<I>{std::nullopt};
    }

    namespace unsafe {
        template<class I>
        auto get_type() -> void;
        template<concepts::random_access_iter I>
        auto get_type() -> decltype(iter::detail::impl::get(std::declval<I&>(), 0ul));
        template<class I>
        using get_t = decltype(get_type<I>());

        template<class I>
        [[nodiscard]] constexpr auto get_option(I&& iter, std::size_t index) {
            std::size_t size = iter::detail::impl::size(iter);
            using get_t = decltype(iter::detail::impl::get(iter, index));
            if constexpr (std::is_lvalue_reference_v<get_t>)
                return (index < size) ? std::addressof(iter::detail::impl::get(iter, index)) : nullptr;
            else if constexpr (std::is_rvalue_reference_v<get_t>) {
                auto&& item = iter::detail::impl::get(iter, index);
                return move_next{(index < size) ? std::addressof(item) : nullptr};
            } else
                return (index < size) ? MAKE_OPTIONAL(iter::detail::impl::get(iter, index)) : std::nullopt;
        }
    }

    namespace detail {
        template<class Self, class... I>
        struct enable_random_access;

        template<class Self, class I>
        requires (!concepts::random_access_iter<I>)
        struct enable_random_access<Self, I> {
            static constexpr bool random_access = false;

        protected:
            using this_t = enable_random_access;
            using base_t = this_t;
        };

        template<class Self, concepts::random_access_iter I>
        struct enable_random_access<Self, I> {
            static constexpr bool random_access = true;
            std::size_t index = 0;

        protected:
            using this_t = enable_random_access;
            using base_t = this_t;

            constexpr auto ITER_IMPL_SIZE (this_t const& base) {
                return impl::size(static_cast<Self const&>(base).i);
            }
            constexpr auto ITER_IMPL_NEXT (this_t& base) {
                auto index = base.index++;
                auto& self = static_cast<Self&>(base);
                return iter::unsafe::get_option(self, index);
            }
            constexpr auto ITER_IMPL_NEXT_BACK (this_t& base) {
                auto index = base.index++;
                auto& self = static_cast<Self&>(base);
                return iter::unsafe::get_option(self, impl::size(self) - 1 - index);
            }
        };

        template<class Self, class... I>
        requires (sizeof...(I) > 1) && (!concepts::random_access_iter<I> || ...)
        struct enable_random_access<Self, I...> {
            static constexpr bool random_access = false;

        protected:
            using this_t = enable_random_access;
            using base_t = this_t;
        };

        template<class Self, concepts::random_access_iter... I>
        requires (sizeof...(I) > 1)
        struct enable_random_access<Self, I...> {
            static constexpr bool random_access = true;
            std::size_t index = 0;
            std::size_t size = 0;

        protected:
            using this_t = enable_random_access;
            using base_t = this_t;

            constexpr auto ITER_IMPL_SIZE (this_t const& base) {
                return base.size;
            }
            constexpr auto ITER_IMPL_NEXT (this_t& base) {
                auto index = base.index++;
                auto& self = static_cast<Self&>(base);
                return iter::unsafe::get_option(self, index);
            }
            constexpr auto ITER_IMPL_NEXT_BACK (this_t& base) {
                auto index = base.index++;
                auto& self = static_cast<Self&>(base);
                return iter::unsafe::get_option(self, impl::size(self) - 1 - index);
            }
        };
    }

    namespace concepts {
        template<class F, class T>
        concept inspector = requires (F func, T t) {
            { func(t) } -> std::same_as<void>;
        };
    }
}

using iter::begin;
using iter::end;

// Make all iters iterable, since they are trivally convertible to iters
// This fulfills the iter::iterable concept which explicitly subsumes
// the iter::iter concept to simplify overload resolution between iterable and iter.
// Without this specialisation, it would not be safe call iter::to_iter on everything
// matching the iter::iterable concept. In other words: this is tightly coupled with
// the iter::iterable concept -- DO NOT REMOVE.
template<iter::iter I>
constexpr auto ITER_IMPL(to_iter) (I&& iter) -> I&& {
    return FWD(iter);
}

// Define random access functions as deleted by default
template<class... Ts>
void ITER_DETAIL_IMPL(get) (Ts&&...) = delete;
template<class... Ts>
void ITER_DETAIL_IMPL(size) (Ts&&...) = delete;

#endif /* INCLUDE_ITER_CORE_HPP */

// Iterables
#ifndef INCLUDE_ITER_TO_ITER_HPP
#define INCLUDE_ITER_TO_ITER_HPP

#ifndef INCLUDE_ITER_STD_FWD_HPP
#define INCLUDE_ITER_STD_FWD_HPP

namespace std {
    template<class T, std::size_t N>
    class array;

    template<class T, class A>
    class vector;

    template<class, class, class, class>
    class map;
}

#endif /* INCLUDE_ITER_STD_FWD_HPP */

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

        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index) -> auto& {
            return (*self.container)[index];
        }

        constexpr auto ITER_IMPL_SIZE (this_t const& self) {
            return std::size(*self.container);
        }

        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            return self.pos != std::size(*self.container)
                ? std::addressof((*self.container)[self.pos++])
                : nullptr;
        }

        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
            auto const size = std::size(*self.container);
            return self.pos != size
                ? std::addressof((*self.container)[(size - 1 - self.pos++)])
                : nullptr;
        }

        struct cycle;

        constexpr auto ITER_IMPL_THIS(cycle) (this_t&& self) {
            return cycle{FWD(self)};
        }
    };

    template<class T>
    random_access_container_iter(T&) -> random_access_container_iter<T>;

    template<class T>
    struct random_access_container_iter<T>::cycle : random_access_container_iter<T> {
        using this_t = cycle;

        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index) -> auto& {
            const auto size = std::size(*self.container);
            return (*self.container)[index % size];
        }

        constexpr auto ITER_IMPL_SIZE (this_t const&) {
            return std::numeric_limits<std::size_t>::max();
        }

        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            self.pos = self.pos == std::size(*self.container) ? 0 : self.pos;
            return std::addressof((*self.container)[self.pos++]);
        }

        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
            const auto size = std::size(*self.container);
            self.pos = self.pos == size ? 0 : self.pos;
            return std::addressof((*self.container)[(size - 1 - self.pos++)]);
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

#ifndef INCLUDE_ITER_ONCE_HPP
#define INCLUDE_ITER_ONCE_HPP

#ifndef INCLUDE_ITER_REPEAT_HPP
#define INCLUDE_ITER_REPEAT_HPP

namespace iter {
    template<class T>
    struct repeat {
        using this_t = repeat;
        T value;
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            return std::addressof(std::as_const(self.value));
        }
        constexpr auto ITER_IMPL_SIZE (this_t const&) {
            return std::numeric_limits<std::size_t>::max();
        }
        constexpr auto ITER_IMPL_GET (this_t& self, size_t) -> auto& {
            return std::as_const(self.value);
        }
    };

    template<class T>
    repeat(T) -> repeat<T>;
}

#endif /* INCLUDE_ITER_REPEAT_HPP */

namespace iter {
    template<class T>
    struct once {
        using this_t = once;
        [[no_unique_address]] T value;
        bool on = true;
        constexpr std::size_t ITER_IMPL_SIZE (this_t const&) {
            return 1;
        }
        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t) {
            return (self.value);
        }
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            return self.on ? (self.on = false, std::addressof(self.value)) : nullptr;
        }
        constexpr auto ITER_IMPL_THIS(cycle) (this_t const& self) {
            return repeat{self.value};
        }
        constexpr auto ITER_IMPL_THIS(cycle) (this_t&& self) {
            return repeat{std::move(self.value)};
        }
    };

    template<class T>
    once(T) -> once<T>;

    template<class T>
    struct once_ref {
        using this_t = once_ref;
        constexpr once_ref(T& in) : value{&in} {}
    private:
        T* value;
        constexpr std::size_t ITER_IMPL_SIZE (this_t const&) {
            return 1;
        }
        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t) {
            return *self.value;
        }
        constexpr decltype(auto) ITER_IMPL_NEXT (this_t& self) {
            return std::exchange(self.value, nullptr);
        }
        constexpr auto ITER_IMPL_THIS(cycle) (const this_t& self) {
            return repeat<T const&>{*self.value};
        }
    };

    template<class F>
    once_ref(F&) -> once_ref<F>;
}

#endif /* INCLUDE_ITER_ONCE_HPP */

#ifndef INCLUDE_ITER_OPTIONAL_HPP
#define INCLUDE_ITER_OPTIONAL_HPP

namespace iter {
    template<class T>
    struct optional : std::optional<T> {
        using this_t = optional;

        constexpr optional(auto&&... args) : std::optional<T>(FWD(args)...) {}

        constexpr std::size_t ITER_IMPL_SIZE (this_t const& self) {
            return self ? 1 : 0;
        }
        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t) {
            return *self;
        }
        constexpr auto ITER_IMPL_NEXT (this_t& self) -> std::optional<T> {
            std::optional<T> result(std::move(self));
            self.reset();
            return result;
        }
        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
            return detail::impl::next_back(self);
        }
    };

    template<class T>
    optional(T) -> optional<T>;

    namespace concepts {
        template<class T>
        static constexpr bool is_iter_of_optional = false;
        template<class T>
        constexpr bool is_iter_of_optional<iter::optional<T>> = true;
        template<class T>
        concept iter_of_optional = is_iter_of_optional<T>;
    }
}

#endif /* INCLUDE_ITER_OPTIONAL_HPP */

#ifndef INCLUDE_ITER_RANGE_HPP
#define INCLUDE_ITER_RANGE_HPP

ITER_DECLARE(until)
ITER_ALIAS(til, until)

namespace iter {
    template<std::integral T = int, bool Inclusive = false>
    struct [[nodiscard]] range {
        using this_t = range;
        T begin_;
        T end_ = std::numeric_limits<T>::max() - Inclusive;
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            if constexpr (Inclusive)
                return self.begin_ <= self.end_ ? std::optional(self.begin_++) : std::nullopt;
            else
                return self.begin_ < self.end_ ? std::optional(self.begin_++) : std::nullopt;
        }
        constexpr std::size_t ITER_IMPL_SIZE (this_t const& self) {
            if constexpr (Inclusive)
                return 1ul + self.end_ - self.begin_;
            else
                return self.end_ - self.begin_;
        }
        constexpr T ITER_IMPL_GET (this_t& self, std::size_t index) {
            return self.begin_ + index;
        }
        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
            if constexpr (Inclusive)
                return self.begin_ <= self.end_ ? std::optional(self.end_--) : std::nullopt;
            else
                return self.begin_ < self.end_ ? std::optional(--self.end_) : std::nullopt;
        }
    };

    template<class T>
    range(T) -> range<T>;
    template<class T>
    range(T, T) -> range<T>;

    template<class T>
    struct inclusive_range : range<T, true> {};

    template<class T>
    inclusive_range(T) -> inclusive_range<T>;
    template<class T>
    inclusive_range(T, T) -> inclusive_range<T>;

    namespace detail {
        template<std::integral T = std::size_t>
        struct [[nodiscard]] indices_iter {
            using this_t = indices_iter;
            indices_iter() = default;
        private:
            T i = 0;
            constexpr auto ITER_IMPL_NEXT (this_t& self) {
                return std::optional(self.i++);
            }
            constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
                return std::optional(std::numeric_limits<T>::max() - self.i++);
            }
            constexpr std::size_t ITER_IMPL_SIZE (this_t const&) {
                return std::numeric_limits<T>::max();
            }
            constexpr T ITER_IMPL_GET (this_t&, std::size_t index) {
                return index;
            }
        };

        template<std::integral T = std::size_t>
        struct indices_tag {};
        template<class T>
        constexpr auto ITER_IMPL(to_iter) (detail::indices_tag<T>) {
            return detail::indices_iter<T>{};
        }
    }

    template<class T = std::size_t>
    static constexpr detail::indices_tag<T> indices_ = {};
    static constexpr auto indices = indices_<>;

    template<class T>
    constexpr auto ITER_IMPL(to_iter) (detail::indices_tag<T>) {
        return detail::indices_iter<T>{};
    }
}

template<std::integral T>
constexpr auto ITER_IMPL(until) (T begin, T end) {
    return iter::range{begin, end};
}

#endif /* INCLUDE_ITER_RANGE_HPP */

#ifndef INCLUDE_ITER_GENERATE_HPP
#define INCLUDE_ITER_GENERATE_HPP

namespace iter {
    template<std::invocable<> F>
    requires concepts::optional_next<std::invoke_result_t<F>>
          || concepts::pointer_next<std::invoke_result_t<F>>
    struct generate : F {
        using this_t = generate;
        constexpr decltype(auto) ITER_IMPL_NEXT (this_t& self) {
            return self();
        }
    };

    template<class F>
    generate(F) -> generate<F>;
}

#endif /* INCLUDE_ITER_GENERATE_HPP */

#if __cpp_impl_coroutine >= 201902L && !defined(INCLUDE_ITER_GENERATOR_HPP)
#define INCLUDE_ITER_GENERATOR_HPP

#include <coroutine>

namespace iter {
    template<class T>
    class generator;

    namespace concepts {
        namespace detail {
            template<class T>
            static constexpr bool is_generator = false;
            template<class T>
            constexpr bool is_generator<generator<T>> = true;
        }
        template<class T>
        concept generator = detail::is_generator<T>;
    }

    namespace detail {
        template<class T>
        struct generator_promise {
            using value_type = std::remove_reference_t<T>;
            using reference_type = std::conditional_t<std::is_reference_v<T>, T, T&>;
            using pointer_type = value_type*;

            generator_promise() = default;

            generator<T> get_return_object() noexcept;

            constexpr std::suspend_always initial_suspend() const noexcept { return {}; }
            constexpr std::suspend_always final_suspend() const noexcept { return {}; }

            constexpr std::suspend_always yield_value(value_type& value) noexcept
                requires (!std::is_rvalue_reference_v<T>)
            {
                m_value = std::addressof(value);
                return {};
            }

            constexpr std::suspend_always yield_value(value_type&& value) noexcept {
                m_value = std::addressof(value);
                return {};
            }

            constexpr void unhandled_exception() {
                m_exception = std::current_exception();
            }

            constexpr void return_void() {}

            constexpr pointer_type value() const noexcept {
                return m_value;
            }

            // Disallow co_await
            template<class U>
            std::suspend_never await_transform(U&& value) = delete;

            void rethrow_if_exception() {
                if (m_exception)
                    std::rethrow_exception(m_exception);
            }

        private:
            pointer_type m_value;
            std::exception_ptr m_exception;
        };
    }

    template<class T = void_t>
    struct [[nodiscard]] generator {
        using promise_type = detail::generator_promise<T>;

        constexpr generator() noexcept
            : m_coroutine(nullptr)
        {}

        constexpr generator(const generator& other) = delete;
        constexpr generator(generator&& other) noexcept
            : m_coroutine(std::exchange(other.m_coroutine, nullptr))
        {}

        constexpr generator& operator=(generator&& other) noexcept {
            if (m_coroutine)
                m_coroutine.destroy();
            m_coroutine = std::exchange(other.m_coroutine, nullptr);
            return *this;
        }

        constexpr ~generator() {
            if (m_coroutine)
                m_coroutine.destroy();
        }

    private:
        using this_t = generator;
        constexpr auto ITER_IMPL_NEXT (this_t& self) { return self.next(); }

        constexpr T* next() {
            if (!m_coroutine) [[unlikely]]
                return nullptr;

            m_coroutine.resume();
            if (m_coroutine.done()) [[unlikely]] {
                m_coroutine.promise().rethrow_if_exception();
                return nullptr;
            }

            return m_coroutine.promise().value();
        }

        friend class detail::generator_promise<T>;

        explicit generator(std::coroutine_handle<promise_type> coroutine) noexcept
            : m_coroutine(coroutine)
        {}

        std::coroutine_handle<promise_type> m_coroutine;
    };

    template<class T>
    generator<T> detail::generator_promise<T>::get_return_object() noexcept {
        using coroutine_handle = std::coroutine_handle<generator_promise<T>>;
        return generator<T>{coroutine_handle::from_promise(*this)};
    }

    template<class... Ts, std::invocable<Ts&...> F>
    requires concepts::generator<std::invoke_result_t<F, Ts&...>>
    constexpr auto ITER_IMPL(cycle) (F&& make_iter, Ts&&... args) {
        // "Capture" args by value with an inner coroutine taking them by value
        // The outer function takes by universal reference to observe constness
        return [](auto make_iter, auto... args) -> std::invoke_result_t<F, Ts&...> {
            while (true)
                for (auto it = std::invoke(make_iter, static_cast<Ts&>(args)...); auto next = iter::detail::impl::next(it);)
                    co_yield *next;
        }(FWD(make_iter), FWD(args)...);
    }
    template<std::invocable<> F>
    requires concepts::generator<std::invoke_result_t<F>>
    constexpr auto ITER_IMPL(cycle) (F&& make_iter) {
        return [](auto make_iter) -> std::invoke_result_t<F> {
            while (true)
                for (auto it = std::invoke(make_iter); auto next = iter::detail::impl::next(it);)
                    co_yield *next;
        }(FWD(make_iter));
    }
}

#endif /* INCLUDE_ITER_GENERATOR_HPP */

#ifndef INCLUDE_ITER_COMPOUND_HPP
#define INCLUDE_ITER_COMPOUND_HPP

namespace iter {
    template<class A, class F>
    requires std::constructible_from<std::optional<A>, std::invoke_result_t<F, A const&>>
    struct compound {
        std::optional<A> value;
        [[no_unique_address]] F func;

        using this_t = compound;
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            auto result = std::move(self.value);
            if (result) {
                EMPLACE_NEW(self.value, self.func(iter::as_const(*result)));
            }
            return result;
        }
    };

    template<class A, class F>
    compound(A, F) -> compound<A, F>;
}

#endif /* INCLUDE_ITER_COMPOUND_HPP */

#ifndef INCLUDE_ITER_EMPTY_HPP
#define INCLUDE_ITER_EMPTY_HPP

namespace iter {
    namespace detail {
        template<class T>
        struct empty_iter {
            using this_t = empty_iter;
            constexpr auto ITER_IMPL_NEXT (this_t&) -> T* {
                return nullptr;
            }
            constexpr std::size_t ITER_IMPL_SIZE (this_t const&) {
                return 0;
            }
            constexpr auto ITER_IMPL_GET (this_t&, std::size_t) -> T& {
                ITER_UNREACHABLE();
                return reinterpret_cast<T&>(*((T*)0));
            }
        };
    }

    template<class T>
    detail::empty_iter<T> empty = {};
}

#endif /* INCLUDE_ITER_EMPTY_HPP */

// Adaptors
#ifndef INCLUDE_ITER_FILTER_HPP
#define INCLUDE_ITER_FILTER_HPP

ITER_DECLARE(filter)

namespace iter::detail {
    template<assert_iter I, std::predicate<ref_t<I>> P>
    struct [[nodiscard]] filter_iter {
        using this_t = filter_iter;

        [[no_unique_address]] I i;
        [[no_unique_address]] P pred;

        constexpr next_t<I> ITER_IMPL_NEXT (this_t& self) {
            auto val = no_next<I>();
            while (emplace_next(val, self.i)) [[unlikely]] {
                if (self.pred(*val)) {
                    return val;
                }
            }

            return val;
        }
    };

    template<class I, class P>
    filter_iter(I, P) -> filter_iter<I, P>;
}

template<iter::assert_iterable I, std::predicate<iter::ref_t<I>> P>
constexpr auto ITER_IMPL(filter) (I&& iterable, P&& pred) {
    return iter::detail::filter_iter<iter::iter_t<I>, std::remove_cvref_t<P>>{.i = iter::to_iter(FWD(iterable)), .pred = FWD(pred)};
}

#endif /* INCLUDE_ITER_FILTER_HPP */

#ifndef INCLUDE_ITER_TAKE_HPP
#define INCLUDE_ITER_TAKE_HPP

ITER_DECLARE(take)

namespace iter::detail {
    template<assert_iter I>
    struct take_iter : enable_random_access<take_iter<I>, I> {
        [[no_unique_address]] I i;
        std::size_t n;

        using this_t = take_iter;

        constexpr auto ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            return self.n-- > 0 ? impl::next(self.i) : no_next<I>();
        }

        constexpr auto ITER_IMPL_SIZE (this_t const& self)
            requires this_t::random_access
        {
            return std::min(self.n, impl::size(self.i));
        }

        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return impl::get(self.i, index);
        }
    };

    template<class I>
    take_iter(I, std::size_t) -> take_iter<I>;
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(take) (I&& iterable, std::size_t n) {
    return iter::detail::take_iter<iter::iter_t<I>>{.i = iter::to_iter(FWD(iterable)), .n = n};
}

#endif /* INCLUDE_ITER_TAKE_HPP */

#ifndef INCLUDE_ITER_TAKE_WHILE_HPP
#define INCLUDE_ITER_TAKE_WHILE_HPP

ITER_DECLARE(take_while)

namespace iter::detail {
    template<assert_iter I, std::predicate<ref_t<I>> P>
    struct take_while_iter {
        using this_t = take_while_iter;

        [[no_unique_address]] I i;
        [[no_unique_address]] P pred;

        constexpr next_t<I> ITER_IMPL_NEXT (this_t& self) {
            auto val = impl::next(self.i);
            return val && self.pred(*val) ? val : no_next<I>(); // TODO test RVO
        }
    };

    template<class I, class P>
    take_while_iter(I, P) -> take_while_iter<I, P>;
}

template<iter::assert_iterable I, std::predicate<iter::ref_t<I>> P>
constexpr auto ITER_IMPL(take_while) (I&& iterable, P&& predicate) {
    return iter::detail::take_while_iter<iter::iter_t<I>, std::remove_cvref_t<P>>{.i = iter::to_iter(FWD(iterable)), .pred = FWD(predicate)};
}

#endif /* INCLUDE_ITER_TAKE_WHILE_HPP */

#ifndef INCLUDE_ITER_SKIP_HPP
#define INCLUDE_ITER_SKIP_HPP

ITER_DECLARE(skip)

namespace iter::detail {
    template<assert_iter I>
    struct skip_iter : enable_random_access<skip_iter<I>, I> {
        [[no_unique_address]] I i;
        std::size_t n;

        using this_t = skip_iter;
        constexpr auto ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            auto next = no_next<I>();
            while (emplace_next(next, self.i)) {
                if (self.n) [[unlikely]] {
                    --self.n;
                    continue;
                }
                return next;
            }
            return next;
        }

        constexpr auto ITER_IMPL_SIZE (this_t const& self)
            requires this_t::random_access
        {
            std::size_t size = impl::size(self.i);
            return size > self.n ? size - self.n : 0;
        }

        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return impl::get(self.i, index + self.n);
        }
    };

    template<class I>
    skip_iter(I, std::size_t) -> skip_iter<I>;
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(skip) (I&& iterable, std::size_t n) {
    return iter::detail::skip_iter<iter::iter_t<I>>{.i = iter::to_iter(FWD(iterable)), .n = n};
}

#endif /* INCLUDE_ITER_SKIP_HPP */

#ifndef INCLUDE_ITER_SKIP_WHILE_HPP
#define INCLUDE_ITER_SKIP_WHILE_HPP

ITER_DECLARE(skip_while)

namespace iter::detail {
    template<assert_iter I, std::predicate<cref_t<I>> P>
    struct skip_while_iter {
        using this_t = skip_while_iter;

        [[no_unique_address]] I i;
        std::optional<P> pred;

        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            auto next = no_next<I>();
            while (emplace_next(next, self.i)) {
                if (self.pred) [[unlikely]] {
                    if (std::invoke(*self.pred, as_const(*next))) {
                        continue;
                    }
                    self.pred.reset();
                }
                return next;
            }
            return next;
        }
    };

    template<class I, class P>
    skip_while_iter(I, P) -> skip_while_iter<I, P>;
}

template<iter::assert_iterable I, std::predicate<iter::cref_t<I>> P>
constexpr auto ITER_IMPL(skip_while) (I&& iterable, P&& pred) {
    return iter::detail::skip_while_iter<iter::iter_t<I>, std::remove_cvref_t<P>>{.i = iter::to_iter(FWD(iterable)), .pred = FWD(pred)};
}

#endif /* INCLUDE_ITER_SKIP_WHILE_HPP */

#ifndef INCLUDE_ITER_MAP_HPP
#define INCLUDE_ITER_MAP_HPP

#ifndef INCLUDE_ITER_FLATTEN_HPP
#define INCLUDE_ITER_FLATTEN_HPP

#ifndef INCLUDE_ITER_ITER_WRAPPER_HPP
#define INCLUDE_ITER_ITER_WRAPPER_HPP

namespace iter::detail {
    template<class I>
    struct iter_wrapper {
        static_assert(iterable<I&>);
        I iterable;
        using iter_t = iter::iter_t<I&>;
        iter_t iter = to_iter(iterable);
    };
    template<iter I>
    struct iter_wrapper<I> {
        using iter_t = I;
        I iter;
    };
    template<class I>
    iter_wrapper(I) -> iter_wrapper<I>;

    template<class T>
    struct optional_iter_wrapper {
        static_assert(iterable<decltype(get_value_t(std::declval<T>()))&>);
        T optional_iterable;
        using iter_t = iter::iter_t<decltype(*optional_iterable)>;
        std::optional<iter_t> optional_iter = optional_iterable ? MAKE_OPTIONAL(to_iter(*optional_iterable)) : std::nullopt;
    };
    template<class T>
    requires iter<decltype(get_value_t(std::declval<T>()))>
    struct optional_iter_wrapper<T> {
        using iter_t = decltype(get_value_t(std::declval<T>()));
        T optional_iter;
    };
    template<class T>
    optional_iter_wrapper(T) -> optional_iter_wrapper<T>;
}

#endif /* INCLUDE_ITER_ITER_WRAPPER_HPP */

ITER_DECLARE(flatten)

namespace iter::detail {
    template<assert_iter I>
    struct [[nodiscard]] flatten_iter {
        using this_t = flatten_iter;

        constexpr static auto get_current(I& i) {
            return optional_iter_wrapper{impl::next(i)};
        }

        [[no_unique_address]] I i;
        decltype(this_t::get_current(std::declval<I&>())) current{};

        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            using inner_iter_t = typename decltype(current)::iter_t;
            auto val = no_next<inner_iter_t>();
            do {
                if (self.current.optional_iter)
                    if (emplace_next(val, *self.current.optional_iter))
                        return val;
            } while (EMPLACE_NEW(self.current, get_current(self.i)).optional_iter);
            return val;
        }
    };

    template<class I>
    flatten_iter(I) -> flatten_iter<I>;
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(flatten) (I&& iterable) {
    return iter::detail::flatten_iter<iter::iter_t<I>>{.i = iter::to_iter(FWD(iterable))};
}

#endif /* INCLUDE_ITER_FLATTEN_HPP */

#ifndef INCLUDE_ITER_FLATMAP_HPP
#define INCLUDE_ITER_FLATMAP_HPP

ITER_DECLARE(flatmap)
ITER_ALIAS(flat_map, flatmap)

namespace iter::detail {
    template<assert_iter I, std::invocable<consume_t<I>> F>
    struct [[nodiscard]] flatmap_iter {
        using this_t = flatmap_iter;
        using invoke_result = std::invoke_result_t<F, consume_t<I>>;
        static_assert(!concepts::iter_of_optional<invoke_result>,
            "Do not return iter::optional in iter::flatmap, instead return std::optional in iter::filter_map.");
        using wrapped_inner_iter_t = iter_wrapper<invoke_result>;
        using inner_iter_t = typename wrapped_inner_iter_t::iter_t;

        std::optional<wrapped_inner_iter_t> current = std::nullopt;
        [[no_unique_address]] I i;
        [[no_unique_address]] F func;

        constexpr auto get_current() {
            auto next = impl::next(i);
            return next
                ? MAKE_OPTIONAL(iter_wrapper{func(consume(next))})
                : std::nullopt;
        }

        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            auto val = no_next<inner_iter_t>();
            do {
                if (self.current)
                    if (emplace_next(val, self.current->iter))
                        return val;
            } while (EMPLACE_NEW(self.current, self.get_current()));
            return val;
        }
    };

    template<class I, class F>
    flatmap_iter(I, F) -> flatmap_iter<I, F>;
}

template<iter::assert_iterable I, std::invocable<iter::consume_t<I>> F>
constexpr auto ITER_IMPL(flatmap) (I&& iterable, F&& func) {
    return iter::detail::flatmap_iter<iter::iter_t<I>, std::remove_cvref_t<F>>{.i = iter::to_iter(FWD(iterable)), .func = FWD(func)};
}

#ifndef INCLUDE_ITER_FILTER_MAP_HPP
#define INCLUDE_ITER_FILTER_MAP_HPP

ITER_DECLARE(filter_map)

namespace iter::detail {
    template<assert_iter I, std::invocable<consume_t<I>> F>
    struct [[nodiscard]] filter_map_iter {
        using this_t = filter_map_iter;
        using mapped_t = std::invoke_result_t<F, ref_t<I>>;
        static_assert(concepts::optional_next<mapped_t> || concepts::pointer_next<mapped_t>);

        [[no_unique_address]] I i;
        [[no_unique_address]] F func;

        constexpr mapped_t ITER_IMPL_NEXT (this_t& self) {
            auto mapped = mapped_t{};
            while (auto val = impl::next(self.i)) {
                if (EMPLACE_NEW(mapped, self.func(consume(val)))) {
                    return mapped;
                }
            }
            return mapped;
        }
    };

    template<class I, class P>
    filter_map_iter(I, P) -> filter_map_iter<I, P>;
}

template<iter::assert_iterable I, std::invocable<iter::consume_t<I>> F>
constexpr auto ITER_IMPL(filter_map) (I&& iterable, F&& func) {
    return iter::detail::filter_map_iter<iter::iter_t<I>, std::remove_cvref_t<F>>{.i = iter::to_iter(FWD(iterable)), .func = FWD(func)};
}

#endif /* INCLUDE_ITER_FILTER_MAP_HPP */

// flatmap on std::optional is equivalent to the specially optimised filter_map
template<iter::assert_iterable I, std::invocable<iter::consume_t<I>> F>
requires iter::concepts::optional_next<std::invoke_result_t<F, iter::consume_t<I>>>
      || iter::concepts::pointer_next<std::invoke_result_t<F, iter::consume_t<I>>>
constexpr auto ITER_IMPL(flatmap) (I&& iterable, F&& func) {
    return iter::filter_map(FWD(iterable), FWD(func));
}

#endif /* INCLUDE_ITER_FLATMAP_HPP */

ITER_DECLARE(map)

namespace iter::detail {
    template<assert_iter I, std::invocable<consume_t<I>> F>
    struct [[nodiscard]] map_iter : enable_random_access<map_iter<I, F>, I> {
        [[no_unique_address]] I i;
        [[no_unique_address]] F func;

    private:
        using this_t = map_iter;
        using result_t = std::invoke_result_t<F, consume_t<I>>;
        using mapped_t = std::conditional_t<std::is_reference_v<result_t>, std::remove_reference_t<result_t>*, std::optional<result_t>>;

        constexpr mapped_t ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            auto val = impl::next(self.i);
            if constexpr (concepts::optional<mapped_t>)
                return val ? MAKE_OPTIONAL(self.func(consume(val))) : std::nullopt;
            else
                return val ? std::addressof(self.func(consume(val))) : nullptr;
        }

        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return self.func(impl::get(self.i, index));
        }

        constexpr auto ITER_IMPL_THIS(flatten) (this_t&& self) {
            return iter::flatmap(std::move(self.i), std::move(self.func));
        }
        constexpr auto ITER_IMPL_THIS(flatten) (this_t const& self) {
            return iter::flatmap(self.i, self.func);
        }
    };

    template<class I, class F>
    map_iter(I, F) -> map_iter<I, F>;
}

template<iter::assert_iterable I, std::invocable<iter::consume_t<I>> F>
constexpr auto ITER_IMPL(map) (I&& iterable, F&& func) {
    return iter::detail::map_iter<iter::iter_t<I>, std::remove_cvref_t<F>>{.i = iter::to_iter(FWD(iterable)), .func = FWD(func)};
}

#endif /* INCLUDE_ITER_MAP_HPP */

#ifndef INCLUDE_ITER_MAP_WHILE_HPP
#define INCLUDE_ITER_MAP_WHILE_HPP

ITER_DECLARE(map_while)

namespace iter::detail {
    template<assert_iter I, std::invocable<consume_t<I>> F>
    struct [[nodiscard]] map_while_iter {
        using this_t = map_while_iter;
        using mapped_t = std::invoke_result_t<F, consume_t<I>>;
        static_assert(concepts::optional_next<mapped_t> || concepts::pointer_next<mapped_t>);

        [[no_unique_address]] I i;
        [[no_unique_address]] F func;

        constexpr mapped_t ITER_IMPL_NEXT (this_t& self) {
            auto val = impl::next(self.i);
            return val ? self.func(consume(val)) : mapped_t{};
        }
    };

    template<class I, class P>
    map_while_iter(I, P) -> map_while_iter<I, P>;
}

template<iter::assert_iterable I, std::invocable<iter::consume_t<I>> F>
constexpr auto ITER_IMPL(map_while) (I&& iterable, F&& func) {
    return iter::detail::map_while_iter<iter::iter_t<I>, std::remove_cvref_t<F>>{.i = iter::to_iter(FWD(iterable)), .func = FWD(func)};
}

#endif /* INCLUDE_ITER_MAP_WHILE_HPP */

#ifndef INCLUDE_ITER_ZIP_HPP
#define INCLUDE_ITER_ZIP_HPP

ITER_DECLARE(zip)

namespace iter::detail {
    // Simply dereference pointers to avoid copy/move construction
    // but unwrap optionals into new instances
    template<class T>
    static constexpr auto lazy_unwrap_next(T&& in) {
        using t = std::decay_t<T>;
        if constexpr (concepts::optional_next<t>)
            return [&] { return std::move(*in); };
        else
            return [&]() -> auto&& { return *in; };
    }

    template<assert_iter... I>
    requires (sizeof...(I) > 1)
    struct [[nodiscard]] zip_iter : enable_random_access<zip_iter<I...>, I...> {
        using this_t = zip_iter;

        [[no_unique_address]] tuple<I...> i;

        constexpr auto ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            return apply([](auto&... is) {
                return [](auto&&... vals) {
                    return (... && vals)
                        ? MAKE_OPTIONAL(make_tuple_lazy(lazy_unwrap_next(FWD(vals))...))
                        : std::nullopt;
                }(impl::next(is)...);
            }, self.i);
        }

        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return apply([=](auto&... is) {
                return make_tuple_lazy([&, index]() -> decltype(auto) { return impl::get(is, index); }...);
            }, self.i);
        }
    };

    template<class T> static constexpr bool is_zip = false;
    template<class... Ts> constexpr bool is_zip<zip_iter<Ts...>> = true;
    template<class T>
    concept decays_to_zip = is_zip<std::remove_cvref_t<T>>;
}

template<iter::assert_iterable... I>
constexpr auto ITER_IMPL(zip) (I&&... iterables) {
    auto zip = iter::detail::zip_iter<iter::iter_t<I>...>{.i = {iter::to_iter(FWD(iterables))...}};
    if constexpr(decltype(zip)::random_access) {
        zip.size = apply([](auto&... iters) {
            return std::min({iter::detail::impl::size(iters)...});
        }, zip.i);
    }
    return zip;
}

template<iter::detail::decays_to_zip I, iter::assert_iterable... Is>
constexpr auto ITER_IMPL(zip) (I&& zip_iter, Is&&... iterables) {
    return apply([&](auto&&... zip_iters) {
        return iter::zip(FWD(zip_iters)..., FWD(iterables)...);
    }, FWD(zip_iter).i);
}

#endif /* INCLUDE_ITER_ZIP_HPP */

#ifndef INCLUDE_ITER_ENUMERATE_HPP
#define INCLUDE_ITER_ENUMERATE_HPP

XTD_INVOKER(iter_enumerate)

namespace iter {
    namespace tag {
        template<class T = std::size_t>
        struct enumerate_ : xtd::tagged_bindable<enumerate_<T>, xtd::invokers::iter_enumerate> {};
    }

    template<class T = std::size_t>
    static constexpr tag::enumerate_<T> enumerate_;
}

ITER_ALIAS(enumerate, enumerate_<>)

template<class T, iter::assert_iterable I>
constexpr decltype(auto) XTD_IMPL_TAG_(iter_enumerate, iter::tag::enumerate_<T>) (I&& iterable) {
    return iter::zip(FWD(iterable), iter::indices_<T>);
}

#endif /* INCLUDE_ITER_ENUMERATE_HPP */

#ifndef INCLUDE_ITER_CYCLE_HPP
#define INCLUDE_ITER_CYCLE_HPP

namespace iter::detail {
    template<assert_iter I>
    struct cycle_iter : enable_random_access<cycle_iter<I>, I> {
        using this_t = cycle_iter;
        [[no_unique_address]] I i;
        [[no_unique_address]] std::conditional_t<this_t::random_access, void_t, I> i_orig = i;

    private:
        constexpr std::size_t ITER_IMPL_SIZE (this_t const& self)
            requires this_t::random_access
        {
            return std::numeric_limits<std::size_t>::max();
        }

        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return impl::get(self.i, index % impl::size(self.i));
        }

        constexpr next_t<I> ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            auto val = impl::next(self.i);
            if (val) [[likely]] {
                return val;
            }
            // assume we are at the end of the underlying, so reinitialise
            EMPLACE_NEW(self.i, self.i_orig);
            emplace_next(val, self.i);
            return val; // if first value is empty, then this prevents an infinite loop
        }
    };

    template<class T>
    cycle_iter(T) -> cycle_iter<T>;
}

template<iter::iter I>
constexpr auto ITER_IMPL(cycle) (I&& iter) {
    return iter::detail::cycle_iter<std::remove_cvref_t<I>>{.i = FWD(iter)};
}

template<class I>
constexpr auto ITER_IMPL(cycle) (I&& iterable) {
    static_assert(iter::iterable<I>);
    return iter::cycle(iter::to_iter(FWD(iterable)));
}

#endif /* INCLUDE_ITER_CYCLE_HPP */

#ifndef INCLUDE_ITER_REVERSE_HPP
#define INCLUDE_ITER_REVERSE_HPP

ITER_DECLARE(reverse)

namespace iter::detail {
    template<assert_iter I>
    struct reverse_iter : enable_random_access<reverse_iter<I>, I> {
        static_assert(concepts::double_ended_iter<I>);
        [[no_unique_address]] I i;

        using this_t = reverse_iter;
        constexpr auto ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            return impl::next_back(self.i);
        }
        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self)
            requires (!this_t::random_access)
        {
            return impl::next(self.i);
        }

        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return impl::get(self.i, impl::size(self.i) - index - 1);
        }

        constexpr auto ITER_IMPL_THIS(reverse) (this_t&& self) { return std::move(self.i); }
        constexpr auto ITER_IMPL_THIS(reverse) (this_t const& self) { return self.i; }
    };
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(reverse) (I&& iterable) {
    static_assert(iter::concepts::double_ended_iter<iter::iter_t<I>>);
    return iter::detail::reverse_iter<iter::iter_t<I>>{.i = iter::to_iter(FWD(iterable))};
}

#endif /* INCLUDE_ITER_REVERSE_HPP */

#ifndef INCLUDE_ITER_CHAIN_HPP
#define INCLUDE_ITER_CHAIN_HPP

ITER_DECLARE(chain)

namespace iter::detail {
    template<assert_iter I1, assert_iter I2>
    struct [[nodiscard]] chain_iter : enable_random_access<chain_iter<I1, I2>, I1, I2> {
        static_assert(std::same_as<value_t<I1>, value_t<I2>>);

        std::optional<I1> i1;
        [[no_unique_address]] I2 i2;

    private:
        using this_t = chain_iter;

        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            std::size_t i1s = impl::size(*self.i1);
            return index < i1s ? impl::get(*self.i1, index) : impl::get(self.i2, index - i1s);
        }

        static constexpr bool optional_next = concepts::optional_iter<I1> || concepts::optional_iter<I2>;
        using next_t = std::conditional_t<optional_next, std::optional<value_t<I1>>, value_t<I1>*>;

        constexpr next_t ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            auto val = next_t{};
            if (self.i1) {
                if constexpr (optional_next && !concepts::optional_iter<I1>) {
                    if (auto pval = impl::next(*self.i1)) {
                        val.emplace(*pval);
                        return val;
                    }
                } else {
                    if (emplace_next(val, *self.i1))
                        return val;
                }
                // If we haven't returned by this point, we reached the end of I1
                self.i1.reset();
            }
            if constexpr (optional_next && !concepts::optional_iter<I2>) {
                if (auto pval = impl::next(self.i2))
                    val.emplace(*pval);
            } else {
                emplace_next(val, self.i2);
            }
            return val;
        }
    };

    template<class I1, class I2>
    chain_iter(std::optional<I1>, I2) -> chain_iter<I1, I2>;
}

template<iter::assert_iterable I1, iter::assert_iterable I2>
constexpr auto ITER_IMPL(chain) (I1&& iterable1, I2&& iterable2) {
    using chain_t = iter::detail::chain_iter<iter::iter_t<I1>, iter::iter_t<I2>>;
    if constexpr (chain_t::random_access) {
        auto chain = chain_t{.i1 = MAKE_OPTIONAL(iter::to_iter(FWD(iterable1))), .i2 = iter::to_iter(FWD(iterable2))};
        chain.size = iter::detail::impl::size(*chain.i1) + iter::detail::impl::size(chain.i2);
        return chain;
    } else {
        return chain_t{.i1 = MAKE_OPTIONAL(iter::to_iter(FWD(iterable1))), .i2 = iter::to_iter(FWD(iterable2))};
    }
}

#endif /* INCLUDE_ITER_CHAIN_HPP */

#ifndef INCLUDE_ITER_CHUNKS_HPP
#define INCLUDE_ITER_CHUNKS_HPP

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
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            std::size_t n = 0;
            while (auto next = impl::next(self.i)) {
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
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
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
        constexpr auto ITER_IMPL_NEXT (this_t& self) -> lazy_chunk_iter<I>* {
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
    return iter::detail::chunks_iter<std::remove_reference_t<I>, N>{{}, FWD(iterable)};
}

#endif /* INCLUDE_ITER_CHUNKS_HPP */

#ifndef INCLUDE_ITER_WINDOW_HPP
#define INCLUDE_ITER_WINDOW_HPP

XTD_INVOKER(iter_window)

namespace iter {
    namespace tag {
        template<std::size_t N>
        struct window : xtd::tagged_bindable<window<N>, xtd::invokers::iter_window> {};
    }
    template<std::size_t N = 2>
    static constexpr tag::window<N> window;
}

namespace iter::detail {
    template<class T, std::size_t N>
    struct window_iter_storage {
        std::array<T, N> buffer = {};
        std::size_t size = 0;
        std::size_t end = 0;
        constexpr auto to_iter() {
            using namespace xtd::literals;
            return cycle(buffer) | skip(_, end) | take(_, size--);
        }
    };

    template<assert_iter I, std::size_t N>
    struct [[nodiscard]] window_iter : window_iter_storage<value_t<I>, N> {
        static_assert(N > 1, "Window must be of at least size 2");
        [[no_unique_address]] I i;

        using this_t = window_iter;
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            while (self.size < N) [[likely]] {
                if (auto next = impl::next(self.i)) [[likely]] {
                    self.buffer[self.end] = consume(next);
                    ++self.size;
                    self.end = (self.end + 1) % N;
                } else break;
            }
            return self.size == N ? MAKE_OPTIONAL(self.to_iter()) : std::nullopt;
        }
    };
}

template<std::size_t N, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_window, iter::tag::window<N>) (I&& iterable) {
    return iter::detail::window_iter<std::remove_reference_t<I>, N>{{}, {FWD(iterable)}};
}

#endif /* INCLUDE_ITER_WINDOW_HPP */

#ifndef INCLUDE_ITER_INSPECT_HPP
#define INCLUDE_ITER_INSPECT_HPP

ITER_DECLARE(inspect)

namespace iter::detail {
    template<assert_iter I, concepts::inspector<ref_t<I>> F>
    struct [[nodiscard]] inspect_iter : enable_random_access<inspect_iter<I, F>, I> {
        [[no_unique_address]] I i;
        [[no_unique_address]] F func;

    private:
        using this_t = inspect_iter;
        constexpr next_t<I> ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            auto val = impl::next(self.i);
            if (val) self.func(*val);
            return val;
        }

        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            decltype(auto) val = impl::get(self.i, index);
            self.func(val);
            return val;
        }
    };

    template<class I, class F>
    inspect_iter(I, F) -> inspect_iter<I, F>;
}

template<iter::assert_iterable I, iter::concepts::inspector<iter::ref_t<I>> F>
constexpr auto ITER_IMPL(inspect) (I&& iterable, F func) {
    return iter::detail::inspect_iter<iter::iter_t<I>, std::remove_cvref_t<F>>{.i = iter::to_iter(FWD(iterable)), .func = std::move(func)};
}

#endif /* INCLUDE_ITER_INSPECT_HPP */

#ifndef INCLUDE_ITER_TO_POINTER_ITER_HPP
#define INCLUDE_ITER_TO_POINTER_ITER_HPP

ITER_DECLARE(to_pointer_iter)

namespace iter::detail {
    template<concepts::optional_iter I>
    struct [[nodiscard]] to_pointer_iter {
        using this_t = to_pointer_iter;

        [[no_unique_address]] I i;
        next_t<I> store = std::nullopt;

        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            emplace_next(self.store, self.i);
            return self.store ? std::addressof(*self.store) : nullptr;
        }
    };

    template<concepts::optional_iter I>
    requires concepts::random_access_iter<I>
    struct [[nodiscard]] to_pointer_iter<I> : enable_random_access<to_pointer_iter<I>, I> {
        using this_t = to_pointer_iter;

        // If impl::get returns a value, then we need to store it to return a pointer to storage
        static constexpr bool get_val = !std::is_reference_v<unsafe::get_t<I>>;
        [[no_unique_address]] I i;
        [[no_unique_address]] std::conditional_t<get_val, next_t<I>, void_t> store;

        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index) {
            if constexpr (get_val) {
                EMPLACE_NEW(self.store, MAKE_OPTIONAL(impl::get(self.i, index)));
                return *self.store;
            } else
                return impl::get(self.i, index);
        }
    };

    template<class I>
    to_pointer_iter(I) -> to_pointer_iter<I>;
}

template<iter::assert_iter I>
constexpr decltype(auto) ITER_IMPL(to_pointer_iter) (I&& iter) {
    if constexpr (iter::concepts::pointer_iter<I>) {
        return FWD(iter);
    } else {
        return iter::detail::to_pointer_iter<iter::iter_t<I>>{.i = FWD(iter)};
    }
}

#endif /* INCLUDE_ITER_TO_POINTER_ITER_HPP */

#ifndef INCLUDE_ITER_MOVE_HPP
#define INCLUDE_ITER_MOVE_HPP

ITER_DECLARE(move)

namespace iter::detail {
    template<iter::assert_iter I>
    struct move_iter : enable_random_access<move_iter<I>, I> {
        [[no_unique_address]] I i;

        using this_t = move_iter;

        constexpr move_next<next_t<I>> ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            return move_next{impl::next(self.i)};
        }

        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            decltype(auto) item = impl::get(self.i, index);
            if constexpr (std::is_reference_v<decltype(item)>)
                return std::move(item);
            else
                return item;
        }
    };

    template<class T>
    move_iter(T) -> move_iter<T>;
}

template<iter::assert_iterable I>
constexpr decltype(auto) ITER_IMPL(move) (I&& iterable) {
    if constexpr (iter::concepts::move_next<iter::next_t<I>>)
        return FWD(iterable);
    else
        return iter::detail::move_iter<iter::iter_t<I>>{.i = iter::to_iter(FWD(iterable))};
}

#endif /* INCLUDE_ITER_MOVE_HPP */

#ifndef INCLUDE_ITER_BOX_HPP
#define INCLUDE_ITER_BOX_HPP

ITER_DECLARE(box)

namespace iter {
    template<concepts::next Next, class GetType = void>
    struct virtual_iter : virtual_iter<Next, void> {
        virtual std::size_t size() const = 0;
        virtual GetType get(std::size_t index) = 0;
    private:
        using this_t = virtual_iter;
        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index) { return self.get(index); }
        constexpr auto ITER_IMPL_SIZE (this_t const& self) { return self.size(); }
    };
    template<concepts::next Next>
    struct virtual_iter<Next, void> {
        virtual Next next() = 0;
        virtual ~virtual_iter() = default;
    private:
        using this_t = virtual_iter;
        constexpr auto ITER_IMPL_NEXT (this_t& self) { return self.next(); }
    };

    namespace detail {
        template<iter I>
        struct virtual_iter_impl final : virtual_iter<next_t<I>>, I {
            template<class... Ts>
            constexpr virtual_iter_impl(Ts&&... in) : I{FWD(in)...} {}
            next_t<I> next() final { return impl::next(static_cast<I&>(*this)); }
        };
        template<concepts::random_access_iter I>
        struct virtual_iter_impl<I> final : virtual_iter<next_t<I>, unsafe::get_t<I>>, I {
            template<class... Ts>
            constexpr virtual_iter_impl(Ts&&... in) : I{FWD(in)...} {}
            next_t<I> next() final { return impl::next(static_cast<I&>(*this)); }
            std::size_t size() const final {
                return impl::size(static_cast<I const&>(*this));
            }
            unsafe::get_t<I> get(std::size_t index) final {
                return impl::get(static_cast<I&>(*this), index);
            }
        };

        struct alignas(char) deleter {
            char const heap = 1;
            template<class T>
            void operator()(T* ptr) const {
                if (heap) delete ptr;
                else ptr->~T();
            }
        };
    }

    template<std::size_t Size, std::size_t Align = 8>
    struct scratch : void_t {
        template<class T, class... Ts>
        requires (sizeof(T) <= Size) && (alignof(T) <= Align) && (Align % alignof(T) == 0)
        T* make(Ts&&... ins) { return std::launder(new (std::addressof(storage)) T(FWD(ins)...)); }
    private:
        std::aligned_storage_t<Size, Align> storage;
    };

    template<concepts::next Next, class Get = void>
    struct boxed {
        using this_t = boxed;
        static constexpr bool random_access = !std::same_as<Get, void>;

        template<iter I>
        requires std::same_as<Next, next_t<I>>
             && (!random_access || std::same_as<Get, unsafe::get_t<I>>)
        constexpr boxed(I&& to_box)
            : it{new detail::virtual_iter_impl<std::remove_cvref_t<I>>(FWD(to_box))}
        {}

        template<iter I, std::size_t Size, std::size_t Align>
        requires std::same_as<Next, next_t<I>>
             && (!random_access || std::same_as<Get, unsafe::get_t<I>>)
        constexpr boxed(I&& to_box, scratch<Size, Align>& scratch)
            : it{scratch.template make<detail::virtual_iter_impl<std::remove_cvref_t<I>>>(FWD(to_box)), {0}}
        {}

        template<class OU> requires (!random_access)
        constexpr boxed(boxed<Next, OU>&& other) : it{std::move(other.it)} {}

    private:
        template<concepts::next, class> friend struct boxed;
        constexpr Next ITER_IMPL_NEXT (this_t& self) { return self.it->next(); }
        constexpr std::size_t ITER_IMPL_SIZE (this_t const& self) requires random_access {
            return self.it->size();
        }
        constexpr Get ITER_IMPL_GET (this_t& self, std::size_t index) requires random_access {
            return self.it->get(index);
        }

        std::unique_ptr<virtual_iter<Next, Get>, detail::deleter> it;
    };

    template<iter::iter I>
    boxed(I) -> boxed<next_t<I>, unsafe::get_t<I>>;
    template<iter::iter I, std::size_t Size, std::size_t Align>
    boxed(I, scratch<Size, Align>&) -> boxed<next_t<I>, unsafe::get_t<I>>;

    template<iter I>
    using virtual_t = iter::virtual_iter<iter::next_t<I>, iter::unsafe::get_t<I>>;
    template<iter I>
    using boxed_t = boxed<next_t<I>, unsafe::get_t<I>>;
}

template<iter::assert_iter I>
constexpr auto ITER_IMPL(box) (I&& iter) {
    return iter::boxed(FWD(iter));
}

template<iter::assert_iter I, std::size_t Size, std::size_t Align>
constexpr auto ITER_IMPL(box) (I&& iter, iter::scratch<Size, Align>& scratch) {
    return iter::boxed(FWD(iter), scratch);
}

#endif /* INCLUDE_ITER_BOX_HPP */

// Consumers
#ifndef INCLUDE_ITER_FOREACH_HPP
#define INCLUDE_ITER_FOREACH_HPP

ITER_DECLARE(foreach)
ITER_ALIAS(for_each, foreach)

template<iter::assert_iterable I, iter::concepts::inspector<iter::consume_t<I>> F>
constexpr void ITER_IMPL(foreach) (I&& iterable, F func) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (auto val = iter::detail::impl::next(iter)) {
        func(iter::detail::consume(val));
    }
}

template<iter::assert_iterable I>
constexpr void ITER_IMPL(foreach) (I&& iterable) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (iter::detail::impl::next(iter)) {}
}

#endif /* INCLUDE_ITER_FOREACH_HPP */

#ifndef INCLUDE_ITER_FOLD_HPP
#define INCLUDE_ITER_FOLD_HPP

ITER_DECLARE(fold)
ITER_ALIAS(fold_left, fold)

template<iter::assert_iterable I, class T, std::invocable<const T&, iter::consume_t<I>> F>
constexpr auto ITER_IMPL(fold) (I&& iterable, T&& init, F func) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    auto acc = FWD(init);
    while (auto val = iter::detail::impl::next(iter)) {
        acc = func(iter::as_const(acc), iter::detail::consume(val));
    }
    return acc;
}

#endif /* INCLUDE_ITER_FOLD_HPP */

#ifndef INCLUDE_ITER_REDUCE_HPP
#define INCLUDE_ITER_REDUCE_HPP

ITER_DECLARE(reduce)

template<iter::assert_iterable I, std::invocable<iter::ref_t<I>, iter::consume_t<I>> F>
constexpr std::optional<iter::value_t<I>> ITER_IMPL(reduce) (I&& iterable, F&& func) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    auto acc = iter::detail::impl::next(iter);
    return acc
        ? MAKE_OPTIONAL(iter::fold(iter, iter::detail::consume(acc), FWD(func)))
        : std::nullopt;
}

#endif /* INCLUDE_ITER_REDUCE_HPP */

#ifndef INCLUDE_ITER_SUM_HPP
#define INCLUDE_ITER_SUM_HPP

ITER_DECLARE(sum)

template<iter::assert_iterable I>
requires std::is_arithmetic_v<iter::value_t<I>>
constexpr auto ITER_IMPL(sum) (I&& iterable) {
    std::remove_const_t<iter::value_t<I>> sum = 0;
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (auto val = iter::detail::impl::next(iter)) {
        sum += *val;
    }
    return sum;
}

#endif /* INCLUDE_ITER_SUM_HPP */

#ifndef INCLUDE_ITER_PRODUCT_HPP
#define INCLUDE_ITER_PRODUCT_HPP

ITER_DECLARE(product)

template<iter::assert_iterable I>
requires std::is_arithmetic_v<iter::value_t<I>>
constexpr auto ITER_IMPL(product) (I&& iterable) {
    std::remove_const_t<iter::value_t<I>> product = 1;
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (auto val = iter::detail::impl::next(iter)) {
        product *= *val;
    }
    return product;
}

#endif /* INCLUDE_ITER_PRODUCT_HPP */

#ifndef INCLUDE_ITER_LAST_HPP
#define INCLUDE_ITER_LAST_HPP

ITER_DECLARE(last)

template<iter::concepts::random_access_iterable I>
constexpr auto ITER_IMPL(last) (I&& iterable) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    std::size_t size = iter::detail::impl::size(iter);
    using get_t = decltype(iter::detail::impl::get(iter, size - 1));
    if constexpr (std::is_lvalue_reference_v<decltype(iter)> && std::is_reference_v<get_t>)
        return size > 0 ? std::addressof(iter::detail::impl::get(iter, size - 1)) : nullptr;
    else
        return size > 0 ? MAKE_OPTIONAL(iter::detail::impl::get(iter, size - 1)) : std::nullopt;
}

template<iter::concepts::random_access_iterable I, class T>
constexpr auto ITER_IMPL(last) (I&& iterable, T&& fallback) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    std::size_t size = iter::detail::impl::size(iter);
    return size > 0 ? iter::detail::impl::get(iter, size - 1) : FWD(fallback);
}

template<iter::concepts::optional_iterable I>
requires (!iter::concepts::random_access_iterable<I>)
constexpr auto ITER_IMPL(last) (I&& iterable) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    iter::next_t<decltype(iter)> results[2] = {std::nullopt, std::nullopt};
    char i = 0;
    while (true) {
        bool empty = !iter::detail::emplace_next(results[i], iter);
        i ^= 1;
        if (empty) [[unlikely]] return std::move(results[i]);
    }
}

template<iter::concepts::pointer_iterable I>
requires (!iter::concepts::random_access_iterable<I>)
constexpr auto ITER_IMPL(last) (I&& iterable) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    std::optional<iter::value_t<I>> result = std::nullopt;
    while (auto val = iter::detail::impl::next(iter)) {
        result = iter::detail::consume(val);
    }
    return result;
}

template<iter::concepts::optional_iterable I, class T>
requires (!iter::concepts::random_access_iterable<I>)
constexpr auto ITER_IMPL(last) (I&& iterable, T&& fallback) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    iter::next_t<decltype(iter)> results[2] = {std::nullopt, std::nullopt};
    char i = 0;
    while (true) {
        bool empty = !iter::detail::emplace_next(results[i], iter);
        i ^= 1;
        if (empty) [[unlikely]] {
            return results[i] ? std::move(*results[i]) : FWD(fallback);
        }
    }
}

template<iter::concepts::pointer_iterable I, class T>
requires (!iter::concepts::random_access_iterable<I>)
constexpr auto ITER_IMPL(last) (I&& iterable, T&& fallback) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    iter::value_t<I> result = FWD(fallback);
    while (auto val = iter::detail::impl::next(iter)) {
        result = iter::detail::consume(val);
    }
    return result;
}

#endif /* INCLUDE_ITER_LAST_HPP */

#ifndef INCLUDE_ITER_NTH_HPP
#define INCLUDE_ITER_NTH_HPP

ITER_DECLARE(nth)

template<iter::concepts::random_access_iterable I>
constexpr auto ITER_IMPL(nth) (I&& iterable, std::size_t n) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    std::size_t size = iter::detail::impl::size(iter);
    using get_t = decltype(iter::detail::impl::get(iter, n));
    if constexpr (std::is_lvalue_reference_v<decltype(iter)> && std::is_reference_v<get_t>)
        return size > n ? std::addressof(iter::detail::impl::get(iter, n)) : nullptr;
    else
        return size > n ? MAKE_OPTIONAL(iter::detail::impl::get(iter, n)) : std::nullopt;
}

template<iter::concepts::random_access_iterable I, class T>
constexpr auto ITER_IMPL(nth) (I&& iterable, std::size_t n, T&& fallback) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    std::size_t size = iter::detail::impl::size(iter);
    return size > n ? iter::detail::impl::get(iter, n) : FWD(fallback);
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(nth) (I&& iterable, std::size_t n) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    auto result = iter::no_next<decltype(iter)>();
    while (iter::detail::emplace_next(result, iter) && n-- > 0);
    if constexpr (iter::concepts::optional_iterable<I> || std::is_lvalue_reference_v<decltype(iter)>)
        return result;
    else
        return result ? std::make_optional(*result) : std::nullopt;
}

template<iter::assert_iterable I, class T>
constexpr auto ITER_IMPL(nth) (I&& iterable, std::size_t n, T&& fallback) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    auto result = iter::no_next<decltype(iter)>();
    while (iter::detail::emplace_next(result, iter) && n-- > 0);
    if constexpr (iter::concepts::optional_iterable<I>)
        return result ? std::move(*result) : FWD(fallback);
    else
        return result ? *result : FWD(fallback);
}
#endif /* INCLUDE_ITER_NTH_HPP */

#ifndef INCLUDE_ITER_MAX_HPP
#define INCLUDE_ITER_MAX_HPP

#ifndef INCLUDE_ITER_MIN_HPP
#define INCLUDE_ITER_MIN_HPP

ITER_DECLARE(min)
ITER_DECLARE(min_by)

namespace iter::detail::minmax {
    template<class C, iter::concepts::optional_iterable I, class F>
    constexpr auto apply(C&& comp, I&& iterable, F&& func) {
        decltype(auto) iter = iter::to_iter(FWD(iterable));
        auto next = iter::no_next<I>(), current = iter::no_next<I>();
        auto emplace_next = [&]() -> auto& { return iter::detail::emplace_next(next, iter); };
        if (emplace_next()) {
            current = std::move(next);
            while (emplace_next())
                if (std::invoke(FWD(comp), std::invoke(FWD(func), iter::as_const(*current), iter::as_const(*next))))
                    current = std::move(next);
        }
        return current;
    }

    template<class C, iter::concepts::pointer_iterable I, class F>
    constexpr auto apply(C&& comp, I&& iterable, F&& func) {
        decltype(auto) iter = iter::to_iter(FWD(iterable));
        iter::next_t<I> val{};
        auto emplace_next = [&] { return val = impl::next(iter); };
        std::optional<iter::value_t<I>> result;
        if (emplace_next()) {
            result = *val;
            while (emplace_next())
                if (std::invoke(FWD(comp), std::invoke(FWD(func), iter::as_const(*result), iter::as_const(*val))))
                    *result = *val;
        }
        return result;
    }

    static constexpr auto min = [](auto&& l) { return l > 0; };
    static constexpr auto max = [](auto&& l) { return l < 0; };

    template<class C, iter::concepts::optional_iterable I, class F>
    constexpr auto by(C&& comp, I&& iterable, F&& func) {
        decltype(auto) iter = iter::to_iter(FWD(iterable));
        auto next = iter::no_next<I>(), current = iter::no_next<I>();
        auto emplace_next = [&]() -> auto& { return iter::detail::emplace_next(next, iter); };
        if (emplace_next()) {
            auto current_proj = std::invoke(FWD(func), iter::as_const(*next));
            current = std::move(next);
            while (emplace_next()) {
                auto next_proj = std::invoke(FWD(func), iter::as_const(*next));
                if (std::invoke(FWD(comp), iter::as_const(current_proj), iter::as_const(next_proj))) {
                    current = std::move(next);
                    current_proj = std::move(next_proj);
                }
            }
        }
        return current;
    }

    template<class C, iter::concepts::pointer_iterable I, class F>
    constexpr auto by(C&& comp, I&& iterable, F&& func) {
        decltype(auto) iter = iter::to_iter(FWD(iterable));
        iter::next_t<I> val{};
        auto emplace_next = [&] { return val = impl::next(iter); };
        std::optional<iter::value_t<I>> result;
        if (emplace_next()) {
            auto current_proj = std::invoke(FWD(func), iter::as_const(*val));
            result = *val;
            while (emplace_next()) {
                auto next_proj = std::invoke(FWD(func), iter::as_const(*val));
                if (std::invoke(FWD(comp), iter::as_const(current_proj), iter::as_const(next_proj))) {
                    *result = *val;
                    current_proj = std::move(next_proj);
                }
            }
        }
        return result;
    }

    static constexpr auto min_by = [](auto&& next, auto&& current) { return next > current; };
    static constexpr auto max_by = [](auto&& next, auto&& current) { return next < current; };
}

template<iter::assert_iterable I, std::invocable<iter::cref_t<I>, iter::cref_t<I>> F = std::compare_three_way>
constexpr auto ITER_IMPL(min) (I&& iterable, F&& func = {}) {
    return iter::detail::minmax::apply(iter::detail::minmax::min, FWD(iterable), FWD(func));
}

template<iter::assert_iterable I, std::invocable<iter::cref_t<I>> F>
requires std::totally_ordered<std::invoke_result_t<F, iter::cref_t<I>>>
constexpr auto ITER_IMPL(min_by) (I&& iterable, F&& func) {
    return iter::detail::minmax::by(iter::detail::minmax::min_by, FWD(iterable), FWD(func));
}

#endif /* INCLUDE_ITER_MIN_HPP */

ITER_DECLARE(max)
ITER_DECLARE(max_by)

template<iter::assert_iterable I, std::invocable<iter::cref_t<I>, iter::cref_t<I>> F = std::compare_three_way>
constexpr auto ITER_IMPL(max) (I&& iterable, F&& func = {}) {
    return iter::detail::minmax::apply(iter::detail::minmax::max, FWD(iterable), FWD(func));
}

template<iter::assert_iterable I, std::invocable<iter::cref_t<I>> F>
requires std::totally_ordered<std::invoke_result_t<F, iter::cref_t<I>>>
constexpr auto ITER_IMPL(max_by) (I&& iterable, F&& func) {
    return iter::detail::minmax::by(iter::detail::minmax::max_by, FWD(iterable), FWD(func));
}

#endif /* INCLUDE_ITER_MAX_HPP */

#ifndef INCLUDE_ITER_FIND_LINEAR_HPP
#define INCLUDE_ITER_FIND_LINEAR_HPP

ITER_DECLARE(find_linear)

template<iter::assert_iterable I, std::predicate<iter::ref_t<I>> P>
constexpr auto ITER_IMPL(find_linear) (I&& iterable, P&& predicate) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    auto val = iter::no_next<decltype(iter)>();
    while (iter::detail::emplace_next(val, iter)) {
        if (std::invoke(FWD(predicate), *val)) {
            break;
        }
    }
    if constexpr (iter::concepts::optional_iterable<I> || std::is_lvalue_reference_v<I>)
        return val;
    else
        return val ? std::make_optional(*val) : std::nullopt;
}

#endif /* INCLUDE_ITER_FIND_LINEAR_HPP */

#ifndef INCLUDE_ITER_FIND_MAP_HPP
#define INCLUDE_ITER_FIND_MAP_HPP

ITER_DECLARE(find_map)

template<iter::assert_iterable I, std::invocable<iter::consume_t<I>> F>
constexpr auto ITER_IMPL(find_map) (I&& iterable, F&& func) {
    auto fm = iter::filter_map(FWD(iterable), FWD(func));
    if constexpr (iter::concepts::optional_iterable<decltype(fm)>)
        return iter::detail::impl::next(fm);
    else {
        auto val = iter::detail::impl::next(fm);
        return val ? std::make_optional(*val) : std::nullopt;
    }
}

#endif /* INCLUDE_ITER_FIND_MAP_HPP */

#ifndef INCLUDE_ITER_ANY_HPP
#define INCLUDE_ITER_ANY_HPP

ITER_DECLARE(any)

template<iter::assert_iterable I, std::predicate<iter::ref_t<I>> P>
constexpr auto ITER_IMPL(any) (I&& iterable, P&& predicate) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (auto val = iter::detail::impl::next(iter)) {
        if ((FWD(predicate))(*val)) {
            return true;
        }
    }

    return false;
}

#endif /* INCLUDE_ITER_ANY_HPP */

#ifndef INCLUDE_ITER_ALL_HPP
#define INCLUDE_ITER_ALL_HPP

ITER_DECLARE(all)

template<iter::assert_iterable I, std::predicate<iter::ref_t<I>> P>
constexpr auto ITER_IMPL(all) (I&& iterable, P&& predicate) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (auto val = iter::detail::impl::next(iter)) {
        if (!(FWD(predicate))(*val)) {
            return false;
        }
    }

    return true;
}

#endif /* INCLUDE_ITER_ALL_HPP */

// Collectors
#ifndef INCLUDE_ITER_COLLECT_HPP
#define INCLUDE_ITER_COLLECT_HPP

XTD_INVOKER(iter_collect)

namespace iter {
    namespace tag {
        template<template<class...> class C = std::vector, template<class> class A = std::allocator, template<class> class... Traits>
        struct collect : xtd::tagged_bindable<collect<C, A, Traits...>, xtd::invokers::iter_collect> {};
    }

    template<template<class...> class C = std::vector, template<class> class A = std::allocator, template<class> class... Traits>
    static constexpr tag::collect<C, A, Traits...> collect;
}

ITER_ALIAS(to_vector, collect<std::vector>)
ITER_ALIAS(to_map, collect<std::map>)
ITER_ALIAS(to_string, collect<std::basic_string, std::allocator, std::char_traits>)

template<template<class...> class CT, template<class> class AT, template<class> class... Traits, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_collect, iter::tag::collect<CT, AT, Traits...>)(I&& iter) {
    using T = iter::value_t<I>;
    CT<T, Traits<T>..., AT<T>> container;
    if constexpr (iter::concepts::random_access_iter<I>) {
        container.reserve(iter::detail::impl::size(iter));
    }
    while (auto val = iter::detail::impl::next(iter)) {
        container.emplace_back(iter::detail::consume(val));
    }
    return container;
}

template<template<class...> class CT, template<class> class AT, template<class> class... Traits, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_collect, iter::tag::collect<CT, AT, Traits...>)(I&& iter, std::size_t reserve) {
    using T = iter::value_t<I>;
    CT<T, Traits<T>..., AT<T>> container;
    if constexpr (iter::concepts::random_access_iter<I>) {
        reserve = std::max(reserve, iter::detail::impl::size(iter));
    }
    container.reserve(reserve);
    while (auto val = iter::detail::impl::next(iter)) {
        container.emplace_back(iter::detail::consume(val));
    }
    return container;
}

template<template<class> class AT, iter::assert_iter I, class Comp>
constexpr auto XTD_IMPL_TAG_(iter_collect, iter::tag::collect<std::map, AT>)(I&& iter, Comp&& compare) {
    using KV = iter::value_t<I>;
    using K = std::tuple_element_t<0, KV>;
    using V = std::tuple_element_t<1, KV>;
    using A = AT<std::pair<K const, V>>;
    std::map<K, V, std::remove_cvref_t<Comp>, A> container(FWD(compare));
    while (auto val = iter::detail::impl::next(iter)) {
        container.emplace(iter::detail::consume(val));
    }
    return container;
}

template<template<class> class AT, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_collect, iter::tag::collect<std::map, AT>)(I&& iter) {
    using KV = iter::value_t<I>;
    using K = std::tuple_element_t<0, KV>;
    return iter::collect<std::map, AT>(FWD(iter), std::less<K>{});
}

#endif /* INCLUDE_ITER_COLLECT_HPP */

#ifndef INCLUDE_ITER_PARTITION_HPP
#define INCLUDE_ITER_PARTITION_HPP

XTD_INVOKER(iter_partition)

namespace iter {
    namespace tag {
        template<std::size_t N = 2>
        struct partition_ : xtd::tagged_bindable<partition_<N>, xtd::invokers::iter_partition> {};
    }

    template<std::size_t N = 2>
    static constexpr tag::partition_<N> partition_;

    template<std::size_t I>
    struct index_t : index_t<I+1> {
        template<std::size_t J>
        requires (J < I)
        constexpr index_t(index_t<J> j) : index_t<I+1>{j} {}
        constexpr index_t() : index_t<I+1>{I} {}
    protected:
        constexpr index_t(size_t i) : index_t<I+1>{i} {}
    };

    template<>
    struct index_t<12> {
        constexpr std::size_t value() const { return index; }
    protected:
        constexpr index_t(size_t i) : index{i} {}
        std::size_t const index;
    };

    template<std::size_t I>
    static constexpr auto index = index_t<I>{};

    template<std::size_t I>
    struct maximum {
        static constexpr auto values = []<std::size_t... Is>(std::index_sequence<Is...>) {
            return std::array<index_t<I>, I+1>{index_t<Is>{}...};
        }(std::make_index_sequence<I+1>{});
    };
}

ITER_ALIAS(partition, partition_<>)

template<size_t N, iter::assert_iterable I, class F>
constexpr decltype(auto) XTD_IMPL_TAG_(iter_partition, iter::tag::partition_<N>) (I&& iterable, F&& func) {
    return iter::partition_<N>(iter::to_iter(FWD(iterable)), FWD(func));
}

template<size_t N, iter::iter I, class F>
requires (N > 1)
constexpr decltype(auto) XTD_IMPL_TAG_(iter_partition, iter::tag::partition_<N>) (I&& iter, F&& func) {
    auto out = std::array<std::vector<iter::value_t<std::decay_t<I>>>, N>{};

    if constexpr (iter::concepts::random_access_iter<I>) {
        std::size_t size = iter::detail::impl::size(iter) / N;
        apply([=](auto&&... outs) { (outs.reserve(size), ...); }, out);
    }

    while (auto val = iter::detail::impl::next(iter)) {
        auto slot = std::invoke(FWD(func), iter::as_const(*val));
        std::size_t index;
        if constexpr (std::is_same_v<bool, decltype(slot)>) {
            static_assert(N == 2, "Boolean predicate function only permitted with iter::partition<2>.");
            index = slot ? 0 : 1;
        } else {
            static_assert(std::is_same_v<iter::index_t<N-1>, decltype(slot)>,
                "Function called in iter::partition<N> must return iter::index_t<N-1>.");
            index = slot.value();
        }

        out[index].emplace_back(iter::detail::consume(val));
    }
    return out;
}

#endif /* INCLUDE_ITER_PARTITION_HPP */

#ifndef INCLUDE_ITER_UNZIP_HPP
#define INCLUDE_ITER_UNZIP_HPP

XTD_INVOKER(iter_unzip)

namespace iter {
    namespace tag {
        template<template<class...> class C = std::vector, template<class> class A = std::allocator>
        struct unzip_ : xtd::tagged_bindable<unzip_<C, A>, xtd::invokers::iter_unzip> {};
    }

    template<template<class...> class C = std::vector, template<class> class A = std::allocator>
    static constexpr tag::unzip_<C, A> unzip_;

    namespace detail {
        template<template<class...> class CT, template<class> class AT, class>
        struct unzipped;

        template<template<class...> class CT, template<class> class AT, class... Ts>
        struct unzipped<CT, AT, tuple<Ts...>> {
            using type = tuple<CT<Ts, AT<Ts>>...>;
            static constexpr std::size_t size = sizeof...(Ts);
        };
        template<template<class...> class CT, template<class> class AT, class... Ts>
        struct unzipped<CT, AT, std::tuple<Ts...>> {
            using type = std::tuple<CT<Ts, AT<Ts>>...>;
            static constexpr std::size_t size = sizeof...(Ts);
        };
        template<template<class...> class CT, template<class> class AT, class T, std::size_t N>
        struct unzipped<CT, AT, std::array<T, N>> {
            using type = std::array<CT<T, AT<T>>, N>;
            static constexpr std::size_t size = N;
        };
    }
}

ITER_ALIAS(unzip, unzip_<>)

template<template<class...> class CT, template<class> class AT, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_unzip, iter::tag::unzip_<CT, AT>)(I&& iter) {
    using traits = iter::detail::unzipped<CT, AT, iter::value_t<I>>;
    typename traits::type containers{};

    if constexpr (iter::concepts::random_access_iter<I>) {
        apply([size = iter::detail::impl::size(iter)](auto&... c) {
            (c.reserve(size), ...);
        }, containers);
    }
    while (auto val = iter::detail::impl::next(iter)) {
        [&]<std::size_t... Is>(std::index_sequence<Is...>) {
            (get<Is>(containers).emplace_back(std::move(get<Is>(*val))), ...);
        }(std::make_index_sequence<traits::size>{});
    }
    return containers;
}

template<template<class...> class CT, template<class> class AT, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_unzip, iter::tag::unzip_<CT, AT>)(I&& iter, std::size_t reserve) {
    using traits = iter::detail::unzipped<CT, AT, iter::value_t<I>>;
    typename traits::type containers{};

    if constexpr (iter::concepts::random_access_iter<I>)
        reserve = std::max(reserve, iter::detail::impl::size(iter));

    apply([=](auto&... c) {
        (c.reserve(reserve), ...);
    }, containers);

    while (auto val = iter::detail::impl::next(iter)) {
        [&]<std::size_t... Is>(std::index_sequence<Is...>) {
            (get<Is>(containers).emplace_back(std::move(get<Is>(*val))), ...);
        }(std::make_index_sequence<traits::size>{});
    }

    return containers;
}

#endif /* INCLUDE_ITER_UNZIP_HPP */

#ifndef INCLUDE_ITER_SORTED_HPP
#define INCLUDE_ITER_SORTED_HPP

XTD_INVOKER(iter_sorted)

namespace iter {
    namespace tag {
        template<template<class...> class C = std::vector, template<class> class A = std::allocator>
        struct sorted_ : xtd::tagged_bindable<sorted_<C, A>, xtd::invokers::iter_sorted> {};
    }

    template<template<class...> class C = std::vector, template<class> class A = std::allocator>
    static constexpr tag::sorted_<C, A> sorted_;
}

ITER_ALIAS(sorted, sorted_<>)

template<template<class...> class CT, template<class> class AT,
         iter::assert_iter I, std::invocable<iter::ref_t<I>, iter::ref_t<I>> P>
constexpr auto XTD_IMPL_TAG_(iter_sorted, iter::tag::sorted_<CT, AT>)(I&& iter, P&& predicate) {
    auto container = iter::collect<CT, AT>(FWD(iter));
    std::sort(std::begin(container), std::end(container), FWD(predicate));
    return container;
}

template<template<class...> class CT, template<class> class AT, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_sorted, iter::tag::sorted_<CT, AT>)(I&& iter) {
    auto container = iter::collect<CT, AT>(FWD(iter));
    std::sort(std::begin(container), std::end(container));
    return container;
}

#endif /* INCLUDE_ITER_SORTED_HPP */

// Misc
#ifndef INCLUDE_ITER_COMPARISON_HPP
#define INCLUDE_ITER_COMPARISON_HPP

template<iter::iter I1, iter::iter I2>
constexpr bool operator==(I1 i1, I2 i2) {
    auto item1 = iter::no_next<I1>();
    auto item2 = iter::no_next<I2>();

    while (!!iter::detail::emplace_next(item1, i1) & !!iter::detail::emplace_next(item2, i2)) {
        if (*item1 != *item2) return false;
    }

    return !!item1 == !!item2;
}

template<iter::concepts::random_access_iter I1, iter::concepts::random_access_iter I2>
constexpr bool operator==(I1 i1, I2 i2) {
    auto size = iter::detail::impl::size(i1);
    if (size != iter::detail::impl::size(i2)) return false;

    for (std::size_t i = 0; i < size; ++i) {
        decltype(auto) item1 = iter::detail::impl::get(i1, i);
        decltype(auto) item2 = iter::detail::impl::get(i2, i);
        if (item1 != item2) return false;
    }

    return true;
}

template<iter::iter I1, iter::iter I2>
constexpr auto operator<=>(I1 i1, I2 i2) {
    auto item1 = iter::no_next<I1>();
    auto item2 = iter::no_next<I2>();

    while (!!iter::detail::emplace_next(item1, i1)
         & !!iter::detail::emplace_next(item2, i2)) {
        if (auto rel = *item1 <=> *item2; rel != 0) return rel;
    }

    return !!item1 <=> !!item2;
}

#endif /* INCLUDE_ITER_COMPARISON_HPP */

// Must be last
#ifndef INCLUDE_ITER_WRAP_HPP
#define INCLUDE_ITER_WRAP_HPP

namespace iter {
    template<iterable I>
    struct [[nodiscard]] wrap : wrap<iter_t<I>> {
        template<class II>
        wrap(II&& iterable) : wrap<iter_t<I>>{to_iter(FWD(iterable))} {}
    };
    template<iter I>
    struct [[nodiscard]] wrap<I> {
        [[no_unique_address]] I i;

        using this_t = wrap;
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            return iter::detail::impl::next(self.i);
        }
        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self)
            requires concepts::double_ended_iter<I>
        {
            return iter::detail::impl::next_back(self.i);
        }
        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index)
            requires concepts::random_access_iter<I>
        {
            return iter::detail::impl::get(self.i, index);
        }
        constexpr std::size_t ITER_IMPL_SIZE (this_t const& self)
            requires concepts::random_access_iter<I>
        {
            return iter::detail::impl::size(self.i);
        }

#define ITER_X(fun) \
        template<class... Ts>\
        constexpr decltype(auto) fun(Ts&&... args) & {\
            return invoke(::iter::fun, i, FWD(args)...);\
        }\
        template<class... Ts>\
        constexpr decltype(auto) fun(Ts&&... args) && {\
            return invoke(::iter::fun, std::move(i), FWD(args)...);\
        }
/* Do not modify, generated by scripts/update_x_macros.sh */

// Invoke iter::cycle on this iter
ITER_X(cycle)
// Invoke iter::filter on this iter
ITER_X(filter)
// Invoke iter::take on this iter
ITER_X(take)
// Invoke iter::take_while on this iter
ITER_X(take_while)
// Invoke iter::skip on this iter
ITER_X(skip)
// Invoke iter::skip_while on this iter
ITER_X(skip_while)
// Invoke iter::flatten on this iter
ITER_X(flatten)
// Invoke iter::flatmap on this iter
ITER_X(flatmap)
// Invoke iter::flat_map (aka iter::flatmap) on this iter
ITER_X(flat_map)
// Invoke iter::filter_map on this iter
ITER_X(filter_map)
// Invoke iter::map on this iter
ITER_X(map)
// Invoke iter::map_while on this iter
ITER_X(map_while)
// Invoke iter::zip on this iter
ITER_X(zip)
// Invoke iter::enumerate (aka iter::enumerate_<>) on this iter
ITER_X(enumerate)
// Invoke iter::reverse on this iter
ITER_X(reverse)
// Invoke iter::chain on this iter
ITER_X(chain)
// Invoke iter::chunks (aka iter::chunks_<>) on this iter
ITER_X(chunks)
// Invoke iter::inspect on this iter
ITER_X(inspect)
// Invoke iter::move on this iter
ITER_X(move)
// Invoke iter::box on this iter
ITER_X(box)
// Invoke iter::foreach on this iter
ITER_X(foreach)
// Invoke iter::for_each (aka iter::foreach) on this iter
ITER_X(for_each)
// Invoke iter::fold on this iter
ITER_X(fold)
// Invoke iter::fold_left (aka iter::fold) on this iter
ITER_X(fold_left)
// Invoke iter::reduce on this iter
ITER_X(reduce)
// Invoke iter::sum on this iter
ITER_X(sum)
// Invoke iter::product on this iter
ITER_X(product)
// Invoke iter::last on this iter
ITER_X(last)
// Invoke iter::nth on this iter
ITER_X(nth)
// Invoke iter::min on this iter
ITER_X(min)
// Invoke iter::min_by on this iter
ITER_X(min_by)
// Invoke iter::max on this iter
ITER_X(max)
// Invoke iter::max_by on this iter
ITER_X(max_by)
// Invoke iter::find_linear on this iter
ITER_X(find_linear)
// Invoke iter::find_map on this iter
ITER_X(find_map)
// Invoke iter::any on this iter
ITER_X(any)
// Invoke iter::all on this iter
ITER_X(all)
// Invoke iter::to_vector (aka iter::collect<std::vector>) on this iter
ITER_X(to_vector)
// Invoke iter::to_map (aka iter::collect<std::map>) on this iter
ITER_X(to_map)
// Invoke iter::to_string (aka iter::collect<std::basic_string, std::allocator, std::char_traits>) on this iter
ITER_X(to_string)
// Invoke iter::partition (aka iter::partition_<>) on this iter
ITER_X(partition)
// Invoke iter::unzip (aka iter::unzip_<>) on this iter
ITER_X(unzip)
// Invoke iter::sorted (aka iter::sorted_<>) on this iter
ITER_X(sorted)

#undef ITER_X

#define ITER_EXPAND(...) __VA_ARGS__
#define ITER_X(fun, tmplParams, tmplArgs) \
        template<ITER_EXPAND tmplParams, class... Ts>\
        constexpr decltype(auto) fun(Ts&&... args) & {\
            return invoke(::iter::fun<ITER_EXPAND tmplArgs>, i, FWD(args)...);\
        }\
        template<ITER_EXPAND tmplParams, class... Ts>\
        constexpr decltype(auto) fun(Ts&&... args) && {\
            return invoke(::iter::fun<ITER_EXPAND tmplArgs>, std::move(i), FWD(args)...);\
        }
/* Do not modify, generated by scripts/update_x_macros.sh */

// Invoke iter::T on this iter
ITER_X(enumerate_, (class T = std::size_t), (T))
// Invoke iter::N on this iter
ITER_X(chunks_, (std::size_t N = 0), (N))
// Invoke iter::N on this iter
ITER_X(window, (std::size_t N = 2), (N))
// Invoke iter::C, A, Traits... on this iter
ITER_X(collect, (template<class...> class C = std::vector, template<class> class A = std::allocator, template<class> class... Traits), (C, A, Traits...))
// Invoke iter::N on this iter
ITER_X(partition_, (std::size_t N = 2), (N))
// Invoke iter::C, A on this iter
ITER_X(unzip_, (template<class...> class C = std::vector, template<class> class A = std::allocator), (C, A))
// Invoke iter::C, A on this iter
ITER_X(sorted_, (template<class...> class C = std::vector, template<class> class A = std::allocator), (C, A))

#undef ITER_EXPAND
#undef ITER_X

    private:
        template<xtd::concepts::Bindable Tag, class Underlying, class... Ts>
        static constexpr decltype(auto) invoke(Tag const& tag, Underlying&& underlying, Ts&&... args) {
            auto call = [&]() -> decltype(auto) { return tag(FWD(underlying), FWD(args)...); };
            if constexpr (iter<decltype(call())>)
                return ::iter::wrap{call()};
            else
                return call();
        }
    };

    template<iter::iterable I>
    requires (!iter::iter<I>)
    wrap(I&&) -> wrap<I>;

    template<iter::iter I>
    wrap(I) -> wrap<I>;
}

#endif /* INCLUDE_ITER_WRAP_HPP */

#endif /* ITER_INCLUDE_ITER_HPP */

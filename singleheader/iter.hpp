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
#  define ITER_LIBRARY_VERSION 20210906
#endif

#ifndef EXTEND_INCLUDE_EXTEND_HPP
#define EXTEND_INCLUDE_EXTEND_HPP

#include <type_traits>
#include <concepts>
#include <functional>
#include <string_view>

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
            noexcept(noexcept(xtd_invoke_main((Ts&&) in...)))
            -> decltype(xtd_invoke_main((Ts&&) in...))
        {
            return xtd_invoke_main((Ts&&) in...);
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
    return { (F&&) f };
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
        CP::invoker_t::invoke(xtd_free_t{}, bindable.self(), (Ts&&) in...);
    };
    template<class CP, class... Ts>
    concept CustomisedMethod = requires (const CP& bindable, Ts&&... in) {
        CP::invoker_t::invoke(xtd_method_t<>{}, bindable.self(), (Ts&&) in...);
    };
    template<class CP, class... Ts>
    concept Customised = CustomisedMethod<CP, Ts...> || CustomisedFree<CP, Ts...>;

    template<class T, class R>
    concept CompatInterface = std::constructible_from<R, T>;

    template<class CP, class... Ts>
    concept AllowedFree = requires (const CP& bindable, Ts&&... in) {
        { CP::invoker_t::invoke(xtd_free_t{}, bindable.self(), (Ts&&) in...) } ->
            CompatInterface<decltype(typename CP::interface_t{}.apply((Ts&&) in...))>;
    };

    template<class CP, class... Ts>
    concept AllowedMethod = requires (const CP& bindable, Ts&&... in) {
        { CP::invoker_t::invoke(xtd_method_t<>{}, bindable.self(), (Ts&&) in...) } ->
            CompatInterface<decltype(typename CP::interface_t{}.apply((Ts&&) in...))>;
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
    return r((L&&) l);
}

template<class L, concepts::Bindable R>
requires (!concepts::Bound<L>)
constexpr decltype(auto) operator ->* (L&& l, const R& r) {
    return make_bound([&]<class... T>(T&&... in) {
        return r((L&&) l, (T&&) in...);
    });
}

template<concepts::Bound L, concepts::Bound R>
constexpr auto operator ->* (L&& l, R&& r) {
    static_assert(std::decay_t<L>::owner && std::decay_t<R>::owner,
        "When building a chain of bindables that is not immediately invoked, "
        "please call bindable.capture(...) instead of bindable(...)");
    return make_bound<0, true>(
        [l = (L&&) l, r = (R&&) r]<class T>(T&& in) {
            return l((T&&) in) ->* r; });
}

template<concepts::Bound L, class R>
requires (!concepts::Bound<R>)
constexpr decltype(auto) operator ->* (const L& l, R&& r) {
    return l((R&&) r);
}

template<concepts::Bound L>
constexpr decltype(auto) operator ->* (const L& l, bind_placeholder<>) {
    return l();
}

template<class L, class R>
constexpr decltype(auto) operator | (L&& l, R&& r) {
    return (L&&) l ->* (R&&) r;
}

namespace detail {
    template<class... Fs>
    struct overload : Fs... {
        using Fs::operator()...;
    };
    template<class... Fs>
    overload(Fs...) -> overload<Fs...>;
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
            return invoker_t::invoke(dispatcher{}, self(), (Ts&&) ins...);
        else {
            if constexpr (concepts::CustomisedMethod<bindable, Ts...>)
                static_assert(concepts::AllowedMethod<bindable, Ts...>,
                    "No implementation for this parameter set follows the interface");
            else
                static_assert(concepts::AllowedFree<bindable, Ts...>,
                    "No implementation for this parameter set follows the interface");
            using return_t = decltype(Interface{}.apply((Ts&&) ins...));
            if constexpr (std::is_same_v<void, return_t>)
                return invoker_t::invoke(dispatcher{}, self(), (Ts&&) ins...);
            else
                return return_t(invoker_t::invoke(dispatcher{}, self(), (Ts&&) ins...));
        }
    }

    template<class... Ts>
    // This call operator is only enabled if it has its own function calls
    requires (!is_specializable && sizeof...(Ts) > 0)
          && (!concepts::BindPlaceholder<Ts> && ...)
          && (std::invocable<Fs const, Ts...> || ...)
    constexpr decltype(auto) operator()(Ts&&... ins) const {
        return detail::overload<Fs...>::operator()((Ts&&) ins...);
    }

    [[nodiscard]] constexpr auto operator()() const {
        return operator()(bind_placeholder<0>{});
    }

    template<class... Ts>
    requires (sizeof...(Ts) > 0) && (concepts::BindPlaceholderAt<Ts, 0> || ...)
    [[nodiscard]] constexpr auto operator()(Ts&&... in) const {
        return capture<sizeof...(Ts) == 1>((Ts&&) in...);
    }

    template<bool Store = true, class... Ts>
    requires (sizeof...(Ts) > 0) && (concepts::BindPlaceholderAt<Ts, 0> || ...)
    [[nodiscard]] constexpr auto capture(Ts&&... in) const {
        constexpr std::size_t count = ((concepts::BindPlaceholder<Ts> ? 1 : 0) + ...);
        static_assert(count == 1, "May specify only one placeholder");
        if constexpr (Store) {
            return make_bound<0, Store>([=, this]<class L>(L&& l) {
                return (*this)(select((L&&) l, in)...);
            });
        } else {
            return make_bound([&, this]<class L>(L&& l) {
                return (*this)(select((L&&) l, (Ts&&) in)...);
            });
        }
    }

    template<size_t I, class... Ts>
    requires (I > 0) && (!concepts::BindPlaceholder<Ts> && ...)
    [[nodiscard]] constexpr auto operator()(const bind_placeholder<I>&, Ts&&... ins) const {
        return bind_n<I, sizeof...(Ts) == 0>(std::make_index_sequence<1 + sizeof...(Ts)>{}, (Ts&&) ins...);
    }

    template<size_t I, class... Ts>
    requires (I > 0) && (!concepts::BindPlaceholder<Ts> && ...)
    [[nodiscard]] constexpr auto capture(const bind_placeholder<I>&, Ts&&... ins) const {
        return bind_n<I, true>(std::make_index_sequence<1 + sizeof...(Ts)>{}, (Ts&&) ins...);
    }

    constexpr auto& self() const {
        if constexpr (is_tagged())
            return static_cast<U const&>(*this);
        else
            return *this;
    }

private:

    template<class L, class R>
    static constexpr decltype(auto) select(L&& l, R&& r) {
        if constexpr (concepts::BindPlaceholder<R>)
            return (L&&) l;
        else
            return (R&&) r;
    }

    template<size_t BI, bool Store, std::size_t... Is, class... Ts>
    constexpr decltype(auto) bind_n(std::index_sequence<Is...>, Ts&&... ins) const {
        if constexpr (Store) {
            return make_bound<0, Store>([=, this]<class L>(L&& l) {
                return (*this)(select_i<BI, Is>((L&&) l, ins...)...);
            });
        } else {
            return make_bound([&, this]<class L>(L&& l) {
                return (*this)(select_i<BI, Is>((L&&) l, (Ts&&) ins...)...);
            });
        }
    }

    template<size_t I, class T, class... Ts>
    static constexpr decltype(auto) get(T&& in, Ts&&... ins) {
        if constexpr (I == 0)
            return (T&&) in;
        else
            return get<I-1>((Ts&&) ins...);
    }

    template<size_t BI, std::size_t I, class L, class... Rs>
    static constexpr decltype(auto) select_i(L&& l, Rs&&... rs) {
        constexpr std::size_t index = I > BI ? I - 1 : I;
        if constexpr (index == BI)
            return (L&&) l;
        else
            return get<index>((Rs&&) rs...);
    }
};

template<class... F>
bindable(F...) -> bindable<void, invokers::main, void, F...>;

template<class Tag, class Invoker = xtd::invokers::main, class Interface = void>
using tagged_bindable = bindable<Tag, Invoker, Interface>;

template<class F>
constexpr auto apply(F&& func) {
    return [func = (F&&)func]<class T>(T&& tuple) mutable {
        return apply(func, (T&&)tuple);
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
                noexcept(noexcept(xtd_invoke_ ## name((Ts&&) in...)))\
                -> decltype(xtd_invoke_ ## name((Ts&&) in...))\
            {\
                return xtd_invoke_ ## name((Ts&&) in...);\
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
    template<class T, class... Ts>\
    constexpr auto impl(tag) (T&& obj, Ts&&... in)\
        noexcept(noexcept((T&&) obj.func((Ts&&) in...)))\
        -> decltype((T&&) obj.func((Ts&&) in...))\
    {\
        return ((T&&) obj).func((Ts&&) in...);\
    }

#define XTD_IMPL_TRY_FORWARD(tag, func) \
    template<class... Ts>\
    constexpr auto impl(tag) (Ts&&... in)\
        noexcept(noexcept(func((Ts&&) in...)))\
        -> decltype(func((Ts&&) in...))\
    {\
        return func((Ts&&) in...);\
    }

#endif /* EXTEND_INCLUDE_EXTEND_HPP */

#include <optional>
#include <memory>
#include <limits>

#ifndef FWD
#  define FWD(arg) static_cast<decltype(arg)&&>(arg)
#endif

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
        static_assert(I < tuple.size(), "Tuple index out of bounds");
        return detail::get<I>(std::forward<Tuple>(tuple));
    }

    // Make a tuple with element types exactly the same as those returned from lazy_values
    template<std::invocable<>... Fs>
    tuple<std::invoke_result_t<Fs>...> make_tuple_lazy(Fs&&... lazy_values) {
        return {std::invoke(FWD(lazy_values))...};
    }

    template<class F, concepts::decays_to_tuple Tuple>
    decltype(auto) apply(F&& func, Tuple&& tuple) {
        return [&]<std::size_t... Is>(std::index_sequence<Is...>) {
            return std::invoke(std::forward<F>(func), get<Is>(std::forward<Tuple>(tuple))...);
        }(std::make_index_sequence<tuple.size()>{});
    }

    template<class T, concepts::decays_to_tuple Tuple>
    decltype(auto) make_from_tuple(Tuple&& tuple) {
        return [&]<std::size_t... Is>(std::index_sequence<Is...>) {
            return T(get<Is>(std::forward<Tuple>(tuple))...);
        }(std::make_index_sequence<tuple.size()>{});
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
#  define ITER_INVOKER(name) XTD_INVOKER(iter_ ## name)
#  define ITER_FUNCTION(fun, ...) XTD_FUNCTION_(iter_ ## fun) fun
#  define ITER_IMPL(name) XTD_IMPL_(iter_ ## name, iter::name)
#  define ITER_IMPL_THIS(name) XTD_IMPL_THIS_(iter_ ## name, iter::name)
#  define ITER_UNSAFE_IMPL(name) XTD_IMPL_(iter_ ## name, iter::unsafe::name)
#  define ITER_UNSAFE_SIZE XTD_IMPL_THIS_(iter_size, iter::unsafe::size)
#  define ITER_UNSAFE_GET XTD_IMPL_THIS_(iter_get, iter::unsafe::get)
#else
#  warning Overload resolution is more complex with ITER_GLOBAL_INVOKER. \
           Any failing invocations may return an endless list of candidates.
#  define ITER_INVOKER(name)
#  define ITER_FUNCTION(fun) XTD_FUNCTION fun
#  define ITER_IMPL(name) XTD_IMPL(iter::name)
#  define ITER_IMPL_THIS(name) XTD_IMPL_THIS(iter::name)
#  define ITER_UNSAFE_IMPL(name) XTD_IMPL(iter::unsafe::name)
#  define ITER_UNSAFE_SIZE XTD_IMPL_THIS(iter::unsafe::size)
#  define ITER_UNSAFE_GET XTD_IMPL_THIS(iter::unsafe::get)
#endif

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
ITER_DECLARE(next)
ITER_DECLARE(cycle)
ITER_INVOKER(get)
ITER_INVOKER(size)

namespace iter {
    namespace unsafe {
        // Random access functions
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
            { iter::next(it) } -> pointer_next;
        };
        template<class T>
        concept optional_iter = requires(T it) {
            { iter::next(it) } -> optional_next;
        };
        template<class T>
        concept iter = pointer_iter<T> || optional_iter<T>;

        template<class T>
        concept random_access_iter = iter<T> && requires (T it, std::size_t index) {
            iter::unsafe::get(it, index);
            { iter::unsafe::size(it) } -> std::same_as<std::size_t>;
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

    namespace unsafe {
        template<class I>
        auto get_type() -> void;
        template<concepts::random_access_iter I>
        auto get_type() -> decltype(iter::unsafe::get(std::declval<std::remove_reference_t<I>&>(), 0));
        template<class I>
        using get_t = decltype(get_type<I>());

        template<class I>
        [[nodiscard]] constexpr auto get_option(I&& iter, std::size_t index) {
            std::size_t size = iter::unsafe::size(iter);
            using get_t = decltype(iter::unsafe::get(iter, index));
            if constexpr (std::is_lvalue_reference_v<get_t>)
                return (index < size) ? std::addressof(iter::unsafe::get(iter, index)) : nullptr;
            else if constexpr (std::is_rvalue_reference_v<get_t>) {
                auto&& item = iter::unsafe::get(iter, index);
                return move_next{(index < size) ? std::addressof(item) : nullptr};
            } else
                return (index < size) ? MAKE_OPTIONAL(iter::unsafe::get(iter, index)) : std::nullopt;
        }
    }

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
    using next_t = decltype(iter::next(std::declval<iter_t<std::remove_reference_t<I>>&>()));

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
            return EMPLACE_NEW(current, iter::next(it));
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
    using value_t = typename detail::iter_traits<std::remove_cvref_t<T>>::value_t;
    template<class T>
    using ref_t = typename detail::iter_traits<std::remove_cvref_t<T>>::ref_t;
    template<class T>
    using cref_t = typename detail::iter_traits<std::remove_cvref_t<T>>::cref_t;
    template<class T>
    using consume_t = typename detail::iter_traits<std::remove_cvref_t<T>>::consume_t;

    template<iterable I>
    static constexpr next_t<I> no_next() {
        if constexpr(concepts::pointer_iterable<I>)
            return next_t<I>{nullptr};
        else
            return next_t<I>{std::nullopt};
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
            using base_t = enable_random_access;
        };

        template<class Self, concepts::random_access_iter I>
        struct enable_random_access<Self, I> {
            static constexpr bool random_access = true;
            std::size_t index = 0;

        protected:
            using this_t = enable_random_access;
            using base_t = enable_random_access;

            constexpr auto ITER_UNSAFE_SIZE (this_t const& base) {
                return iter::unsafe::size(static_cast<Self const&>(base).i);
            }
            constexpr auto ITER_IMPL_THIS(next) (this_t& base) {
                auto index = base.index++;
                auto& self = static_cast<Self&>(base);
                return iter::unsafe::get_option(self, index);
            }
        };

        template<class Self, class... I>
        requires (sizeof...(I) > 1) && (!concepts::random_access_iter<I> || ...)
        struct enable_random_access<Self, I...> {
            static constexpr bool random_access = false;

        protected:
            using this_t = enable_random_access;
            using base_t = enable_random_access;
        };

        template<class Self, concepts::random_access_iter... I>
        requires (sizeof...(I) > 1)
        struct enable_random_access<Self, I...> {
            static constexpr bool random_access = true;
            std::size_t index = 0;
            std::size_t size = 0;

        protected:
            using this_t = enable_random_access;
            using base_t = enable_random_access;

            constexpr auto ITER_UNSAFE_SIZE (this_t const& base) {
                return base.size;
            }
            constexpr auto ITER_IMPL_THIS(next) (this_t& base) {
                auto index = base.index++;
                auto& self = static_cast<Self&>(base);
                return iter::unsafe::get_option(self, index);
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

// Define unsafe random access functions as deleted by default
template<class... Ts>
void ITER_UNSAFE_IMPL(get) (Ts&&...) = delete;
template<class... Ts>
void ITER_UNSAFE_IMPL(size) (Ts&&...) = delete;

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
    template<class Underlying>
    struct [[nodiscard]] container_iter {
    protected:
        using this_t = container_iter;
        static constexpr bool owner = !std::is_lvalue_reference_v<Underlying>;

        using underyling_t = std::conditional_t<owner, Underlying, std::remove_reference_t<Underlying>*>;
        underyling_t underlying;
        std::size_t pos;

        auto& get_underlying() {
            if constexpr (owner)
                return underlying;
            else
                return *underlying;
        }
        auto& get_underlying() const {
            if constexpr (owner)
                return underlying;
            else
                return *underlying;
        }

    public:
        template<class... Ts>
        requires (owner)
        container_iter(std::in_place_t, Ts&&... ins)
            : underlying{FWD(ins)...}
            , pos{0}
        {}

        template<class... Ts>
        requires (!owner)
        container_iter(std::in_place_t, Underlying& under)
            : underlying{&under}
            , pos{0}
        {}

        container_iter(container_iter&& other)
            : underlying{std::move(other.underlying)}
            , pos{other.pos}
        {}

        container_iter(const container_iter& other)
            : underlying{other.underlying}
            , pos{other.pos}
        {}

        container_iter& operator=(container_iter&& other) {
            underlying = std::move(other.underlying);
            pos = other.pos;
            return *this;
        }

        container_iter& operator=(const container_iter& other) {
            underlying = other.underlying;
            pos = other.pos;
            return *this;
        }

        constexpr auto ITER_UNSAFE_GET (this_t& self, std::size_t index) -> auto& {
            return self.get_underlying()[index];
        }

        constexpr auto ITER_UNSAFE_SIZE (this_t const& self) {
            return std::size(self.get_underlying());
        }

        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
            return self.pos != std::size(self.get_underlying())
                ? std::addressof(self.get_underlying()[self.pos++])
                : nullptr;
        }

        struct cycle;

        constexpr auto ITER_IMPL_THIS(cycle) (this_t&& self) requires (owner) {
            return cycle{FWD(self)};
        }
    };

    template<class T>
    struct container_iter<T>::cycle : container_iter<T> {
        using this_t = cycle;

        constexpr auto ITER_UNSAFE_GET (this_t& self, std::size_t index) -> auto& {
            auto size = std::size(self.get_underlying());
            return self.get_underlying()[index % size];
        }

        constexpr auto ITER_UNSAFE_SIZE (this_t const&) {
            return std::numeric_limits<std::size_t>::max();
        }

        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
            if (self.pos == std::size(self.get_underlying())) [[unlikely]]
                self.pos = 0;
            auto result = std::addressof(self.get_underlying()[self.pos]);
            ++self.pos;
            return result;
        }
     };
}

namespace iter::concepts {
    template<class T>
    static constexpr bool is_array = false;

    template<class T, std::size_t N>
    constexpr bool is_array<std::array<T, N>> = true;

    template<class T>
    concept array = is_array<std::remove_cvref_t<T>>;

    template<class T>
    static constexpr bool is_vector = false;

    template<class T, class A>
    constexpr bool is_vector<std::vector<T, A>> = true;

    template<class T>
    concept vector = is_vector<std::remove_cvref_t<T>>;

    template<class T>
    concept container = array<T> || vector<T>;
}

template<iter::concepts::container T>
constexpr auto ITER_IMPL(to_iter) (T&& container) {
    return iter::detail::container_iter<T>{std::in_place, FWD(container)};
}
template<iter::iterable T>
requires iter::concepts::container<T> && (std::remove_cvref_t<T>::owner)
constexpr auto ITER_IMPL(cycle) (T&& container) {
    return typename iter::detail::container_iter<T>::cycle{{std::in_place, FWD(container)}};
}

namespace iter::detail {
    template<class T>
    struct optional_to_iter {
        using this_t = optional_to_iter;
        std::optional<T> option;
        constexpr std::size_t ITER_UNSAFE_SIZE (this_t const& self) {
            return self.option ? 1 : 0;
        }
        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t) {
            return *self.option;
        }
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) -> std::optional<T> {
            auto r = std::move(self.option);
            self.option.reset();
            return r;
        }
    };

    template<class T>
    optional_to_iter(std::optional<T>) -> optional_to_iter<T>;
}

template<iter::concepts::optional T>
constexpr auto ITER_IMPL(to_iter) (T&& optional) {
    return iter::detail::optional_to_iter{FWD(optional)};
}

namespace iter {
    template<class T>
    struct pointer_to_iter {
        using this_t = pointer_to_iter;
        T* ptr;

        constexpr std::size_t ITER_UNSAFE_SIZE (this_t const& self) {
            return self.ptr ? 1 : 0;
        }
        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t) {
            return *self.ptr;
        }
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
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
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
            return std::addressof(std::as_const(self.value));
        }
        constexpr auto ITER_UNSAFE_SIZE (this_t const&) {
            return std::numeric_limits<std::size_t>::max();
        }
        constexpr auto ITER_UNSAFE_GET (this_t& self, size_t) -> auto& {
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
        constexpr std::size_t ITER_UNSAFE_SIZE (this_t const&) {
            return 1;
        }
        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t) {
            return (self.value);
        }
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
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
        constexpr std::size_t ITER_UNSAFE_SIZE (this_t const&) {
            return 1;
        }
        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t) {
            return *self.value;
        }
        constexpr decltype(auto) ITER_IMPL_THIS(next) (this_t& self) {
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

#ifndef INCLUDE_ITER_RANGE_HPP
#define INCLUDE_ITER_RANGE_HPP

ITER_DECLARE(until)
ITER_ALIAS(til, until)

namespace iter {
    template<std::integral T = int>
    struct [[nodiscard]] range {
        using this_t = range;
        T begin_;
        T end_ = std::numeric_limits<T>::max();
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
            return self.begin_ < self.end_ ? std::optional(self.begin_++) : std::nullopt;
        }
        constexpr std::size_t ITER_UNSAFE_SIZE (this_t const& self) {
            return self.end_ - self.begin_;
        }
        constexpr T ITER_UNSAFE_GET (this_t& self, std::size_t index) {
            return self.begin_ + index;
        }
    };

    template<class T>
    range{T} -> range<T>;
    template<class T>
    range{T, T} -> range<T>;

    namespace detail {
        struct [[nodiscard]] indices_iter {
            using this_t = indices_iter;
            indices_iter() = default;
        private:
            int i = 0;
            constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
                return std::optional(self.i++);
            }
            constexpr std::size_t ITER_UNSAFE_SIZE (this_t const&) {
                return std::numeric_limits<int>::max();
            }
            constexpr int ITER_UNSAFE_GET (this_t&, std::size_t index) {
                return (int)index;
            }
        };
    }

    static constexpr struct {} indices;
    constexpr auto ITER_IMPL(to_iter) (decltype(indices)) {
        return detail::indices_iter{};
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
        constexpr decltype(auto) ITER_IMPL_THIS(next) (this_t& self) {
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
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) { return self.next(); }

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
    constexpr auto ITER_IMPL(cycle) (F&& invocable, Ts&&... args) {
        return [make_iter = std::forward<F>(invocable), ...as = std::forward<Ts>(args)] () mutable
            -> std::invoke_result_t<F, Ts&...>
        {
            while (true)
                for (auto it = make_iter(static_cast<Ts&>(as)...); auto next = iter::next(it);)
                    co_yield *next;
        }();
    }
    template<std::invocable<> F>
    requires concepts::generator<std::invoke_result_t<F>>
    constexpr std::invoke_result_t<F> ITER_IMPL(cycle) (F&& invocable) {
        for (auto make_iter = std::forward<F>(invocable); true; )
            for (auto it = make_iter(); auto next = iter::next(it);)
                co_yield *next;
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
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
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
            constexpr auto ITER_IMPL_THIS(next) (this_t&) -> T* {
                return nullptr;
            }
            constexpr std::size_t ITER_UNSAFE_SIZE (this_t const&) {
                return 0;
            }
            constexpr auto ITER_UNSAFE_GET (this_t&, std::size_t) -> T& {
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

        constexpr next_t<I> ITER_IMPL_THIS(next) (this_t& self) {
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
    return iter::detail::filter_iter{.i = iter::to_iter(FWD(iterable)), .pred = FWD(pred)};
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

        constexpr auto ITER_IMPL_THIS(next) (this_t& self)
            requires (!this_t::random_access)
        {
            return self.n-- > 0 ? iter::next(self.i) : no_next<I>();
        }

        constexpr auto ITER_UNSAFE_SIZE (this_t const& self)
            requires this_t::random_access
        {
            return std::min(self.n, iter::unsafe::size(self.i));
        }

        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return iter::unsafe::get(self.i, index);
        }
    };

    template<class I>
    take_iter(I, std::size_t) -> take_iter<I>;
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(take) (I&& iterable, std::size_t n) {
    return iter::detail::take_iter{.i = iter::to_iter(FWD(iterable)), .n = n};
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

        constexpr next_t<I> ITER_IMPL_THIS(next) (this_t& self) {
            auto val = iter::next(self.i);
            return val && self.pred(*val) ? val : no_next<I>(); // TODO test RVO
        }
    };

    template<class I, class P>
    take_while_iter(I, P) -> take_while_iter<I, P>;
}

template<iter::assert_iterable I, std::predicate<iter::ref_t<I>> P>
constexpr auto ITER_IMPL(take_while) (I&& iterable, P&& predicate) {
    return iter::detail::take_while_iter{.i = iter::to_iter(FWD(iterable)), .pred = FWD(predicate)};
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
        constexpr auto ITER_IMPL_THIS(next) (this_t& self)
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

        constexpr auto ITER_UNSAFE_SIZE (this_t const& self)
            requires this_t::random_access
        {
            std::size_t size = iter::unsafe::size(self.i);
            return size > self.n ? size - self.n : 0;
        }

        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return iter::unsafe::get(self.i, index + self.n);
        }
    };

    template<class I>
    skip_iter(I, std::size_t) -> skip_iter<I>;
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(skip) (I&& iterable, std::size_t n) {
    return iter::detail::skip_iter{.i = iter::to_iter(FWD(iterable)), .n = n};
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

        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
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
    return iter::detail::skip_while_iter{.i = iter::to_iter(FWD(iterable)), .pred = FWD(pred)};
}

#endif /* INCLUDE_ITER_SKIP_WHILE_HPP */

#ifndef INCLUDE_ITER_MAP_HPP
#define INCLUDE_ITER_MAP_HPP

#ifndef INCLUDE_ITER_FLATTEN_HPP
#define INCLUDE_ITER_FLATTEN_HPP

ITER_DECLARE(flatten)

namespace iter::detail {
    template<assert_iter I>
    struct [[nodiscard]] flatten_iter {
        using this_t = flatten_iter;
        using inner_t = value_t<I>;
        static_assert(iterable<consume_t<I>>);

        constexpr static auto get_current(I& i) {
            if constexpr (iter<inner_t>)
                return iter::next(i);
            else {
                auto val = iter::next(i);
                return val ? MAKE_OPTIONAL(iter::to_iter(consume(val))) : std::nullopt;
            }
        }

        [[no_unique_address]] I i;
        decltype(this_t::get_current(std::declval<I&>())) current{};

        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
            auto val = no_next<iter_t<inner_t>>();
            do {
                if (self.current)
                    if (emplace_next(val, *self.current))
                        return val;
            } while (EMPLACE_NEW(self.current, get_current(self.i)));
            return val;
        }
    };

    template<class I>
    flatten_iter(I) -> flatten_iter<I>;
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(flatten) (I&& iterable) {
    return iter::detail::flatten_iter{.i = iter::to_iter(FWD(iterable))};
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
        static_assert(iterable<invoke_result>);
        using inner_iter_t = iter_t<invoke_result>;

        std::optional<inner_iter_t> current = std::nullopt;
        [[no_unique_address]] I i;
        [[no_unique_address]] F func;

        constexpr auto get_current() {
            auto next = iter::next(i);
            if constexpr (iter<invoke_result>) {
                return next
                    ? MAKE_OPTIONAL(func(consume(next)))
                    : std::nullopt;
            } else {
                return next
                    ? MAKE_OPTIONAL(iter::to_iter(func(consume(next))))
                    : std::nullopt;
            }
        }

        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
            auto val = no_next<inner_iter_t>();
            do {
                if (self.current)
                    if (emplace_next(val, *self.current))
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
    return iter::detail::flatmap_iter{.i = iter::to_iter(FWD(iterable)), .func = FWD(func)};
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

        constexpr mapped_t ITER_IMPL_THIS(next) (this_t& self) {
            auto mapped = mapped_t{};
            while (auto val = iter::next(self.i)) {
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
    return iter::detail::filter_map_iter{.i = iter::to_iter(FWD(iterable)), .func = FWD(func)};
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

        constexpr mapped_t ITER_IMPL_THIS(next) (this_t& self)
            requires (!this_t::random_access)
        {
            auto val = iter::next(self.i);
            if constexpr (concepts::optional<mapped_t>)
                return val ? MAKE_OPTIONAL(self.func(consume(val))) : std::nullopt;
            else
                return val ? std::addressof(self.func(consume(val))) : nullptr;
        }

        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return self.func(iter::unsafe::get(self.i, index));
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
    return iter::detail::map_iter{.i = iter::to_iter(FWD(iterable)), .func = FWD(func)};
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

        constexpr mapped_t ITER_IMPL_THIS(next) (this_t& self) {
            auto val = iter::next(self.i);
            return val ? self.func(consume(val)) : mapped_t{};
        }
    };

    template<class I, class P>
    map_while_iter(I, P) -> map_while_iter<I, P>;
}

template<iter::assert_iterable I, std::invocable<iter::consume_t<I>> F>
constexpr auto ITER_IMPL(map_while) (I&& iterable, F&& func) {
    return iter::detail::map_while_iter{.i = iter::to_iter(FWD(iterable)), .func = FWD(func)};
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

        constexpr auto ITER_IMPL_THIS(next) (this_t& self)
            requires (!this_t::random_access)
        {
            return apply([](auto&&... is) {
                return std::invoke([](auto&&... vals) {
                    return (... && vals)
                        ? MAKE_OPTIONAL(make_tuple_lazy(lazy_unwrap_next(FWD(vals))...))
                        : std::nullopt;
                }, iter::next(is)...);
            }, self.i);
        }

        constexpr auto ITER_UNSAFE_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return apply([=](auto&&... is) {
                return make_tuple_lazy([&]() -> decltype(auto) { return iter::unsafe::get(is, index); }...);
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
            return std::min({iter::unsafe::size(iters)...});
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

ITER_DECLARE(enumerate)

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(enumerate) (I&& iterable) {
    return iter::zip(FWD(iterable), iter::indices);
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
        constexpr std::size_t ITER_UNSAFE_SIZE (this_t const& self)
            requires this_t::random_access
        {
            return std::numeric_limits<std::size_t>::max();
        }

        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return iter::unsafe::get(self.i, index % iter::unsafe::size(self.i));
        }

        constexpr next_t<I> ITER_IMPL_THIS(next) (this_t& self)
            requires (!this_t::random_access)
        {
            auto val = iter::next(self.i);
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
    return iter::detail::cycle_iter{.i = FWD(iter)};
}

template<class I>
constexpr auto ITER_IMPL(cycle) (I&& iterable) {
    static_assert(iter::iterable<I>);
    return iter::cycle(iter::to_iter(FWD(iterable)));
}

#endif /* INCLUDE_ITER_CYCLE_HPP */

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

        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            std::size_t i1s = iter::unsafe::size(*self.i1);
            return index < i1s ? iter::unsafe::get(*self.i1, index) : iter::unsafe::get(self.i2, index - i1s);
        }

        static constexpr bool optional_next = concepts::optional_iter<I1> || concepts::optional_iter<I2>;
        using next_t = std::conditional_t<optional_next, std::optional<value_t<I1>>, value_t<I1>*>;

        constexpr next_t ITER_IMPL_THIS(next) (this_t& self)
            requires (!this_t::random_access)
        {
            auto val = next_t{};
            if (self.i1) {
                if constexpr (optional_next && !concepts::optional_iter<I1>) {
                    if (auto pval = iter::next(*self.i1)) {
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
                if (auto pval = iter::next(self.i2))
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
    if constexpr (iter::concepts::random_access_iterable<I1> && iter::concepts::random_access_iterable<I2>) {
        auto chain = iter::detail::chain_iter{.i1 = MAKE_OPTIONAL(iter::to_iter(FWD(iterable1))), .i2 = iter::to_iter(FWD(iterable2))};
        chain.size = iter::unsafe::size(*chain.i1) + iter::unsafe::size(chain.i2);
        return chain;
    } else {
        return iter::detail::chain_iter{.i1 = MAKE_OPTIONAL(iter::to_iter(FWD(iterable1))), .i2 = iter::to_iter(FWD(iterable2))};
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
    return iter::detail::chunks_iter<std::remove_reference_t<I>, N>{{}, {.i = FWD(iterable)}};
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
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
            while (self.size < N) [[likely]] {
                if (auto next = iter::next(self.i)) [[likely]] {
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
        constexpr next_t<I> ITER_IMPL_THIS(next) (this_t& self)
            requires (!this_t::random_access)
        {
            auto val = iter::next(self.i);
            if (val) self.func(*val);
            return val;
        }

        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            decltype(auto) val = iter::unsafe::get(self.i, index);
            self.func(val);
            return val;
        }
    };

    template<class I, class F>
    inspect_iter(I, F) -> inspect_iter<I, F>;
}

template<iter::assert_iterable I, iter::concepts::inspector<iter::ref_t<I>> F>
constexpr auto ITER_IMPL(inspect) (I&& iterable, F func) {
    return iter::detail::inspect_iter{.i = iter::to_iter(FWD(iterable)), .func = std::move(func)};
}

#endif /* INCLUDE_ITER_INSPECT_HPP */

#ifndef INCLUDE_ITER_TO_POINTER_HPP
#define INCLUDE_ITER_TO_POINTER_HPP

ITER_DECLARE(to_pointer_iter)

namespace iter::detail {
    template<concepts::optional_iter I>
    struct [[nodiscard]] to_pointer_iter {
        using this_t = to_pointer_iter;

        [[no_unique_address]] I i;
        next_t<I> store = std::nullopt;

        constexpr auto ITER_IMPL_THIS(next) (this_t& self) {
            emplace_next(self.store, self.i);
            return self.store ? std::addressof(*self.store) : nullptr;
        }
    };

    template<concepts::optional_iter I>
    requires concepts::random_access_iter<I>
    struct [[nodiscard]] to_pointer_iter<I> : enable_random_access<to_pointer_iter<I>, I> {
        using this_t = to_pointer_iter;

        // If iter::unsafe::get returns a value, then we need to store it to return a pointer to storage
        static constexpr bool get_val = !std::is_reference_v<unsafe::get_t<I>>;
        [[no_unique_address]] I i;
        [[no_unique_address]] std::conditional_t<get_val, next_t<I>, void_t> store;

        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t index) {
            if constexpr (get_val) {
                EMPLACE_NEW(self.store, MAKE_OPTIONAL(iter::unsafe::get(self.i, index)));
                return *self.store;
            } else
                return iter::unsafe::get(self.i, index);
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
        return iter::detail::to_pointer_iter{.i = FWD(iter)};
    }
}

#endif /* INCLUDE_ITER_TO_POINTER_HPP */

#ifndef INCLUDE_ITER_MOVE_HPP
#define INCLUDE_ITER_MOVE_HPP

ITER_DECLARE(move)

namespace iter::detail {
    template<iter::assert_iter I>
    struct move_iter : enable_random_access<move_iter<I>, I> {
        [[no_unique_address]] I i;

        using this_t = move_iter;

        constexpr move_next<next_t<I>> ITER_IMPL_THIS(next) (this_t& self)
            requires (!this_t::random_access)
        {
            return move_next{iter::next(self.i)};
        }

        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            decltype(auto) item = iter::unsafe::get(self.i, index);
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
        return iter::detail::move_iter{.i = iter::to_iter(FWD(iterable))};
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
        constexpr auto ITER_UNSAFE_GET (this_t& self, std::size_t index) { return self.get(index); }
        constexpr auto ITER_UNSAFE_SIZE (this_t const& self) { return self.size(); }
    };
    template<concepts::next Next>
    struct virtual_iter<Next, void> {
        virtual Next next() = 0;
        virtual ~virtual_iter() = default;
    private:
        using this_t = virtual_iter;
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) { return self.next(); }
    };

    namespace detail {
        template<iter I>
        struct virtual_iter_impl final : virtual_iter<next_t<I>>, I {
            template<class... Ts>
            constexpr virtual_iter_impl(Ts&&... in) : I{FWD(in)...} {}
            next_t<I> next() final { return iter::next(static_cast<I&>(*this)); }
        };
        template<concepts::random_access_iter I>
        struct virtual_iter_impl<I> final : virtual_iter<next_t<I>, unsafe::get_t<I>>, I {
            template<class... Ts>
            constexpr virtual_iter_impl(Ts&&... in) : I{FWD(in)...} {}
            next_t<I> next() final { return iter::next(static_cast<I&>(*this)); }
            std::size_t size() const final {
                return iter::unsafe::size(static_cast<I const&>(*this));
            }
            unsafe::get_t<I> get(std::size_t index) final {
                return iter::unsafe::get(static_cast<I&>(*this), index);
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
        constexpr Next ITER_IMPL_THIS(next) (this_t& self) { return self.it->next(); }
        constexpr std::size_t ITER_UNSAFE_SIZE (this_t const& self) requires random_access {
            return self.it->size();
        }
        constexpr Get ITER_UNSAFE_GET (this_t& self, std::size_t index) requires random_access {
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
    while (auto val = iter::next(iter)) {
        func(iter::detail::consume(val));
    }
}

template<iter::assert_iterable I>
constexpr void ITER_IMPL(foreach) (I&& iterable) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (iter::next(iter)) {}
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
    while (auto val = iter::next(iter)) {
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
    auto acc = iter::next(iter);
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
    while (auto val = iter::next(iter)) {
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
    while (auto val = iter::next(iter)) {
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
    std::size_t size = iter::unsafe::size(iter);
    using get_t = decltype(iter::unsafe::get(iter, size - 1));
    if constexpr (std::is_lvalue_reference_v<decltype(iter)> && std::is_reference_v<get_t>)
        return size > 0 ? std::addressof(iter::unsafe::get(iter, size - 1)) : nullptr;
    else
        return size > 0 ? MAKE_OPTIONAL(iter::unsafe::get(iter, size - 1)) : std::nullopt;
}

template<iter::concepts::random_access_iterable I, class T>
constexpr auto ITER_IMPL(last) (I&& iterable, T&& fallback) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    std::size_t size = iter::unsafe::size(iter);
    return size > 0 ? iter::unsafe::get(iter, size - 1) : FWD(fallback);
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
    while (auto val = iter::next(iter)) {
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
    while (auto val = iter::next(iter)) {
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
    std::size_t size = iter::unsafe::size(iter);
    using get_t = decltype(iter::unsafe::get(iter, n));
    if constexpr (std::is_lvalue_reference_v<decltype(iter)> && std::is_reference_v<get_t>)
        return size > n ? std::addressof(iter::unsafe::get(iter, n)) : nullptr;
    else
        return size > n ? MAKE_OPTIONAL(iter::unsafe::get(iter, n)) : std::nullopt;
}

template<iter::concepts::random_access_iterable I, class T>
constexpr auto ITER_IMPL(nth) (I&& iterable, std::size_t n, T&& fallback) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    std::size_t size = iter::unsafe::size(iter);
    return size > n ? iter::unsafe::get(iter, n) : FWD(fallback);
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
        auto emplace_next = [&] { return val = iter::next(iter); };
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
        auto emplace_next = [&] { return val = iter::next(iter); };
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
        return iter::next(fm);
    else {
        auto val = iter::next(fm);
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
    while (auto val = iter::next(iter)) {
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
    while (auto val = iter::next(iter)) {
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
        template<template<class...> class C = std::vector, template<class> class A = std::allocator>
        struct collect : xtd::tagged_bindable<collect<C, A>, xtd::invokers::iter_collect> {};
    }

    template<template<class...> class C = std::vector, template<class> class A = std::allocator>
    static constexpr tag::collect<C, A> collect;
}

ITER_ALIAS(to_vector, collect<std::vector>)
ITER_ALIAS(to_map, collect<std::map>)

template<template<class...> class CT, template<class> class AT, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_collect, iter::tag::collect<CT, AT>)(I&& iter) {
    using T = iter::value_t<I>;
    using A = AT<T>;
    CT<T, A> container;
    if constexpr (iter::concepts::random_access_iter<I>) {
        container.reserve(iter::unsafe::size(iter));
    }
    while (auto val = iter::next(iter)) {
        container.emplace_back(iter::detail::consume(val));
    }
    return container;
}

template<template<class...> class CT, template<class> class AT, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_collect, iter::tag::collect<CT, AT>)(I&& iter, std::size_t reserve) {
    using T = iter::value_t<I>;
    using A = AT<T>;
    CT<T, A> container;
    if constexpr (iter::concepts::random_access_iter<I>) {
        reserve = std::max(reserve, iter::unsafe::size(iter));
    }
    container.reserve(reserve);
    while (auto val = iter::next(iter)) {
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
    while (auto val = iter::next(iter)) {
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
        std::size_t size = iter::unsafe::size(iter) / N;
        apply([=](auto&&... outs) { (outs.reserve(size), ...); }, out);
    }

    while (auto val = iter::next(iter)) {
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
        apply([size = iter::unsafe::size(iter)](auto&... c) {
            (c.reserve(size), ...);
        }, containers);
    }
    while (auto val = iter::next(iter)) {
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
        reserve = std::max(reserve, iter::unsafe::size(iter));

    apply([=](auto&... c) {
        (c.reserve(reserve), ...);
    }, containers);

    while (auto val = iter::next(iter)) {
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
constexpr bool operator==(I1&& i1, I2&& i2) {
    auto item1 = iter::no_next<I1>();
    auto item2 = iter::no_next<I2>();

    while (!!iter::detail::emplace_next(item1, i1) & !!iter::detail::emplace_next(item2, i2)) {
        if (*item1 != *item2) return false;
    }

    return !!item1 == !!item2;
}

template<iter::concepts::random_access_iter I1, iter::concepts::random_access_iter I2>
constexpr bool operator==(I1&& i1, I2&& i2) {
    auto size = iter::unsafe::size(i1);
    if (size != iter::unsafe::size(i2)) return false;

    for (std::size_t i = 0; i < size; ++i) {
        decltype(auto) item1 = iter::unsafe::get(i1, i);
        decltype(auto) item2 = iter::unsafe::get(i2, i);
        if (item1 != item2) return false;
    }

    return true;
}

template<iter::iter I1, iter::iter I2>
constexpr auto operator<=>(I1&& i1, I2&& i2) {
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
    template<assert_iterable I>
    struct [[nodiscard]] wrap : wrap<iter_t<I>> {
        template<class II>
        wrap(II&& iterable) : wrap<iter_t<I>>{to_iter(FWD(iterable))} {}
    };
    template<iter I>
    struct [[nodiscard]] wrap<I> {
        [[no_unique_address]] I underlying;

        using this_t = wrap;
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) { return iter::next(self.underlying); }
        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t i)
            requires concepts::random_access_iter<I>
        {
            return iter::unsafe::get(self.underlying, i);
        }
        constexpr std::size_t ITER_UNSAFE_SIZE (this_t& self)
            requires concepts::random_access_iter<I>
        {
            return iter::unsafe::size(self.underlying);
        }

#define ITER_X(fun) \
        template<class... Ts>\
        decltype(auto) fun(Ts&&... args) {\
            return invoke(::iter::fun, std::forward<Ts>(args)...);\
        }
/* Do not modify, generated by scripts/update_x_macros.sh */

// Invoke iter::next on this iter
ITER_X(next)
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
// Invoke iter::enumerate on this iter
ITER_X(enumerate)
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
        decltype(auto) fun(Ts&&... args) {\
            return invoke(::iter::fun<ITER_EXPAND tmplArgs>, std::forward<Ts>(args)...);\
        }
/* Do not modify, generated by scripts/update_x_macros.sh */

// Invoke iter::N on this iter
ITER_X(chunks_, (std::size_t N = 0), (N))
// Invoke iter::N on this iter
ITER_X(window, (std::size_t N = 2), (N))
// Invoke iter::C, A on this iter
ITER_X(collect, (template<class...> class C = std::vector, template<class> class A = std::allocator), (C, A))
// Invoke iter::N on this iter
ITER_X(partition_, (std::size_t N = 2), (N))
// Invoke iter::C, A on this iter
ITER_X(unzip_, (template<class...> class C = std::vector, template<class> class A = std::allocator), (C, A))
// Invoke iter::C, A on this iter
ITER_X(sorted_, (template<class...> class C = std::vector, template<class> class A = std::allocator), (C, A))

#undef ITER_EXPAND
#undef ITER_X

        template<xtd::concepts::Bindable Tag, class... Ts>
        decltype(auto) invoke(Tag const& tag, Ts&&... args) {
            auto call = [&]() -> decltype(auto) { return tag(underlying, std::forward<Ts>(args)...); };
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

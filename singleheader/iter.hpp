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

#ifndef INCLUDE_ITER_ITER_HPP
#define INCLUDE_ITER_ITER_HPP

#ifndef ITER_ITERS_ITERS_HPP
#define ITER_ITERS_ITERS_HPP

#ifndef ITER_ITERS_RANDOM_ACCESS_CONTAINER_ITER_HPP
#define ITER_ITERS_RANDOM_ACCESS_CONTAINER_ITER_HPP

#ifndef ITER_CORE_CORE_HPP
#define ITER_CORE_CORE_HPP

#ifndef ITER_LIBRARY_VERSION
#  define ITER_LIBRARY_VERSION 20220618
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
inline constexpr bool xtd_invoker_defined_in_root_namespace = false;
}

namespace xtd {

namespace detail {
    template<auto U = []{}>
    using unique_t = decltype(U);
}

namespace invokers {
    inline constexpr bool xtd_invoker_defined_in_root_namespace = true;

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
    inline constexpr bool is_bindable = false;

    template<class U, class... Fs>
    inline constexpr bool is_bindable<bindable<U, Fs...>> = true;

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
    inline constexpr bool is_bind_placeholder = false;
    template<size_t I>
    inline constexpr bool is_bind_placeholder<bind_placeholder<I>> = true;

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
    inline constexpr bool is_bound = false;
    template<class F, std::size_t A, bool O>
    inline constexpr bool is_bound<bound<F, A, O>> = true;

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
        constexpr explicit(false) sink(auto&&) {}
    };

    template<std::size_t... Is>
    constexpr decltype(auto) get_value(sink<Is>..., auto&& value, auto&&...) {
        return FWD(value);
    }

    template<std::size_t I, class... Ts>
    constexpr decltype(auto) get_i(Ts&&... args) {
        return [&]<std::size_t... Is>(std::index_sequence<Is...>) -> decltype(auto) {
            return get_value<(0*Is)...>(FWD(args)...);
        }(std::make_index_sequence<I>{});
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
    [[maybe_unused]] inline constexpr bind_placeholder _;

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

#include <compare>
#include <cstdint>
#include <limits>
#include <memory>
#include <utility>

#ifndef ITER_CORE_MACROS_HPP
#define ITER_CORE_MACROS_HPP

#ifndef ITER_GLOBAL_INVOKER
#   define ITER_INVOKER(name)          XTD_INVOKER(iter_ ## name)
#   define ITER_FUNCTION(fun)          XTD_FUNCTION_(iter_ ## fun) fun
#   define ITER_IMPL(name)             XTD_IMPL_(iter_ ## name, iter::name)
#   define ITER_IMPL_THIS(name)        XTD_IMPL_THIS_(iter_ ## name, iter::name)
#   define ITER_DETAIL_IMPL(name)      XTD_IMPL_(iter_ ## name, iter::detail::impl::name)
#   define ITER_DETAIL_IMPL_THIS(name) XTD_IMPL_THIS_(iter_ ## name, iter::detail::impl::name)
#else
#   warning Overload resolution is more complex with ITER_GLOBAL_INVOKER. \
            Any failing invocations may return an endless list of candidates.
#   define ITER_INVOKER(name)
#   define ITER_FUNCTION(fun)          XTD_FUNCTION fun
#   define ITER_IMPL(name)             XTD_IMPL(iter::name)
#   define ITER_IMPL_THIS(name)        XTD_IMPL_THIS(iter::name)
#   define ITER_DETAIL_IMPL(name)      XTD_IMPL(iter::detail::impl::name)
#   define ITER_DETAIL_IMPL_THIS(name) XTD_IMPL_THIS(iter::detail::impl::name)
#endif

#define ITER_IMPL_NEXT      ITER_DETAIL_IMPL_THIS(next)
#define ITER_IMPL_NEXT_BACK ITER_DETAIL_IMPL_THIS(next_back)
#define ITER_IMPL_GET       XTD_IMPL_THIS_(iter_get, iter::detail::impl::get)
#define ITER_IMPL_SIZE      ITER_DETAIL_IMPL_THIS(size)

#define ITER_DECLARE(fun) \
    ITER_INVOKER(fun)\
    namespace iter {\
        ITER_FUNCTION(fun);\
    }

#define ITER_ALIAS(alias, ... /*of*/) \
    namespace iter {\
        inline constexpr auto& alias = __VA_ARGS__;\
    }

#if defined(__clang__)
#   define ITER_COMPILER_CLANG
#   define ITER_ASSUME(condition) __builtin_assume(!!(condition))
#   define ITER_UNREACHABLE() __builtin_unreachable()
#elif defined(__GNUC__) || defined (__GNUG__)
#   define ITER_COMPILER_GCC
#   define ITER_ASSUME(condition) do { if(!(condition)) __builtin_unreachable(); } while(0)
#   define ITER_UNREACHABLE() __builtin_unreachable()
#elif defined(_MSC_VER)
#   define ITER_COMPILER_MSVC
#   define ITER_ASSUME(condition) __assume(!!(condition))
#   define ITER_UNREACHABLE() __assume(0)
#else
#   define ITER_ASSUME(condition) do { } while(0)
#   define ITER_UNREACHABLE() do { } while(0)
#endif

#if __cpp_impl_coroutine >= 201902L
#   define ITER_COROUTINE
#endif

#endif /* ITER_CORE_MACROS_HPP */

#ifndef INCLUDE_ITER_EMPLACE_NEW_HPP
#define INCLUDE_ITER_EMPLACE_NEW_HPP

// Semantically equivalent to `current = expr`, but constructs expr
// directly inside current's memory without any intermediate moves
#define EMPLACE_NEW(current, ... /*expr*/) \
    ::iter::detail::emplace_new_impl(current, [&]<class T_UGLY>() { return T_UGLY(__VA_ARGS__); })

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
        current.~T();
        if (std::is_constant_evaluated()) {
            // placement new not strictly speaking constexpr although GCC allows it
            std::construct_at(std::addressof(current), FWD(ctor).template operator()<T>());
        } else {
            new (std::addressof(current), constexpr_new_tag{}) T(FWD(ctor).template operator()<T>());
        }
        return current;
    }
}

#endif /* INCLUDE_ITER_EMPLACE_NEW_HPP */

#ifndef ITER_CORE_ITEM_HPP
#define ITER_CORE_ITEM_HPP

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

#define MAKE_STABILITY(stable, ...) ::iter::make_stability<stable>([&] { return (__VA_ARGS__); })
#define MAKE_STABILITY_AUTO(stable, ...) ::iter::make_stability<stable>([&]() -> decltype(auto) { return (__VA_ARGS__); })

#endif /* ITER_CORE_STABILITY_HPP */

// GCC cannot deal with empty payloads in debug
#if defined(NDEBUG) || !defined(ITER_COMPILER_GCC)
#   define ITER_ITEM_ABI_PAYLOAD_ATTRIBUTE [[no_unique_address]]
#   define ITER_ITEM_ABI_NAMESPACE iter
#   define ITER_ITEM_ABI_IMPORT
#else
#   define ITER_ITEM_ABI_PAYLOAD_ATTRIBUTE
#   define ITER_ITEM_ABI_NAMESPACE iter::detail::item_abi
#   define ITER_ITEM_ABI_IMPORT using ITER_ITEM_ABI_NAMESPACE::item;
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
#   define ITER_CONSTEXPR_NULLPTR ::iter::detail::constexpr_nullptr
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
#   define ITER_CONSTEXPR_NULLPTR nullptr
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

    using stability_t = typename iter::stability_wrapper<stable>::template type<T>;

    constexpr item(stability_t ref) : ptr(std::addressof(get(ref))) {}
    template<std::invocable F>
    requires std::constructible_from<stability_t, std::invoke_result_t<F>>
    constexpr explicit item(F&& f) : ptr(detail::addressof(get(stability_t{std::invoke(FWD(f))}))) {}

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
    requires std::constructible_from<stability_t, std::invoke_result_t<F>>
    item& operator=(F&& f) { return emplace(FWD(f)); }
    template<std::invocable F>
    requires std::constructible_from<stability_t, std::invoke_result_t<F>>
    item& emplace(F&& f) { ptr = std::addressof(get(stability_t{std::invoke(FWD(f))})); return *this; }

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
using make_item_t = item<detail::stability_unwrap<T>, concepts::stable<T>>;

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
        template<class... Ts> inline constexpr bool is_tuple<iter::tuple<Ts...>> = true;
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

ITER_DECLARE(to_iter)
ITER_DECLARE(cycle)

ITER_INVOKER(next)
ITER_INVOKER(next_back)
namespace xtd::invokers { struct iter_get; }
ITER_INVOKER(size)

namespace iter {
    // customisation points
    namespace traits {
        ITER_FUNCTION(next);
        namespace double_ended {
            ITER_FUNCTION(next_back);
        }
        namespace random_access {
            XTD_FUNCTION_(iter_get) get;
            ITER_FUNCTION(size);
        }
    }

    // import and flatten all traits into detail::impl
    namespace detail::impl {
        using namespace iter::traits;
        using namespace double_ended;
        using namespace random_access;
    }

    namespace concepts {
        template<class T>
        concept iter = requires(T it) {
            { iter::traits::next(it) } -> item;
        };

        template<class T>
        concept stable_iter = iter<T> && requires(T it) {
            { iter::traits::next(it) } -> stable_item;
        };

        template<class T>
        concept random_access_iter = iter<T> && requires (T it, std::size_t index) {
            { iter::traits::random_access::get(it, index) } -> stability_wrapper;
            { iter::traits::random_access::size(it) } -> std::same_as<std::size_t>;
        };

        template<class T>
        concept double_ended_iter = iter<T> && requires (T it, std::size_t index) {
            { iter::traits::double_ended::next_back(it) } -> std::same_as<decltype(iter::traits::next(it))>;
        };

        template<class T>
        concept iterable = iter<T> || requires (T&& it) {
            { iter::to_iter(FWD(it)) } -> iter;
        };

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
        template<iterable I>
        auto force_iter() -> std::remove_cvref_t<decltype(iter::to_iter(std::declval<I>()))>;

        template<iter I>
        auto force_iter() -> std::remove_cvref_t<I>;
    }

    template<class I>
    using iter_t = decltype(detail::force_iter<I>());

    namespace concepts {
        template<class T>
        concept random_access_iterable = iterable<T> && random_access_iter<iter_t<T>>;
    }

    template<iterable I>
    using next_t = decltype(traits::next(std::declval<iter_t<I>&>()));

    template<iter I>
    struct iterator_traits {
        using next_t = iter::next_t<I>;
        using iter_t = I;
        using wrapped_type = typename next_t::wrapped_type;
        using value_type = typename next_t::value_type;
        using reference = typename next_t::reference;
        using pointer = typename next_t::pointer;
        using difference_type = std::ptrdiff_t; // only to fulfill ranges concept
        using iterator_category = std::input_iterator_tag;
    };

    namespace detail {
        template<class T, iter I>
        constexpr T& emplace_next(T& current, I& it) {
            return EMPLACE_NEW(current, impl::next(it));
        }

        inline constexpr struct sentinel_t {} sentinel;

        // Minimal implementation to support range-based for loop syntax
        template<iter I>
        struct range_for_wrapper {
            explicit constexpr range_for_wrapper(auto&& it) : i{FWD(it)} {}

            auto operator<=>(const range_for_wrapper&) const = delete;

            constexpr bool operator!=(sentinel_t) { return emplace_next(current, i).has_value(); }
            constexpr bool operator==(sentinel_t) { return !operator!=(sentinel); }
            constexpr auto& operator*() { return *current; }
            constexpr auto& operator*() const { return *current; }
            constexpr auto* operator->() { return std::addressof(*current); }
            constexpr auto* operator->() const { return std::addressof(*current); }
            constexpr void operator++() {}
            constexpr void operator++(int) {}

        private:
            I i;
            next_t<I> current;
        };

        template<class T>
        range_for_wrapper(T) -> range_for_wrapper<T>;

        template<iter I>
        constexpr auto begin(I&& iter) {
            return range_for_wrapper{FWD(iter)};
        }

        template<iter I>
        constexpr auto end(I&&) {
            return sentinel;
        }
    }

    using detail::begin;
    using detail::end;

    template<class T>
    static constexpr auto&& as_const(T&& in) {
        if constexpr (std::is_lvalue_reference_v<T>)
            return std::as_const(in);
        else
            return static_cast<T const&&>(in);
    }

    namespace concepts {
        template<class T>
        inline constexpr bool is_iterator_v = false;
        template<class I>
        inline constexpr bool is_iterator_v<iter::detail::range_for_wrapper<I>> = true;

        template<class T>
        concept iterator = is_iterator_v<std::decay_t<T>>;
    }

    namespace detail {
        constexpr auto consume = [](concepts::item auto& next) -> decltype(auto) {
            return next.consume();
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
    template<iter I>
    using item_t = typename next_t<I>::wrapped_type;
    template<class T>
    using ref_t = typename detail::iter_traits<T>::ref_t;
    template<class T>
    using cref_t = typename detail::iter_traits<T>::cref_t;
    template<class T>
    using consume_t = typename detail::iter_traits<T>::consume_t;

    template<iterable I>
    static constexpr next_t<I> no_next() { return {}; }

    namespace detail {
        template<class I>
        auto get_type() -> void;
        template<concepts::random_access_iter I>
        auto get_type() -> decltype(iter::traits::random_access::get(std::declval<I&>(), 0ul));
        template<class I>
        using get_t = decltype(get_type<I>());

        template<class I>
        [[nodiscard]] constexpr auto get_item(I& iter, std::size_t index, std::size_t size) {
            return (index < size) ? MAKE_ITEM_AUTO(impl::get(iter, index)) : noitem;
        }

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
                auto& self = static_cast<Self&>(base);
                auto size = impl::size(self);
                auto index = base.index++;
                return get_item(self, index, size);
            }
            constexpr auto ITER_IMPL_NEXT_BACK (this_t& base) {
                auto& self = static_cast<Self&>(base);
                auto size = impl::size(self);
                auto index = base.index++;
                return get_item(self, size - 1 - index, size);
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
                auto& self = static_cast<Self&>(base);
                auto size = impl::size(self);
                auto index = base.index++;
                return get_item(self, index, size);
            }
            constexpr auto ITER_IMPL_NEXT_BACK (this_t& base) {
                auto& self = static_cast<Self&>(base);
                auto size = impl::size(self);
                auto index = base.index++;
                return get_item(self, size - 1 - index, size);
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
void XTD_IMPL_(iter_get, iter::traits::random_access::get) (Ts&&...) = delete;
template<class... Ts>
void ITER_DETAIL_IMPL(size) (Ts&&...) = delete;

struct xtd::invokers::iter_get
{
    static constexpr iter::concepts::stability_wrapper
    auto invoke(auto&&... args) requires requires { xtd_invoke_iter_get(FWD(args)...); }
    {
        auto call = [&]() -> decltype(auto) { return xtd_invoke_iter_get(FWD(args)...); };
        using result_t = decltype(call());
        if constexpr (iter::concepts::stability_wrapper<result_t>) {
            return call();
        } else {
            static_assert(!std::is_reference_v<result_t>, "References must be stability qualified");
            return iter::make_stability(call);
        }
    }
};

#endif /* ITER_CORE_CORE_HPP */

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

#ifndef ITER_ITERS_SPAN_HPP
#define ITER_ITERS_SPAN_HPP

namespace iter {
    template<class T>
    struct span {
        using this_t = span;
        constexpr span(T* data, std::size_t size) : data{data}, remaining{size} {}

        template<concepts::random_access_container C>
        constexpr explicit span(C& container) : span{std::addressof(container[0]), std::size(container)} {}

        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            return self.remaining
                ? (--self.remaining, item(stable_ref(*self.data++)))
                : noitem;
        }
        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
            return self.remaining
                ? item(stable_ref(self.get(--self.remaining)))
                : noitem;
        }
        constexpr std::size_t ITER_IMPL_SIZE (this_t const& self) {
            return self.remaining;
        }
        constexpr auto ITER_IMPL_GET (this_t const& self, std::size_t n) {
            return stable_ref(self.get(n));
        }
    private:
        union {
            T* data; // truly active member
            T (*data_as_array)[]; // used as hint to optimiser during random access reads
        };
        std::size_t remaining;

        constexpr T& get(std::size_t i) const {
            if (std::is_constant_evaluated())
                return data[i]; // avoid type punning in consteval
            else
                return (*data_as_array)[i];
        }
    };

    template<class T>
    span(T*, std::size_t) -> span<T>;
    template<concepts::random_access_container C>
    span(C&) -> span<std::remove_reference_t<decltype(std::declval<C&>()[0])>>;
}

#endif /* ITER_ITERS_SPAN_HPP */

namespace iter::iters { using iter::span; }
#ifndef ITER_ITERS_OWNING_ITER_HPP
#define ITER_ITERS_OWNING_ITER_HPP

#ifndef ITER_CORE_ASSERT_CONSTEVAL_HPP
#define ITER_CORE_ASSERT_CONSTEVAL_HPP

namespace iter::detail {
    class unconstructible { unconstructible(auto&&...); };
    template<class...>
    struct fail_compile_at_linker {
        // Impossible to define anywhere, so will always fail to link
        static unconstructible undefinable();
    };
    struct consteval_function_invoked_at_runtime;
    template<class... Ts>
    static constexpr void assert_consteval() {
        // Fail to compile at linker stage if this will be called at runtime.
        // Cannot fail any earlier, as it will prevent genuine constexpr calls.
        if (!std::is_constant_evaluated())
            fail_compile_at_linker<consteval_function_invoked_at_runtime, Ts...>::undefinable();
    }
}

#endif /* ITER_CORE_ASSERT_CONSTEVAL_HPP */

#ifndef ITER_CORE_RELOCATION_HPP
#define ITER_CORE_RELOCATION_HPP

// Disable relocation (copying/moving) unless it is completely elided
// or executed in a constexpr context
namespace iter {
    namespace detail {
        template<class>
        struct non_copiable;
        template<class>
        struct non_movable;
        template<class>
        struct non_relocatable;
    }
    namespace tag {
        inline constexpr struct non_copiable_t {
            template<class T>
            using type = detail::non_copiable<T>;
        } non_copiable;
        inline constexpr struct non_movable_t {
            template<class T>
            using type = detail::non_movable<T>;
        } non_movable;
        inline constexpr struct non_relocatable_t {
            template<class T>
            using type = detail::non_relocatable<T>;
        } non_relocatable;
        inline constexpr struct relocatable_t {
            template<class T>
            using type = void_t;
        } relocatable;
        namespace detail {
            template<class T>
            inline constexpr bool is_relocation = false;
            template<>
            inline constexpr bool is_relocation<tag::non_copiable_t> = true;
            template<>
            inline constexpr bool is_relocation<tag::non_movable_t> = true;
            template<>
            inline constexpr bool is_relocation<tag::non_relocatable_t> = true;
            template<>
            inline constexpr bool is_relocation<tag::relocatable_t> = true;
        }
        namespace concepts {
            template<class T>
            concept relocation = detail::is_relocation<std::remove_cvref_t<T>>;
        }
    }
    namespace detail {
        template<class>
        struct non_copiable {
            constexpr non_copiable(tag::non_copiable_t){}
            non_copiable() = default;
            constexpr non_copiable(non_copiable const&) { detail::assert_consteval<non_copiable>(); }
            non_copiable(non_copiable&&) = default;
        };
        template<class>
        struct non_movable {
            constexpr non_movable(tag::non_movable_t){}
            non_movable() = default;
            non_movable(non_movable const&) = default;
            constexpr non_movable(non_movable&&) { detail::assert_consteval<non_movable>(); }
        };
        template<class T>
        struct non_relocatable : non_copiable<T>, non_movable<T> {
            constexpr non_relocatable(tag::non_relocatable_t){}
            non_relocatable() = default;
        };
    }
}

#endif /* ITER_CORE_RELOCATION_HPP */

namespace iter {
    // Owning iter that is disallowed from being relocated (copied/moved)
    // unless the relocation is elided or in a constant expression
    template<class T, tag::concepts::relocation Tag = tag::non_relocatable_t>
    struct owning_iter;

    template<concepts::random_access_container T, tag::concepts::relocation Tag>
    struct owning_iter<T, Tag> {
        using this_t = owning_iter;

        T container;
        [[no_unique_address]] typename Tag::template type<this_t> _relocation_enforcement{};
        std::size_t pos = 0;

        constexpr auto ITER_IMPL_NEXT(this_t& self) {
            return self.pos < std::size(self.container)
                ? item(stable_ref(self.container[self.pos++]))
                : noitem;
        }

        constexpr auto ITER_IMPL_NEXT_BACK(this_t& self) {
            const auto size = std::size(self.container);
            return self.pos < size
                ? item(stable_ref(size - 1 - self.container[self.pos++]))
                : noitem;
        }

        constexpr auto ITER_IMPL_GET(this_t& self, std::size_t index) {
            return stable_ref(self.container[index]);
        }
        constexpr decltype(auto) ITER_IMPL_SIZE(this_t const& self) {
            return std::size(self.container);
        }
    };

    template<class T>
    owning_iter(T) -> owning_iter<T>;
    template<class T, class Tag>
    owning_iter(T, Tag) -> owning_iter<T, Tag>;
}

#endif /* ITER_ITERS_OWNING_ITER_HPP */

namespace iter::iters { using iter::owning_iter; }
#ifndef ITER_ITERS_ONCE_HPP
#define ITER_ITERS_ONCE_HPP

#ifndef INCLUDE_ITER_REPEAT_HPP
#define INCLUDE_ITER_REPEAT_HPP

namespace iter {
    template<class T>
    struct repeat {
        using this_t = repeat;
        T value;
        constexpr auto ITER_IMPL_NEXT (this_t const& self) {
            return item(stable_ref(self.value));
        }
        constexpr auto ITER_IMPL_SIZE (this_t const&) {
            return std::numeric_limits<std::size_t>::max();
        }
        constexpr auto ITER_IMPL_GET (this_t const& self, size_t) {
            return stable_ref(self.value);
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
        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t) {
            return stable_ref(self.value);
        }
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            return std::exchange(self.on, false) ? item(stable_ref(self.value)) : noitem;
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
        constexpr once_ref(T& in) : value{stable_ref(in)} {}
    private:
        stable_item<T&> value;
        constexpr std::size_t ITER_IMPL_SIZE (this_t const&) {
            return 1;
        }
        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t) {
            return stable_ref(*self.value);
        }
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            return std::exchange(self.value, noitem);
        }
        constexpr auto ITER_IMPL_THIS(cycle) (const this_t& self) {
            return repeat<T const&>{*self.value};
        }
    };

    template<class F>
    once_ref(F&) -> once_ref<F>;
}

#endif /* ITER_ITERS_ONCE_HPP */

namespace iter::iters { using iter::once; }
#ifndef ITER_ITERS_OPTIONAL_HPP
#define ITER_ITERS_OPTIONAL_HPP

namespace iter {
    template<class T>
    struct optional : unstable_item<T> {
        using this_t = optional;

        constexpr std::size_t ITER_IMPL_SIZE (this_t const& self) {
            return self ? 1 : 0;
        }
        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t) {
            return stable_ref(*self);
        }
        constexpr auto ITER_IMPL_NEXT (unstable_item<T>& self) {
            return std::exchange(self, noitem);
        }
        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
            return traits::next(self);
        }
    };

    template<class T>
    optional(T) -> optional<T>;

    namespace concepts {
        template<class T>
        inline constexpr bool is_iter_of_optional = false;
        template<class T>
        inline constexpr bool is_iter_of_optional<iter::optional<T>> = true;
        template<class T>
        concept iter_of_optional = is_iter_of_optional<T>;
    }
}

#endif /* ITER_ITERS_OPTIONAL_HPP */

namespace iter::iters { using iter::optional; }
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
                return self.begin_ <= self.end_ ? item(self.begin_++) : noitem;
            else
                return self.begin_ < self.end_ ? item(self.begin_++) : noitem;
        }
        constexpr std::size_t ITER_IMPL_SIZE (this_t const& self) {
            if constexpr (Inclusive)
                return 1ul + self.end_ - self.begin_;
            else
                return self.end_ - self.begin_;
        }
        constexpr stable<T> ITER_IMPL_GET (this_t const& self, std::size_t index) {
            return {self.begin_ + index};
        }
        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
            if constexpr (Inclusive)
                return self.begin_ <= self.end_ ? item(self.end_--) : noitem;
            else
                return self.begin_ < self.end_ ? item(--self.end_) : noitem;
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
                return item{self.i++};
            }
            constexpr auto ITER_IMPL_NEXT_BACK (this_t& self) {
                return item{std::numeric_limits<T>::max() - self.i++};
            }
            constexpr std::size_t ITER_IMPL_SIZE (this_t const&) {
                return std::numeric_limits<T>::max();
            }
            constexpr stable<T> ITER_IMPL_GET (this_t const&, std::size_t index) {
                return {index};
            }
        };

        template<std::integral T = std::size_t>
        struct indices_tag {};
        template<class T>
        constexpr auto ITER_IMPL(to_iter) (detail::indices_tag<T>) {
            return indices_iter<T>{};
        }
    }

    template<class T = std::size_t>
    inline constexpr detail::indices_tag<T> indices_ = {};
    inline constexpr auto indices = indices_<>;
}

template<std::integral T>
constexpr auto ITER_IMPL(until) (T begin, T end) {
    return iter::range{begin, end};
}

#endif /* INCLUDE_ITER_RANGE_HPP */

namespace iter::iters { using iter::range; }
#ifndef ITER_ITERS_GENERATE_HPP
#define ITER_ITERS_GENERATE_HPP

namespace iter {
    template<std::invocable F>
    requires concepts::item<std::invoke_result_t<F>>
    struct [[nodiscard]] generate : F {
        using this_t = generate;
        constexpr decltype(auto) ITER_IMPL_NEXT (this_t& self) {
            return self();
        }
    };

    template<class F>
    generate(F) -> generate<F>;
}

#endif /* ITER_ITERS_GENERATE_HPP */

namespace iter::iters { using iter::generate; }
#ifndef ITER_ITERS_GENERATOR_HPP
#define ITER_ITERS_GENERATOR_HPP

#ifdef ITER_COROUTINE

#include <coroutine>

namespace iter {
    template<class T>
    class generator;

    namespace concepts {
        namespace detail {
            template<class T>
            inline constexpr bool is_generator = false;
            template<class T>
            inline constexpr bool is_generator<generator<T>> = true;
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

            void unhandled_exception() { throw; }

            constexpr void return_void() {}

            constexpr reference_type value() const noexcept {
                return static_cast<reference_type>(*m_value);
            }

            // Disallow co_await
            std::suspend_never await_transform(auto&& value) = delete;

        private:
            pointer_type m_value;
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

        constexpr unstable_item<T&> next() {
            if (!m_coroutine) [[unlikely]]
                return noitem;

            m_coroutine.resume();
            if (m_coroutine.done()) [[unlikely]]
                return noitem;

            return item(unstable_ref(m_coroutine.promise().value()));
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
} // namespace iter

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
        static_assert(iterable<typename T::value_type&>);
        T optional_iterable;
        using iter_t = iter::iter_t<decltype(*optional_iterable)>;
        item<iter_t> optional_iter = optional_iterable ? MAKE_ITEM(to_iter(*optional_iterable)) : noitem;
    };
    template<class T>
    requires iter<typename T::value_type>
    struct optional_iter_wrapper<T> {
        using iter_t = typename T::value_type;
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
                if (self.current.optional_iter) [[likely]]
                    if (emplace_next(val, *self.current.optional_iter)) [[likely]]
                        return val;
            } while (EMPLACE_NEW(self.current, this_t::get_current(self.i)).optional_iter);
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

// Utilities to cycle iter::generator coroutine
template<class... Ts, std::invocable<Ts&...> F>
requires iter::concepts::generator<std::invoke_result_t<F, Ts&...>>
constexpr auto ITER_IMPL(cycle) (F&& make_iter, Ts&&... args) {
    return iter::detail::flatten_iter {
        iter::generate {
            [make_iter = FWD(make_iter), ...args = FWD(args)]() mutable {
                return MAKE_ITEM(make_iter(static_cast<Ts&>(args)...));
            }
        }
    };
}

template<std::invocable F>
requires iter::concepts::generator<std::invoke_result_t<F>>
constexpr auto ITER_IMPL(cycle) (F&& make_iter) {
    return iter::detail::flatten_iter {
        iter::generate {
            [make_iter = FWD(make_iter)]() mutable {
                return iter::item{make_iter};
            }
        }
    };
}

#endif /* ITER_COROUTINE */

#endif /* ITER_ITERS_GENERATOR_HPP */

#ifdef ITER_COROUTINE
namespace iter::iters { using iter::generator; }
#endif
#ifndef INCLUDE_ITER_COMPOUND_HPP
#define INCLUDE_ITER_COMPOUND_HPP

namespace iter {
    template<class A, class F>
    requires std::constructible_from<item<A>, std::invoke_result_t<F, A const&>>
    struct compound {
        item<A> value;
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

namespace iter::iters { using iter::compound; }

namespace iter::iters { using iter::repeat; }
#ifndef INCLUDE_ITER_EMPTY_HPP
#define INCLUDE_ITER_EMPTY_HPP

namespace iter {
    namespace detail {
        template<class T>
        struct empty_iter {
            using this_t = empty_iter;
            constexpr item<T&> ITER_IMPL_NEXT (this_t&) {
                return noitem;
            }
            constexpr std::size_t ITER_IMPL_SIZE (this_t const&) {
                return 0;
            }
            constexpr auto ITER_IMPL_GET (this_t const&, std::size_t) {
                ITER_UNREACHABLE();
                return stable_ref(reinterpret_cast<T&>(*((T*)0)));
            }
        };
    }

    template<class T>
    detail::empty_iter<T> empty = {};
}

#endif /* INCLUDE_ITER_EMPTY_HPP */

namespace iter::iters { using iter::empty; }

#endif /* ITER_ITERS_ITERS_HPP */

#ifndef ITER_ADAPTERS_ADAPTERS_HPP
#define ITER_ADAPTERS_ADAPTERS_HPP

#ifndef ITER_ADAPTERS_BATCHING_HPP
#define ITER_ADAPTERS_BATCHING_HPP

#ifndef ITER_ITERS_ITER_REF_HPP
#define ITER_ITERS_ITER_REF_HPP

namespace iter::detail {

// Use this to avoid copying when an iter returns an item referring to its own instance
template<iter I>
struct iter_ref {
    using this_t = iter_ref;
    constexpr explicit iter_ref(I& i) : i{std::addressof(i)} {}

    constexpr auto ITER_IMPL_NEXT(this_t& self) {
        return impl::next(*self.i);
    }

private:
    I* i;
};

template<class I>
iter_ref(I&) -> iter_ref<I>;

}

#endif /* ITER_ITERS_ITER_REF_HPP */

ITER_DECLARE(batching)

namespace iter::detail {
    template<iter I, std::invocable<I&> F>
    struct [[nodiscard]] batching_iter {
        using this_t = batching_iter;

        static_assert(concepts::item<std::invoke_result_t<F, iter_ref<I>>>);

        [[no_unique_address]] I i;
        [[no_unique_address]] F func;

        constexpr auto ITER_IMPL_NEXT(this_t& self) {
            return std::invoke(self.func, iter_ref{self.i});
        }
    };

    template<class I, class F>
    batching_iter(I, F) -> batching_iter<I, F>;
}

template<iter::assert_iterable I, std::invocable<iter::iter_t<I>&> F>
constexpr auto ITER_IMPL(batching) (I&& iterable, F&& func) {
    return iter::detail::batching_iter{iter::to_iter(FWD(iterable)), FWD(func)};
}

#endif /* ITER_ADAPTERS_BATCHING_HPP */

namespace iter::adapters { using iter::batching; }
#ifndef ITER_ADAPTERS_BOX_HPP
#define ITER_ADAPTERS_BOX_HPP

ITER_DECLARE(box)

namespace iter {
    template<class ItemType, class GetType = void>
    struct virtual_iter : virtual_iter<ItemType, void> {
        virtual std::size_t size() const = 0;
        virtual GetType get(std::size_t index) = 0;
    private:
        using this_t = virtual_iter;
        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index) { return self.get(index); }
        constexpr auto ITER_IMPL_SIZE (this_t const& self) { return self.size(); }
    };
    template<class ItemType>
    struct virtual_iter<ItemType, void> {
        virtual item<ItemType> next() = 0;
        virtual ~virtual_iter() = default;
    private:
        using this_t = virtual_iter;
        constexpr auto ITER_IMPL_NEXT (this_t& self) { return self.next(); }
    };

    namespace detail {
        template<iter I>
        struct virtual_iter_impl final : I, virtual_iter<item_t<I>> {
            template<class... Ts>
            constexpr virtual_iter_impl(Ts&&... in) : I{FWD(in)...} {}
            item<item_t<I>> next() final { return impl::next(static_cast<I&>(*this)); }
        };
        template<concepts::random_access_iter I>
        struct virtual_iter_impl<I> final : I, virtual_iter<item_t<I>, get_t<I>> {
            template<class... Ts>
            constexpr virtual_iter_impl(Ts&&... in) : I{FWD(in)...} {}
            item<item_t<I>> next() final { return impl::next(static_cast<I&>(*this)); }
            std::size_t size() const final {
                return impl::size(static_cast<I const&>(*this));
            }
            get_t<I> get(std::size_t index) final {
                return impl::get(static_cast<I&>(*this), index);
            }
        };

        struct deleter {
            bool heap = true;
            template<class T>
            void operator()(T* ptr) const {
                if (heap) delete ptr;
                else ptr->~T();
            }
        };
    }

    template<std::size_t Size, std::size_t Align = 8>
    struct scratch {
        template<class T, class... Ts>
        requires (sizeof(T) <= Size) && (alignof(T) <= Align) && (Align % alignof(T) == 0)
        T* make(Ts&&... ins) { return std::launder(new (std::addressof(storage)) T(FWD(ins)...)); }
    private:
        [[no_unique_address]] std::aligned_storage_t<Size, Align> storage;
    };

    template<class ItemType, class Get = void>
    struct [[nodiscard]] boxed {
        using this_t = boxed;
        using Next = item<ItemType>;
        static constexpr bool random_access = !std::same_as<Get, void>;

        template<iter I>
        requires std::same_as<ItemType, item_t<I>>
             && (!random_access || std::same_as<Get, detail::get_t<I>>)
        constexpr boxed(I&& to_box)
            : it{new detail::virtual_iter_impl<std::remove_cvref_t<I>>(FWD(to_box))}
        {}

        template<iter I, std::size_t Size, std::size_t Align>
        requires std::same_as<ItemType, item_t<I>>
             && (!random_access || std::same_as<Get, detail::get_t<I>>)
        constexpr boxed(I&& to_box, scratch<Size, Align>& scratch)
            : it{scratch.template make<detail::virtual_iter_impl<std::remove_cvref_t<I>>>(FWD(to_box)), {0}}
        {}

        template<class OU> requires (!random_access)
        constexpr boxed(boxed<ItemType, OU>&& other) : it{std::move(other.it)} {}

    private:
        template<class, class> friend struct boxed;
        constexpr Next ITER_IMPL_NEXT (this_t& self) { return self.it->next(); }
        constexpr std::size_t ITER_IMPL_SIZE (this_t const& self) requires random_access {
            return self.it->size();
        }
        constexpr Get ITER_IMPL_GET (this_t& self, std::size_t index) requires random_access {
            return self.it->get(index);
        }

        std::unique_ptr<virtual_iter<ItemType, Get>, detail::deleter> it;
    };

    template<iter I>
    boxed(I) -> boxed<item_t<I>, detail::get_t<I>>;
    template<iter I, std::size_t Size, std::size_t Align>
    boxed(I, scratch<Size, Align>&) -> boxed<item_t<I>, detail::get_t<I>>;

    template<iter I>
    using virtual_t = virtual_iter<item_t<I>, detail::get_t<I>>;
    template<iter I>
    using boxed_t = boxed<item_t<I>, detail::get_t<I>>;
}

template<iter::assert_iter I>
constexpr auto ITER_IMPL(box) (I&& iter) {
    return iter::boxed(FWD(iter));
}

template<iter::assert_iter I, std::size_t Size, std::size_t Align>
constexpr auto ITER_IMPL(box) (I&& iter, iter::scratch<Size, Align>& scratch) {
    return iter::boxed(FWD(iter), scratch);
}

#endif /* ITER_ADAPTERS_BOX_HPP */

namespace iter::adapters { using iter::box; }
#ifndef ITER_ADAPTERS_CHAIN_HPP
#define ITER_ADAPTERS_CHAIN_HPP

ITER_DECLARE(chain)

namespace iter::detail {
    template<assert_iter I1, assert_iter I2>
    struct [[nodiscard]] chain_iter : enable_random_access<chain_iter<I1, I2>, I1, I2> {
        static_assert(std::same_as<value_t<I1>, value_t<I2>>);

        item<I1> i1;
        [[no_unique_address]] I2 i2;

    private:
        using this_t = chain_iter;

        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            using stability = common_stability<detail::get_t<I1>, detail::get_t<I2>>;
            std::size_t i1s = impl::size(*self.i1);
            return index < i1s
                ? stability{impl::get(*self.i1, index)}
                : stability{impl::get(self.i2, index - i1s)};
        }

        static constexpr bool owned_next = concepts::owned_item<next_t<I1>> || concepts::owned_item<next_t<I2>>;
        static constexpr bool stable = concepts::stable_iter<I1> && concepts::stable_iter<I2>;
        using item_t = std::conditional_t<owned_next, item<value_t<I1>, stable>, item<value_t<I1>&, stable>>;

        constexpr item_t ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            item_t item;
            if (self.i1) {
                if constexpr (owned_next && !concepts::owned_item<next_t<I1>>) {
                    if (auto val = impl::next(self.i1.value())) {
                        item.emplace(val.value());
                        return item;
                    }
                } else {
                    if (emplace_next(item, self.i1.value()))
                        return item;
                }
                // If we haven't returned by this point, we reached the end of I1
                self.i1.reset();
            }
            if constexpr (owned_next && !concepts::owned_item<next_t<I2>>) {
                if (auto val = impl::next(self.i2))
                    item.emplace(val.value());
            } else {
                emplace_next(item, self.i2);
            }
            return item;
        }
    };

    template<class I1, class I2>
    chain_iter(item<I1>, I2) -> chain_iter<I1, I2>;
}

template<iter::assert_iterable I1, iter::assert_iterable I2>
constexpr auto ITER_IMPL(chain) (I1&& iterable1, I2&& iterable2) {
    using chain_t = iter::detail::chain_iter<iter::iter_t<I1>, iter::iter_t<I2>>;
    if constexpr (chain_t::random_access) {
        auto chain = chain_t{.i1 = MAKE_ITEM(iter::to_iter(FWD(iterable1))), .i2 = iter::to_iter(FWD(iterable2))};
        chain.size = iter::traits::random_access::size(*chain.i1) + iter::traits::random_access::size(chain.i2);
        return chain;
    } else {
        return chain_t{.i1 = MAKE_ITEM(iter::to_iter(FWD(iterable1))), .i2 = iter::to_iter(FWD(iterable2))};
    }
}

#endif /* ITER_ADAPTERS_CHAIN_HPP */

namespace iter::adapters { using iter::chain; }
#ifndef ITER_ADAPTERS_CHUNKS_HPP
#define ITER_ADAPTERS_CHUNKS_HPP

#ifndef ITER_ADAPTERS_TAKE_HPP
#define ITER_ADAPTERS_TAKE_HPP

ITER_DECLARE(take)

namespace iter::detail {
    template<assert_iter I>
    struct [[nodiscard]] take_iter : enable_random_access<take_iter<I>, I> {
        [[no_unique_address]] I i;
        std::size_t n;

        using this_t = take_iter;

        constexpr auto ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            return self.n > 0 ? (--self.n, impl::next(self.i)) : noitem;
        }

        constexpr auto ITER_IMPL_SIZE (this_t const& self)
            requires this_t::random_access
        {
            return std::min(self.n, impl::size(self.i));
        }

        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index)
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

#endif /* ITER_ADAPTERS_TAKE_HPP */

XTD_INVOKER(iter_chunks)

namespace iter {
    namespace detail::tag {
        template<std::size_t N>
        struct chunks_ : xtd::tagged_bindable<chunks_<N>, xtd::invokers::iter_chunks> {};
    }
    template<std::size_t N = 0>
    inline constexpr detail::tag::chunks_<N> chunks_;
}

ITER_ALIAS(chunks, chunks_<>)

namespace iter::detail {
    template<class T, std::size_t N>
    struct chunks_iter_storage;

    template<class T, std::size_t N>
    requires std::is_trivially_default_constructible_v<T>
    struct chunks_iter_storage<T, N>
    {
        T buffer[N]{};
    protected:
        template<class V>
        constexpr void assign(std::size_t n, V&& value) {
            EMPLACE_NEW(buffer[n], FWD(value));
        }
        constexpr auto to_iter(std::size_t n) { return span{buffer, n}; }
    };

    template<class T, std::size_t N>
    requires (!std::is_trivially_default_constructible_v<T>)
    struct chunks_iter_storage<T, N>
    {
        chunks_iter_storage() = default;
        constexpr chunks_iter_storage(chunks_iter_storage const& other) : buffer{}, size{other.size} {
            std::copy_n(other.array(), size, array());
        }
        constexpr ~chunks_iter_storage() {
            auto ptr = array();
            while (size) std::destroy_at(ptr + (--size));
        }
    protected:
        template<class V>
        constexpr void assign(std::size_t n, V&& value) {
            T* ptr = array() + n;
            if (n == size) [[unlikely]] {
                size++;
                std::construct_at(ptr, FWD(value));
            } else {
                *ptr = FWD(value);
            }
        }
        constexpr auto to_iter(std::size_t n) {
            return span{array(), n};
        }
    private:
        std::aligned_union_t<0, T> buffer[N]{};
        std::size_t size = 0;

        using array_t = T[N];
        constexpr array_t& array() {
            return *std::launder(reinterpret_cast<array_t*>(std::addressof(buffer)));
        }
        constexpr array_t const& array() const {
            return *std::launder(reinterpret_cast<array_t const*>(std::addressof(buffer)));
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
            return n > 0 ? MAKE_ITEM(self.to_iter(n)) : noitem;
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
            if (self.remaining) [[likely]] {
                --self.remaining;
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
        constexpr unstable_item<iter_ref<lazy_chunk_iter<I>>> ITER_IMPL_NEXT (this_t& self) {
            if (self.size) [[likely]] {
                if (self.remaining) [[unlikely]] {
                    // Deal with the case where inner iter is not fully iterated
                    while (impl::next(static_cast<lazy_chunk_iter<I>&>(self)));
                    if (self.size == 0)
                        return noitem;
                }
                self.remaining = self.size;
                return iter_ref<lazy_chunk_iter<I>>(self);
            }
            return noitem;
        }
    };
}

template<iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_chunks, iter::detail::tag::chunks_<0>) (I&& iterable, std::uint32_t size) {
    return iter::detail::chunks_iter<std::remove_reference_t<I>, 0>{
        {.size = size, .remaining = 0, .i = FWD(iterable)}};
}

template<std::size_t N, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_chunks, iter::detail::tag::chunks_<N>) (I&& iterable) {
    return iter::detail::chunks_iter<std::remove_reference_t<I>, N>{{}, FWD(iterable)};
}

#endif /* ITER_ADAPTERS_CHUNKS_HPP */

namespace iter::adapters { using iter::chunks; using iter::chunks_; }
#ifndef ITER_ADAPTERS_CHUNK_BY_HPP
#define ITER_ADAPTERS_CHUNK_BY_HPP

ITER_DECLARE(chunk_by)

namespace iter::concepts {
    template<class F, class I>
    concept chunk_by_invocable_proj = requires (F func, cref_t<I> ref) {
        func(ref);
    };
    template<class F, class I>
    concept chunk_by_invocable_adj = requires (F func, cref_t<I> ref) {
        { func(ref, ref) } -> std::same_as<bool>;
    };

    template<class F, class I>
    concept chunk_by_invocable = chunk_by_invocable_proj<F, I> || chunk_by_invocable_adj<F, I>;
}

namespace iter::detail {
    template<concepts::stable_iter I, concepts::chunk_by_invocable<I> F>
    struct chunk_by_iter_base { // chunk_by_invocable_adj
        using this_t = chunk_by_iter_base;

        [[no_unique_address]] I i;
        [[no_unique_address]] F func;
        next_t<I> items[2]{};
        // 0, 1 : in middle of chunk, index to emplace into
        // 2 : end of chunk (index 0)
        // 3 : end of chunk (index 1)
        // 4 : no items left
        // 5 : iteration not started
        std::uint8_t index = 5;

        constexpr item<ref_t<I>, false> ITER_IMPL_NEXT(this_t& self) {
            if (self.index > 1) { // start of next chunk
                self.index -= 2;
                // return item which failed chunk predicate
                return unstable_ref(*self.items[self.index ^ 1]);
            }
            if (auto& current = emplace_next(self.items[self.index], self.i)) {
                self.index ^= 1;
                if (auto& prev = self.items[self.index]) {
                    if (!self.func(as_const(*prev), as_const(*current))) {
                        self.index += 2; // predicate failed, make sure to return current value next time
                        return noitem;
                    }
                }
                return unstable_ref(*current);
            }
            self.index = 4;
            return noitem;
        }
    };

    template<concepts::stable_iter I, concepts::chunk_by_invocable_proj<I> F>
    struct chunk_by_iter_base<I, F> {
        using this_t = chunk_by_iter_base;
        using projection_t = make_item_t<std::invoke_result_t<F, cref_t<I>>>;
        static_assert(concepts::stable_item<projection_t>);
        
        [[no_unique_address]] I i;
        [[no_unique_address]] F func;
        next_t<I> last;
        projection_t projection;
        bool end = false;

        constexpr item<ref_t<I>, false> ITER_IMPL_NEXT(this_t& self) {
            if (self.end && self.last) [[unlikely]] {
                self.end = false;
                return unstable_ref(*self.last);
            }

            if (emplace_next(self.last, self.i)) {
                if (self.projection) [[likely]] {
                    auto projected = MAKE_ITEM_AUTO(self.func(as_const(*self.last)));
                    if (*projected != *self.projection) {
                        self.projection = std::move(projected);
                        self.end = true;
                        return noitem;
                    }
                } else {
                    EMPLACE_NEW(self.projection, MAKE_ITEM_AUTO(self.func(as_const(*self.last))));
                }
                return unstable_ref(*self.last);
            } else {
                self.end = true;
            }
            return noitem;
        }
    };
    template<iter I, class F>
    struct [[nodiscard]] chunk_by_iter : chunk_by_iter_base<I, F> {
        using this_t = chunk_by_iter;
        using base_t = chunk_by_iter_base<I, F>;

        constexpr unstable_item<iter_ref<base_t>> ITER_IMPL_NEXT(this_t& self)
            requires concepts::chunk_by_invocable_proj<F, I> 
        {
            auto& base = static_cast<base_t&>(self);
            if (base.end == base.last.has_value())
                return iter_ref(base);
            else if (base.last) [[unlikely]] {
                while(auto n = impl::next(base));
                if (base.last.has_value())
                    return iter_ref(base);
            }
            return noitem;
        }

        constexpr unstable_item<iter_ref<base_t>> ITER_IMPL_NEXT(this_t& self)
            requires concepts::chunk_by_invocable_adj<F, I>
        {
            auto& base = static_cast<base_t&>(self);
            if (base.index == 5) [[unlikely]] { // kick start iteration
                base.index = 0;
            } else if (base.index == 4) [[unlikely]] { // no more items
                return noitem;
            } else if (base.index < 2) [[unlikely]] { // unfinished chunk
                while(auto n = impl::next(base));
                if (base.index == 4)
                    return noitem;
            }
            return iter_ref(base);
        }
    };
}

template<iter::assert_iterable I, iter::concepts::chunk_by_invocable<I> F>
constexpr auto ITER_IMPL(chunk_by)(I&& iterable, F&& func) {
    return iter::detail::chunk_by_iter{iter::to_iter(FWD(iterable)), FWD(func)};
}

#endif /* ITER_ADAPTERS_CHUNK_BY_HPP */

namespace iter::adapters { using iter::chunk_by; }
#ifndef ITER_ADAPTERS_CONSTEVAL_ONLY_HPP
#define ITER_ADAPTERS_CONSTEVAL_ONLY_HPP

ITER_DECLARE(consteval_only)

namespace iter::detail {
    template<iter I>
    struct [[nodiscard]] consteval_only_iter {
        using this_t = consteval_only_iter;
        [[no_unique_address]] I i;

        constexpr decltype(auto) ITER_IMPL_NEXT(this_t& self) {
            assert_consteval<this_t, struct next>();
            return impl::next(self.i);
        }

        constexpr decltype(auto) ITER_IMPL_NEXT_BACK(this_t& self)
            requires concepts::double_ended_iter<I>
        {
            assert_consteval<this_t, struct next_back>();
            return impl::next_back(self.i);
        }

        constexpr auto ITER_IMPL_GET(this_t& self, std::size_t index)
            requires concepts::random_access_iter<I>
        {
            assert_consteval<this_t, struct get>();
            return impl::get(self.i, index);
        }

        constexpr std::size_t ITER_IMPL_SIZE(this_t const& self)
            requires concepts::random_access_iter<I>
        {
            assert_consteval<this_t, struct size>();
            return impl::size(self.i);
        }
    };

    template<class I>
    consteval_only_iter(I) -> consteval_only_iter<I>;
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(consteval_only) (I&& iterable) {
    return iter::detail::consteval_only_iter{iter::to_iter(FWD(iterable))};
}

#endif /* ITER_ADAPTERS_CONSTEVAL_ONLY_HPP */

namespace iter::adapters { using iter::consteval_only; }
#ifndef ITER_ADAPTERS_CYCLE_HPP
#define ITER_ADAPTERS_CYCLE_HPP

namespace iter::detail {
    template<assert_iter I>
    struct [[nodiscard]] cycle_iter : enable_random_access<cycle_iter<I>, I> {
        using this_t = cycle_iter;
        [[no_unique_address]] I i;
        [[no_unique_address]] std::conditional_t<this_t::random_access, void_t, I> i_orig = i;

    private:
        constexpr std::size_t ITER_IMPL_SIZE (this_t const& self)
            requires this_t::random_access
        {
            return std::numeric_limits<std::size_t>::max();
        }

        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index)
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

#endif /* ITER_ADAPTERS_CYCLE_HPP */

namespace iter::adapters { using iter::cycle; }
#ifndef ITER_ADAPTERS_ELEMENTS_HPP
#define ITER_ADAPTERS_ELEMENTS_HPP

#ifndef ITER_ADAPTERS_MAP_HPP
#define ITER_ADAPTERS_MAP_HPP

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
            "Do not return iter::optional in iter::flatmap, instead return iter::item (preferrably in iter::filter_map).");
        using wrapped_inner_iter_t = iter_wrapper<invoke_result>;
        using inner_iter_t = typename wrapped_inner_iter_t::iter_t;

        item<wrapped_inner_iter_t> current{};
        [[no_unique_address]] I i;
        [[no_unique_address]] F func;

        constexpr auto get_current() {
            auto next = impl::next(i);
            return next
                ? MAKE_ITEM(iter_wrapper{func(consume(next))})
                : noitem;
        }

        constexpr next_t<inner_iter_t> ITER_IMPL_NEXT (this_t& self) {
            auto val = no_next<inner_iter_t>();
            do {
                if (self.current) [[likely]]
                    if (emplace_next(val, self.current->iter)) [[likely]]
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

#ifndef ITER_ADAPTERS_FILTER_MAP_HPP
#define ITER_ADAPTERS_FILTER_MAP_HPP

ITER_DECLARE(filter_map)

namespace iter::detail {
    template<assert_iter I, std::invocable<consume_t<I>> F>
    struct [[nodiscard]] filter_map_iter {
        using this_t = filter_map_iter;
        using mapped_t = std::invoke_result_t<F, consume_t<I>>;
        static_assert(concepts::item<mapped_t>);

        [[no_unique_address]] I i;
        [[no_unique_address]] F func;

        constexpr mapped_t ITER_IMPL_NEXT (this_t& self) {
            mapped_t mapped;
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

#endif /* ITER_ADAPTERS_FILTER_MAP_HPP */

// flatmap on iter::item is equivalent to the specially optimised filter_map
template<iter::assert_iterable I, std::invocable<iter::consume_t<I>> F>
requires iter::concepts::item<std::invoke_result_t<F, iter::consume_t<I>>>
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

        constexpr auto ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            auto val = impl::next(self.i);
            return val ? MAKE_ITEM_AUTO(self.func(consume(val))) : noitem;
        }

        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return self.func(get(impl::get(self.i, index)));
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

#endif /* ITER_ADAPTERS_MAP_HPP */

XTD_INVOKER(iter_elements)
ITER_DECLARE(keys)
ITER_DECLARE(values)

namespace iter {
    namespace detail::tag {
        template<std::size_t N>
        struct elements : xtd::tagged_bindable<elements<N>, xtd::invokers::iter_elements> {};
    }

    template<std::size_t N>
    inline constexpr detail::tag::elements<N> elements;
}

template<std::size_t N, iter::assert_iterable I>
constexpr auto XTD_IMPL_TAG_(iter_elements, iter::detail::tag::elements<N>)(I&& iterable) {
    using std::get;
    return iter::map(FWD(iterable), [](auto&& value) {
        constexpr bool stable = iter::concepts::stable_iter<iter::iter_t<I>>;
        using element_t = std::tuple_element_t<N, iter::value_t<I>>;
        if constexpr (!std::is_reference_v<element_t> && !std::is_reference_v<iter::item_t<I>>)
            return MAKE_STABILITY(stable, get<N>(FWD(value)));
        else
            return MAKE_STABILITY_AUTO(stable, get<N>(FWD(value)));
    });
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(keys)(I&& iterable) {
    return iter::elements<0>(FWD(iterable));
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(values)(I&& iterable) {
    return iter::elements<1>(FWD(iterable));
}

#endif /* ITER_ADAPTERS_ELEMENTS_HPP */

namespace iter::adapters { using iter::elements; using iter::keys; using iter::values; }
#ifndef INCLUDE_ITER_ENUMERATE_MAP_HPP
#define INCLUDE_ITER_ENUMERATE_MAP_HPP

#ifndef INCLUDE_ITER_ZIP_MAP_HPP
#define INCLUDE_ITER_ZIP_MAP_HPP

ITER_DECLARE(zip_map)

namespace iter::detail {
    template<class F, assert_iter... I>
    requires (sizeof...(I) > 1) && std::invocable<F, consume_t<I>...>
    struct [[nodiscard]] zip_map_iter : enable_random_access<zip_map_iter<F, I...>, I...> {
        using this_t = zip_map_iter;

        [[no_unique_address]] F func;
        [[no_unique_address]] tuple<I...> i;

        constexpr auto ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            return apply([&](auto&... iters) {
                return [&](auto... items) {
                    return (... & items.has_value())
                        ? MAKE_ITEM_AUTO(self.func(consume(items)...))
                        : noitem;
                }(impl::next(iters)...);
            }, self.i);
        }

        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return apply([=](auto&... iters) -> decltype(auto) {
                return self.func(get(impl::get(iters, index))...);
            }, self.i);
        }
    };

    template<assert_iterable... Is, class F>
    constexpr decltype(auto) make_zip_map_iter(Is&&... iterables, F&& func)
    {
        return zip_map_iter<std::remove_cvref_t<F>, iter_t<Is>...>{
            .func{FWD(func)}, .i{to_iter(FWD(iterables))...}};
    }
}

// First args are iterables, last arg is function
template<class... Ts>
constexpr auto ITER_IMPL(zip_map) (Ts&&... args) {
    auto zip = [&]<std::size_t... I>(std::index_sequence<I...>) {
        // TODO: Avoid std::tuple_element_t
        return iter::detail::make_zip_map_iter<std::tuple_element_t<I, iter::tuple<Ts...>>...>(FWD(args)...);
    }(std::make_index_sequence<sizeof...(Ts) - 1>{});
    if constexpr(decltype(zip)::random_access) {
        zip.size = apply([](auto&... iters) {
            return std::min({iter::traits::random_access::size(iters)...});
        }, zip.i);
    }
    return zip;
}

#endif /* INCLUDE_ITER_ZIP_MAP_HPP */

XTD_INVOKER(iter_enumerate_map)

namespace iter {
    namespace detail::tag {
        template<class T = std::size_t>
        struct enumerate_map_ : xtd::tagged_bindable<enumerate_map_<T>, xtd::invokers::iter_enumerate_map> {};
    }

    template<class T = std::size_t>
    inline constexpr detail::tag::enumerate_map_<T> enumerate_map_;
}

ITER_ALIAS(enumerate_map, enumerate_map_<>)

template<class T, iter::assert_iterable I, class F>
constexpr decltype(auto) XTD_IMPL_TAG_(iter_enumerate_map, iter::detail::tag::enumerate_map_<T>) (I&& iterable, F&& func) {
    return iter::zip_map(FWD(iterable), iter::indices_<T>, FWD(func));
}

#endif /* INCLUDE_ITER_ENUMERATE_MAP_HPP */

namespace iter::adapters { using iter::enumerate_map; using iter::enumerate_map_; }
#ifndef ITER_ADAPTERS_ENUMERATE_HPP
#define ITER_ADAPTERS_ENUMERATE_HPP

#ifndef ITER_ADAPTERS_ZIP_HPP
#define ITER_ADAPTERS_ZIP_HPP

ITER_DECLARE(zip)

namespace iter::detail {
    template<class T>
    static constexpr auto lazy_unwrap_item(T&& in) {
        if constexpr (concepts::owned_item<T>)
            return [&] { return FWD(in).consume(); };
        else
            return [&]() -> auto&& { return FWD(in).consume(); };
    }
    template<iter I>
    static constexpr auto lazy_get(I& iter, std::size_t index) {
        if constexpr (std::is_reference_v<stability_unwrap<get_t<I>>>)
            return [&iter, index]() -> auto&& { return get(impl::get(iter, index)); };
        else
            return [&iter, index] { return get(impl::get(iter, index)); };
    }

    template<assert_iter... I>
    requires (sizeof...(I) > 1)
    struct [[nodiscard]] zip_iter : enable_random_access<zip_iter<I...>, I...> {
        using this_t = zip_iter;

        [[no_unique_address]] tuple<I...> i;

        constexpr auto ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            constexpr bool stable = (concepts::stable_iter<I> && ...);
            return apply([](auto&... iters) {
                return [](auto... items) {
                    return (... & items.has_value())
                        ? MAKE_ITEM(MAKE_STABILITY(stable, make_tuple_lazy(lazy_unwrap_item(std::move(items))...)))
                        : noitem;
                }(impl::next(iters)...);
            }, self.i);
        }

        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            constexpr bool stable = (concepts::stable<get_t<I>> && ...);
            return apply([=](auto&... iters) {
                return MAKE_STABILITY(stable, make_tuple_lazy(lazy_get(iters, index)...));
            }, self.i);
        }
    };

    template<class T> inline constexpr bool is_zip = false;
    template<class... Ts> inline constexpr bool is_zip<zip_iter<Ts...>> = true;
    template<class T>
    concept decays_to_zip = is_zip<std::remove_cvref_t<T>>;
}

template<iter::assert_iterable... I>
constexpr auto ITER_IMPL(zip) (I&&... iterables) {
    auto zip = iter::detail::zip_iter<iter::iter_t<I>...>{.i = {iter::to_iter(FWD(iterables))...}};
    if constexpr(decltype(zip)::random_access) {
        zip.size = apply([](auto&... iters) {
            return std::min({iter::traits::random_access::size(iters)...});
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

#endif /* ITER_ADAPTERS_ZIP_HPP */

XTD_INVOKER(iter_enumerate)

namespace iter {
    namespace detail::tag {
        template<class T = std::size_t>
        struct enumerate_ : xtd::tagged_bindable<enumerate_<T>, xtd::invokers::iter_enumerate> {};
    }

    template<class T = std::size_t>
    inline constexpr detail::tag::enumerate_<T> enumerate_;
}

ITER_ALIAS(enumerate, enumerate_<>)

template<class T, iter::assert_iterable I>
constexpr decltype(auto) XTD_IMPL_TAG_(iter_enumerate, iter::detail::tag::enumerate_<T>) (I&& iterable) {
    return iter::zip(FWD(iterable), iter::indices_<T>);
}

#endif /* ITER_ADAPTERS_ENUMERATE_HPP */

namespace iter::adapters { using iter::enumerate; using iter::enumerate_; }

namespace iter::adapters { using iter::filter_map; }
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

namespace iter::adapters { using iter::filter; }

namespace iter::adapters { using iter::flatmap; }

namespace iter::adapters { using iter::flatten; }
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

        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            auto val = impl::get(self.i, index);
            self.func(get(val));
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

namespace iter::adapters { using iter::inspect; }
#ifndef INCLUDE_ITER_MAP_WHILE_HPP
#define INCLUDE_ITER_MAP_WHILE_HPP

ITER_DECLARE(map_while)

namespace iter::detail {
    template<assert_iter I, std::invocable<consume_t<I>> F>
    struct [[nodiscard]] map_while_iter {
        using this_t = map_while_iter;
        using mapped_t = std::invoke_result_t<F, consume_t<I>>;
        static_assert(concepts::item<mapped_t>);

        [[no_unique_address]] I i;
        [[no_unique_address]] F func;

        constexpr mapped_t ITER_IMPL_NEXT (this_t& self) {
            auto val = impl::next(self.i);
            return val ? self.func(consume(val)) : noitem;
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

namespace iter::adapters { using iter::map_while; }

namespace iter::adapters { using iter::map; }
#ifndef ITER_ADAPTERS_MOVE_HPP
#define ITER_ADAPTERS_MOVE_HPP

ITER_DECLARE(move)

namespace iter::detail {
    template<iter::assert_iter I>
    struct [[nodiscard]] move_iter : enable_random_access<move_iter<I>, I> {
        [[no_unique_address]] I i;

        using this_t = move_iter;

        constexpr auto ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            return move_item{impl::next(self.i)};
        }

        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            auto value = impl::get(self.i, index);
            if constexpr (std::is_reference_v<detail::stability_unwrap<decltype(value)>>) {
                if constexpr (concepts::stable<decltype(value)>)
                    return stable_ref(std::move(get(value)));
                else
                    return unstable_ref(std::move(get(value)));
            }
            else
                return value;
        }
    };

    template<class T>
    move_iter(T) -> move_iter<T>;
}

template<iter::assert_iterable I>
constexpr decltype(auto) ITER_IMPL(move) (I&& iterable) {
    if constexpr (iter::concepts::move_item<iter::next_t<I>>)
        return FWD(iterable);
    else
        return iter::detail::move_iter<iter::iter_t<I>>{.i = iter::to_iter(FWD(iterable))};
}

#endif /* ITER_ADAPTERS_MOVE_HPP */

namespace iter::adapters { using iter::move; }
#ifndef INCLUDE_ITER_REVERSE_HPP
#define INCLUDE_ITER_REVERSE_HPP

ITER_DECLARE(reverse)

namespace iter::detail {
    template<assert_iter I>
    struct [[nodiscard]] reverse_iter : enable_random_access<reverse_iter<I>, I> {
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

        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index)
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

namespace iter::adapters { using iter::reverse; }
#ifndef ITER_ADAPTERS_SKIP_EAGER_HPP
#define ITER_ADAPTERS_SKIP_EAGER_HPP

ITER_DECLARE(skip_eager)

template<iter::assert_iterable I>
constexpr iter::iter_t<I> ITER_IMPL(skip_eager) (I&& iterable, std::size_t n) {
    static_assert(!iter::concepts::random_access_iterable<I>,
        "iter::iter_eager is only for non-random-access iterables, use iter::skip.");
    auto iter = iter::to_iter(FWD(iterable));
    for(; n > 0 && iter::traits::next(iter).has_value(); --n);
    return iter;
}

#endif /* ITER_ADAPTERS_SKIP_EAGER_HPP */

namespace iter::adapters { using iter::skip_eager; }
#ifndef INCLUDE_ITER_SKIP_WHILE_HPP
#define INCLUDE_ITER_SKIP_WHILE_HPP

ITER_DECLARE(skip_while)

namespace iter::detail {
    template<assert_iter I, std::predicate<cref_t<I>> P>
    struct [[nodiscard]] skip_while_iter {
        using this_t = skip_while_iter;

        [[no_unique_address]] I i;
        item<P> pred;

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
    return iter::detail::skip_while_iter<iter::iter_t<I>, std::remove_cvref_t<P>>{.i{iter::to_iter(FWD(iterable))}, .pred{FWD(pred)}};
}

#endif /* INCLUDE_ITER_SKIP_WHILE_HPP */

namespace iter::adapters { using iter::skip_while; }
#ifndef INCLUDE_ITER_SKIP_HPP
#define INCLUDE_ITER_SKIP_HPP

ITER_DECLARE(skip)

namespace iter::detail {
    template<assert_iter I>
    struct [[nodiscard]] skip_iter : enable_random_access<skip_iter<I>, I> {
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

        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index)
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

namespace iter::adapters { using iter::skip; }
#ifndef ITER_ADAPTERS_SPLIT_HPP
#define ITER_ADAPTERS_SPLIT_HPP

ITER_DECLARE(split)

namespace iter::detail {
    // Book-keeping to ensure no infinite loop if inner iter is not iterated
    enum class inner_iter_status : std::uint8_t {
        created, inner_unfinished, inner_finished, outer_finished
    };

    template<assert_iter I>
    struct split_iter_inner {
        [[no_unique_address]] I i;
        value_t<I> delimiter;
        inner_iter_status status = inner_iter_status::created;

        using this_t = split_iter_inner;
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            auto next = no_next<I>();
            if (!emplace_next(next, self.i)) [[unlikely]] {
                self.status = inner_iter_status::outer_finished;
            } else if (*next == self.delimiter) [[unlikely]] {
                self.status = inner_iter_status::inner_finished;
                next.reset();
            }
            return next;
        }
    };

    template<assert_iter I>
    struct [[nodiscard]] split_iter : split_iter_inner<I> {
        using this_t = split_iter;

        constexpr unstable_item<iter_ref<split_iter_inner<I>>> ITER_IMPL_NEXT (this_t& self) {
            if (self.status == inner_iter_status::outer_finished)
                return noitem;
            if (self.status == inner_iter_status::inner_unfinished) [[unlikely]] {
                // Inner iter was not fully iterated, so we must complete the inner iter
                while (impl::next(static_cast<split_iter_inner<I>&>(self)));
                if (self.status == inner_iter_status::outer_finished)
                    return noitem;
            }
            self.status = inner_iter_status::inner_unfinished;
            return iter_ref<split_iter_inner<I>>(self);
        }
    };
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(split) (I&& iterable, iter::value_t<I> delimiter) {
    return iter::detail::split_iter<iter::iter_t<I>>{
        {.i = iter::to_iter(FWD(iterable)), .delimiter = delimiter}};
}

#endif /* ITER_ADAPTERS_SPLIT_HPP */

namespace iter::adapters { using iter::split; }
#ifndef INCLUDE_ITER_TAKE_WHILE_HPP
#define INCLUDE_ITER_TAKE_WHILE_HPP

ITER_DECLARE(take_while)

namespace iter::detail {
    template<assert_iter I, std::predicate<ref_t<I>> P>
    struct [[nodiscard]] take_while_iter {
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

namespace iter::adapters { using iter::take_while; }

namespace iter::adapters { using iter::take; }
#ifndef ITER_ADAPTERS_TO_POINTER_ITER_HPP
#define ITER_ADAPTERS_TO_POINTER_ITER_HPP

ITER_DECLARE(to_pointer_iter)

namespace iter::detail {
    template<iter I>
    struct [[nodiscard]] to_pointer_iter {
        using this_t = to_pointer_iter;

        [[no_unique_address]] I i;
        next_t<I> store = noitem;

        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            emplace_next(self.store, self.i);
            return self.store ? item(unstable_ref(*self.store)) : noitem;
        }
    };

    template<iter I>
    requires concepts::random_access_iter<I>
    struct [[nodiscard]] to_pointer_iter<I> : enable_random_access<to_pointer_iter<I>, I> {
        using this_t = to_pointer_iter;

        // If impl::get returns a value, then we need to store it to return a pointer to storage
        static constexpr bool get_val = !std::is_reference_v<get_t<I>>;
        [[no_unique_address]] I i;
        [[no_unique_address]] std::conditional_t<get_val, unstable_item<item_t<I>>, void_t> store;

        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index) {
            if constexpr (get_val) {
                self.store.emplace([&] { return impl::get(self.i, index); });
                return unstable_ref(self.store.value());
            } else
                return impl::get(self.i, index);
        }
    };

    template<class I>
    to_pointer_iter(I) -> to_pointer_iter<I>;
}

template<iter::assert_iter I>
constexpr decltype(auto) ITER_IMPL(to_pointer_iter) (I&& iter) {
    if constexpr (!iter::concepts::owned_item<iter::next_t<I>>) {
        return FWD(iter);
    } else {
        return iter::detail::to_pointer_iter<iter::iter_t<I>>{.i = FWD(iter)};
    }
}

#endif /* ITER_ADAPTERS_TO_POINTER_ITER_HPP */

namespace iter::adapters { using iter::to_pointer_iter; }
#ifndef INCLUDE_ITER_WINDOW_HPP
#define INCLUDE_ITER_WINDOW_HPP

XTD_INVOKER(iter_window)

namespace iter {
    namespace detail::tag {
        template<std::size_t N>
        struct window : xtd::tagged_bindable<window<N>, xtd::invokers::iter_window> {};
    }
    template<std::size_t N = 2>
    inline constexpr detail::tag::window<N> window;
}

namespace iter::detail {
    template<concepts::stable_item T, std::size_t N>
    struct window_iter_storage {
        std::array<T, N> buffer = {};
        std::size_t size = 0;
        std::size_t end = 0;
        constexpr auto to_iter() {
            using namespace xtd::literals;
            return cycle(buffer)
                | map(_, [](auto& item) { return unstable_ref(*item); })
                | skip(_, end)
                | take(_, size--);
        }
    };

    template<assert_iter I, std::size_t N>
    struct [[nodiscard]] window_iter : window_iter_storage<next_t<I>, N> {
        static_assert(N > 1, "Window must be of at least size 2");
        [[no_unique_address]] I i;

        using this_t = window_iter;
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            while (self.size < N) [[likely]] {
                if (emplace_next(self.buffer[self.end], self.i)) [[likely]] {
                    ++self.size;
                    self.end = (self.end + 1) % N;
                } else break;
            }
            return self.size == N ? MAKE_ITEM(unstable{self.to_iter()}) : noitem;
        }
    };
}

template<std::size_t N, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_window, iter::detail::tag::window<N>) (I&& iterable) {
    return iter::detail::window_iter<std::remove_reference_t<I>, N>{{}, {FWD(iterable)}};
}

#endif /* INCLUDE_ITER_WINDOW_HPP */

namespace iter::adapters { using iter::window; }

namespace iter::adapters { using iter::zip_map; }

namespace iter::adapters { using iter::zip; }

#endif /* ITER_ADAPTERS_ADAPTERS_HPP */

#ifndef ITER_CONSUMERS_CONSUMERS_HPP
#define ITER_CONSUMERS_CONSUMERS_HPP

// Reducing
#ifndef INCLUDE_ITER_FOREACH_HPP
#define INCLUDE_ITER_FOREACH_HPP

ITER_DECLARE(foreach)
ITER_ALIAS(for_each, foreach)

template<iter::assert_iterable I, iter::concepts::inspector<iter::consume_t<I>> F>
constexpr void ITER_IMPL(foreach) (I&& iterable, F func) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (auto val = iter::traits::next(iter)) {
        func(iter::detail::consume(val));
    }
}

template<iter::assert_iterable I>
constexpr void ITER_IMPL(foreach) (I&& iterable) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (iter::traits::next(iter));
}

#endif /* INCLUDE_ITER_FOREACH_HPP */

namespace iter::consumers { using iter::foreach; }
#ifndef ITER_COLLECTORS_FOLD_HPP
#define ITER_COLLECTORS_FOLD_HPP

ITER_DECLARE(fold)
ITER_ALIAS(fold_left, fold)

template<iter::assert_iterable I, class T, std::invocable<const T&, iter::consume_t<I>> F>
constexpr auto ITER_IMPL(fold) (I&& iterable, T&& init, F func) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    auto acc = FWD(init);
    while (auto val = iter::traits::next(iter)) {
        acc = func(iter::as_const(acc), iter::detail::consume(val));
    }
    return acc;
}

#endif /* ITER_COLLECTORS_FOLD_HPP */

namespace iter::consumers { using iter::fold; }
#ifndef ITER_CONSUMERS_REDUCE_HPP
#define ITER_CONSUMERS_REDUCE_HPP

ITER_DECLARE(reduce)

template<iter::assert_iterable I, std::invocable<iter::ref_t<I>, iter::consume_t<I>> F>
constexpr iter::item<iter::value_t<I>> ITER_IMPL(reduce) (I&& iterable, F&& func) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    auto acc = iter::traits::next(iter);
    return acc
        ? MAKE_ITEM(iter::fold(iter, iter::detail::consume(acc), FWD(func)))
        : iter::noitem;
}

#endif /* ITER_CONSUMERS_REDUCE_HPP */

namespace iter::consumers { using iter::reduce; }
#ifndef INCLUDE_ITER_SUM_HPP
#define INCLUDE_ITER_SUM_HPP

ITER_DECLARE(sum)

template<iter::assert_iterable I>
requires std::is_arithmetic_v<iter::value_t<I>>
constexpr auto ITER_IMPL(sum) (I&& iterable) {
    std::remove_const_t<iter::value_t<I>> sum = 0;
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (auto val = iter::traits::next(iter)) {
        sum += *val;
    }
    return sum;
}

#endif /* INCLUDE_ITER_SUM_HPP */

namespace iter::consumers { using iter::sum; }
#ifndef ITER_CONSUMERS_PRODUCT_HPP
#define ITER_CONSUMERS_PRODUCT_HPP

ITER_DECLARE(product)

template<iter::assert_iterable I>
requires std::is_arithmetic_v<iter::value_t<I>>
constexpr auto ITER_IMPL(product) (I&& iterable) {
    std::remove_const_t<iter::value_t<I>> product = 1;
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (auto val = iter::traits::next(iter)) {
        product *= *val;
    }
    return product;
}

#endif /* ITER_CONSUMERS_PRODUCT_HPP */

namespace iter::consumers { using iter::product; }

// Finding
#ifndef ITER_CONSUMERS_LAST_HPP
#define ITER_CONSUMERS_LAST_HPP

ITER_DECLARE(last)

template<iter::concepts::random_access_iterable I>
constexpr auto ITER_IMPL(last) (I&& iterable) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    std::size_t size = iter::traits::random_access::size(iter);
    using get_t = iter::detail::stability_unwrap<decltype(iter::traits::random_access::get(iter, size - 1))>;
    if constexpr (std::is_lvalue_reference_v<decltype(iter)> && std::is_reference_v<get_t>)
        return size > 0 ? MAKE_ITEM_AUTO(iter::traits::random_access::get(iter, size - 1)) : iter::noitem;
    else
        return size > 0 ? iter::item([&] {
            if constexpr (std::is_reference_v<get_t>)
                return get(iter::traits::random_access::get(iter, size - 1));
            else
                return iter::traits::random_access::get(iter, size - 1);
        }) : iter::noitem;
}

template<iter::concepts::random_access_iterable I, class T>
constexpr auto ITER_IMPL(last) (I&& iterable, T&& fallback) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    std::size_t size = iter::traits::random_access::size(iter);
    return size > 0 ? get(iter::traits::random_access::get(iter, size - 1)) : FWD(fallback);
}

template<iter::iterable I>
requires (iter::concepts::owned_item<iter::next_t<I>>)
      && (!iter::concepts::random_access_iterable<I>)
constexpr auto ITER_IMPL(last) (I&& iterable) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    iter::next_t<decltype(iter)> results[2] = {iter::noitem, iter::noitem};
    char i = 0;
    while (true) {
        bool empty = !iter::detail::emplace_next(results[i], iter);
        i ^= 1;
        if (empty) [[unlikely]] return std::move(results[i]);
    }
}

template<iter::iterable I>
requires (!iter::concepts::owned_item<iter::next_t<I>>)
      && (!iter::concepts::random_access_iterable<I>)
constexpr auto ITER_IMPL(last) (I&& iterable) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    iter::item<iter::value_t<I>> result;
    while (auto val = iter::traits::next(iter)) {
        result.emplace(val.consume());
    }
    return result;
}

template<iter::iterable I, class T>
requires (iter::concepts::owned_item<iter::next_t<I>>)
      && (!iter::concepts::random_access_iterable<I>)
constexpr auto ITER_IMPL(last) (I&& iterable, T&& fallback) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    iter::next_t<decltype(iter)> results[2] = {iter::noitem, iter::noitem};
    char i = 0;
    while (true) {
        bool empty = !iter::detail::emplace_next(results[i], iter);
        i ^= 1;
        if (empty) [[unlikely]] {
            return results[i] ? std::move(*results[i]) : FWD(fallback);
        }
    }
}

template<iter::iterable I, class T>
requires (!iter::concepts::owned_item<iter::next_t<I>>)
      && (!iter::concepts::random_access_iterable<I>)
constexpr auto ITER_IMPL(last) (I&& iterable, T&& fallback) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    iter::value_t<I> result = FWD(fallback);
    while (auto val = iter::traits::next(iter)) {
        result = iter::detail::consume(val);
    }
    return result;
}

#endif /* ITER_CONSUMERS_LAST_HPP */

namespace iter::consumers { using iter::last; }
#ifndef INCLUDE_ITER_NTH_HPP
#define INCLUDE_ITER_NTH_HPP

ITER_DECLARE(nth)

template<iter::concepts::random_access_iterable I>
constexpr auto ITER_IMPL(nth) (I&& iterable, std::size_t n) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    std::size_t size = iter::traits::random_access::size(iter);
    using get_t = decltype(iter::traits::random_access::get(iter, n));
    if constexpr (std::is_lvalue_reference_v<decltype(iter)> && std::is_reference_v<get_t>)
        return size > n ? MAKE_ITEM_AUTO(iter::traits::random_access::get(iter, n)) : iter::noitem;
    else
        return size > n ? MAKE_ITEM(iter::traits::random_access::get(iter, n)) : iter::noitem;
}

template<iter::concepts::random_access_iterable I, class T>
constexpr auto ITER_IMPL(nth) (I&& iterable, std::size_t n, T&& fallback) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    std::size_t size = iter::traits::random_access::size(iter);
    return size > n ? iter::traits::random_access::get(iter, n) : FWD(fallback);
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(nth) (I&& iterable, std::size_t n) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    auto result = iter::no_next<decltype(iter)>();
    while (iter::detail::emplace_next(result, iter) && n-- > 0);
    if constexpr (iter::concepts::owned_item<decltype(result)> || std::is_lvalue_reference_v<decltype(iter)>)
        return result;
    else
        return result ? iter::item(*result) : iter::noitem;
}

template<iter::assert_iterable I, class T>
constexpr auto ITER_IMPL(nth) (I&& iterable, std::size_t n, T&& fallback) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    auto result = iter::no_next<decltype(iter)>();
    while (iter::detail::emplace_next(result, iter) && n-- > 0);
    return result ? result.consume() : FWD(fallback);
}

#endif /* INCLUDE_ITER_NTH_HPP */

namespace iter::consumers { using iter::nth; }
#ifndef INCLUDE_ITER_MAX_HPP
#define INCLUDE_ITER_MAX_HPP

#ifndef ITER_CONSUMERS_MIN_HPP
#define ITER_CONSUMERS_MIN_HPP

ITER_DECLARE(min)
ITER_DECLARE(min_by)

namespace iter::detail::minmax {
    template<class C, iterable I, class F>
    requires concepts::owned_item<next_t<I>>
    constexpr auto apply(C&& comp, I&& iterable, F&& func) {
        decltype(auto) iter = to_iter(FWD(iterable));
        auto next = no_next<I>(), current = no_next<I>();
        auto emplace_next = [&]() -> bool { return detail::emplace_next(next, iter); };
        if (emplace_next()) {
            current = std::move(next);
            while (emplace_next())
                if (std::invoke(FWD(comp), std::invoke(FWD(func), as_const(*current), as_const(*next))))
                    *current = std::move(*next);
        }
        return current;
    }

    template<class C, iterable I, class F>
    requires concepts::stable_item<iter::next_t<I>>
        && (!concepts::owned_item<iter::next_t<I>>)
    constexpr auto apply(C&& comp, I&& iterable, F&& func) {
        decltype(auto) iter = to_iter(FWD(iterable));
        auto next = no_next<I>(), result = no_next<I>();
        auto emplace_next = [&]() -> bool { return next = impl::next(iter); };
        if (emplace_next()) {
            result = next;
            while (emplace_next())
                if (std::invoke(FWD(comp), std::invoke(FWD(func), as_const(*result), as_const(*next))))
                    result = next;
        }
        return result;
    }

    inline constexpr auto min = [](auto&& l) { return l > 0; };
    inline constexpr auto max = [](auto&& l) { return l < 0; };

    template<class C, iterable I, class F>
    requires concepts::owned_item<next_t<I>>
    constexpr auto by(C&& comp, I&& iterable, F&& func) {
        decltype(auto) iter = to_iter(FWD(iterable));
        auto next = no_next<I>(), current = no_next<I>();
        auto emplace_next = [&]() -> bool { return detail::emplace_next(next, iter); };
        if (emplace_next()) {
            auto current_proj = std::invoke(FWD(func), as_const(*next));
            current = std::move(next);
            while (emplace_next()) {
                auto next_proj = std::invoke(FWD(func), as_const(*next));
                if (std::invoke(FWD(comp), as_const(current_proj), as_const(next_proj))) {
                    *current = std::move(*next);
                    current_proj = std::move(next_proj);
                }
            }
        }
        return current;
    }

    template<class C, iterable I, class F>
    requires concepts::stable_item<next_t<I>>
        && (!concepts::owned_item<next_t<I>>)
    constexpr auto by(C&& comp, I&& iterable, F&& func) {
        decltype(auto) iter = to_iter(FWD(iterable));
        auto val = no_next<I>(), result = no_next<I>();
        auto emplace_next = [&]() -> bool { return val = impl::next(iter); };
        if (emplace_next()) {
            auto current_proj = std::invoke(FWD(func), as_const(*val));
            result = val;
            while (emplace_next()) {
                auto next_proj = std::invoke(FWD(func), as_const(*val));
                if (std::invoke(FWD(comp), as_const(current_proj), as_const(next_proj))) {
                    result = val;
                    current_proj = std::move(next_proj);
                }
            }
        }
        return result;
    }

    inline constexpr auto min_by = [](auto&& next, auto&& current) { return next > current; };
    inline constexpr auto max_by = [](auto&& next, auto&& current) { return next < current; };
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

#endif /* ITER_CONSUMERS_MIN_HPP */

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

namespace iter::consumers { using iter::max; }

namespace iter::consumers { using iter::min; }
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
    if constexpr (iter::concepts::owned_item<decltype(val)> || std::is_lvalue_reference_v<decltype(iter)>)
        return val;
    else
        return val ? iter::item(val.consume()) : iter::noitem;
}

#endif /* INCLUDE_ITER_FIND_LINEAR_HPP */

namespace iter::consumers { using iter::find_linear; }
#ifndef INCLUDE_ITER_FIND_MAP_HPP
#define INCLUDE_ITER_FIND_MAP_HPP

ITER_DECLARE(find_map)

template<iter::assert_iterable I, std::invocable<iter::consume_t<I>> F>
constexpr auto ITER_IMPL(find_map) (I&& iterable, F&& func) {
    auto fm = iter::filter_map(FWD(iterable), FWD(func));
    if constexpr (iter::concepts::owned_item<iter::next_t<decltype(fm)>>)
        return iter::traits::next(fm);
    else {
        auto val = iter::traits::next(fm);
        return val ? iter::item{val.consume()} : iter::noitem;
    }
}

#endif /* INCLUDE_ITER_FIND_MAP_HPP */

namespace iter::consumers { using iter::find_map; }

// Predicates
#ifndef INCLUDE_ITER_ANY_HPP
#define INCLUDE_ITER_ANY_HPP

ITER_DECLARE(any)

template<iter::assert_iterable I, std::predicate<iter::ref_t<I>> P>
constexpr auto ITER_IMPL(any) (I&& iterable, P&& predicate) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (auto val = iter::traits::next(iter)) {
        if (FWD(predicate)(*val)) {
            return true;
        }
    }

    return false;
}

#endif /* INCLUDE_ITER_ANY_HPP */

namespace iter::consumers { using iter::any; }
#ifndef ITER_CONSUMERS_ALL_HPP
#define ITER_CONSUMERS_ALL_HPP

ITER_DECLARE(all)

template<iter::assert_iterable I, std::predicate<iter::ref_t<I>> P>
constexpr auto ITER_IMPL(all) (I&& iterable, P&& predicate) {
    decltype(auto) iter = iter::to_iter(FWD(iterable));
    while (auto val = iter::traits::next(iter)) {
        if (!FWD(predicate)(*val)) {
            return false;
        }
    }

    return true;
}

#endif /* ITER_CONSUMERS_ALL_HPP */

namespace iter::consumers { using iter::all; }

#endif /* ITER_CONSUMERS_CONSUMERS_HPP */

#ifndef ITER_COLLECTORS_COLLECTORS_HPP
#define ITER_COLLECTORS_COLLECTORS_HPP

#ifndef ITER_COLLECTORS_COLLECT_HPP
#define ITER_COLLECTORS_COLLECT_HPP

XTD_INVOKER(iter_collect)

namespace iter {
    namespace detail::tag {
        template<template<class...> class C = std::vector, template<class> class A = std::allocator, template<class> class... Traits>
        struct collect : xtd::tagged_bindable<collect<C, A, Traits...>, xtd::invokers::iter_collect> {};
    }

    template<template<class...> class C = std::vector, template<class> class A = std::allocator, template<class> class... Traits>
    inline constexpr detail::tag::collect<C, A, Traits...> collect;
}

ITER_ALIAS(to_vector, collect<std::vector>)
ITER_ALIAS(to_map, collect<std::map>)
ITER_ALIAS(to_string, collect<std::basic_string, std::allocator, std::char_traits>)

template<template<class...> class CT, template<class> class AT, template<class> class... Traits, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_collect, iter::detail::tag::collect<CT, AT, Traits...>)(I&& iter) {
    using T = iter::value_t<I>;
    CT<T, Traits<T>..., AT<T>> container;
    if constexpr (iter::concepts::random_access_iter<I>) {
        container.reserve(iter::traits::random_access::size(iter));
    }
    while (auto val = iter::traits::next(iter)) {
        container.push_back(iter::detail::consume(val));
    }
    return container;
}

template<template<class...> class CT, template<class> class AT, template<class> class... Traits, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_collect, iter::detail::tag::collect<CT, AT, Traits...>)(I&& iter, std::size_t reserve) {
    using T = iter::value_t<I>;
    CT<T, Traits<T>..., AT<T>> container;
    if constexpr (iter::concepts::random_access_iter<I>) {
        reserve = std::max(reserve, iter::traits::random_access::size(iter));
    }
    container.reserve(reserve);
    while (auto val = iter::traits::next(iter)) {
        container.push_back(iter::detail::consume(val));
    }
    return container;
}

template<template<class> class AT, iter::assert_iter I, class Comp>
constexpr auto XTD_IMPL_TAG_(iter_collect, iter::detail::tag::collect<std::map, AT>)(I&& iter, Comp&& compare) {
    using KV = iter::value_t<I>;
    using K = std::tuple_element_t<0, KV>;
    using V = std::tuple_element_t<1, KV>;
    using A = AT<std::pair<K const, V>>;
    std::map<K, V, std::remove_cvref_t<Comp>, A> container(FWD(compare));
    while (auto val = iter::traits::next(iter)) {
        container.emplace(iter::detail::consume(val));
    }
    return container;
}

template<template<class> class AT, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_collect, iter::detail::tag::collect<std::map, AT>)(I&& iter) {
    using KV = iter::value_t<I>;
    using K = std::tuple_element_t<0, KV>;
    return iter::collect<std::map, AT>(FWD(iter), std::less<K>{});
}

#endif /* ITER_COLLECTORS_COLLECT_HPP */

namespace iter::collectors { using iter::collect; }
#ifndef ITER_COLLECTORS_PARTITION_HPP
#define ITER_COLLECTORS_PARTITION_HPP

ITER_DECLARE(partition)

namespace iter {
    template<std::size_t I>
    struct part_t : part_t<I+1> {
        constexpr part_t() : part_t<I+1>{I} {}
    protected:
        constexpr part_t(size_t i) : part_t<I+1>{i} {}
    };

    template<>
    struct part_t<6> {
        constexpr std::size_t value() const { return index; }
    protected:
        constexpr part_t(size_t i) : index{i} {}
        std::size_t const index;
    };

    template<std::size_t I>
    inline constexpr auto part = part_t<I>{};

    namespace concepts {
        template<class T>
        inline constexpr bool is_partition_index = false;
        template<std::size_t I>
        inline constexpr bool is_partition_index<part_t<I>> = true;

        template<class T>
        concept partition_index = is_partition_index<T>;
    }

    template<std::size_t I>
    static constexpr auto parts = []<std::size_t... Is>(std::index_sequence<Is...>) {
        return std::array<part_t<I>, I+1>{part_t<Is>{}...};
    }(std::make_index_sequence<I+1>{});
}

template<iter::assert_iterable I, class F>
constexpr decltype(auto) ITER_IMPL(partition) (I&& iterable, F&& func) {
    using part_t = std::invoke_result_t<F, iter::consume_t<I>>;
    constexpr std::size_t N = []{
        if constexpr (std::is_same_v<bool, part_t>)
            return 2;
        else {
            static_assert(iter::concepts::partition_index<part_t>);
            return 1 + part_t{}.value();
        }
    }();
    static_assert(N > 1, "Must partition at least 2 ways");
    auto out = std::array<std::vector<iter::value_t<I>>, N>{};

    decltype(auto) iter = iter::to_iter(FWD(iterable));

    if constexpr (iter::concepts::random_access_iter<decltype(iter)>) {
        std::size_t size = iter::traits::random_access::size(iter) / N;
        apply([=](auto&&... outs) { (outs.reserve(size), ...); }, out);
    }

    while (auto val = iter::traits::next(iter)) {
        auto slot = std::invoke(FWD(func), iter::as_const(*val));
        std::size_t index;
        if constexpr (std::is_same_v<bool, part_t>) {
            index = slot ? 0 : 1;
        } else {
            index = slot.value();
        }

        out[index].emplace_back(iter::detail::consume(val));
    }
    return out;
}

#endif /* ITER_COLLECTORS_PARTITION_HPP */

namespace iter::collectors { using iter::partition; }
#ifndef ITER_COLLECTORS_SORTED_HPP
#define ITER_COLLECTORS_SORTED_HPP

XTD_INVOKER(iter_sorted)

namespace iter {
    namespace detail::tag {
        template<template<class...> class C = std::vector, template<class> class A = std::allocator>
        struct sorted_ : xtd::tagged_bindable<sorted_<C, A>, xtd::invokers::iter_sorted> {};
    }

    template<template<class...> class C = std::vector, template<class> class A = std::allocator>
    inline constexpr detail::tag::sorted_<C, A> sorted_;
}

ITER_ALIAS(sorted, sorted_<>)

template<template<class...> class CT, template<class> class AT,
         iter::assert_iter I, std::invocable<iter::ref_t<I>, iter::ref_t<I>> P>
constexpr auto XTD_IMPL_TAG_(iter_sorted, iter::detail::tag::sorted_<CT, AT>)(I&& iter, P&& predicate) {
    auto container = iter::collect<CT, AT>(FWD(iter));
    std::sort(std::begin(container), std::end(container), FWD(predicate));
    return container;
}

template<template<class...> class CT, template<class> class AT, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_sorted, iter::detail::tag::sorted_<CT, AT>)(I&& iter) {
    auto container = iter::collect<CT, AT>(FWD(iter));
    std::sort(std::begin(container), std::end(container));
    return container;
}

#endif /* ITER_COLLECTORS_SORTED_HPP */

namespace iter::collectors { using iter::sorted; using iter::sorted_; }
#ifndef ITER_COLLECTORS_TO_INPUT_RANGE_HPP
#define ITER_COLLECTORS_TO_INPUT_RANGE_HPP

ITER_DECLARE(to_input_range)

namespace iter::detail {
    template<iter I>
    struct input_range {
        constexpr explicit input_range(auto&& it)
            : i{FWD(it)}
            , current{impl::next(i)}
        {}

        struct iterator : iterator_traits<I> {
            explicit iterator(input_range* outer)
                : outer{outer}
            {}

            iterator() = default;

            auto operator<=>(const iterator&) const = delete;
            constexpr bool operator==(const iterator& other) const { return outer == other.outer; }

            constexpr bool operator!=(sentinel_t) const { return outer && outer->current.has_value(); }
            constexpr bool operator==(sentinel_t) const { return !operator!=(sentinel); }
            constexpr auto& operator*() const { return *outer->current; }
            constexpr auto* operator->() const { return std::addressof(*outer->current); }
            constexpr auto& operator++() {
                if (outer)
                    emplace_next(outer->current, outer->i);
                return *this;
            }
            constexpr void operator++(int) {
                if (outer)
                    emplace_next(outer->current, outer->i);
            };

        private:
            input_range* outer;
        };

        constexpr auto begin() { return iterator(this); }
        constexpr auto end() const { return sentinel; }

    private:
        [[no_unique_address]] I i;
        next_t<I> current;
    };

    template<class I>
    input_range(I) -> input_range<I>;
}

template<iter::iter I>
constexpr auto ITER_IMPL(to_input_range)(I&& i) {
    return iter::detail::input_range{FWD(i)};
}

#endif /* ITER_COLLECTORS_TO_INPUT_RANGE_HPP */

namespace iter::collectors { using iter::to_input_range; }
#ifndef INCLUDE_ITER_UNZIP_HPP
#define INCLUDE_ITER_UNZIP_HPP

XTD_INVOKER(iter_unzip)

namespace iter {
    namespace detail::tag {
        template<template<class...> class C = std::vector, template<class> class A = std::allocator>
        struct unzip_ : xtd::tagged_bindable<unzip_<C, A>, xtd::invokers::iter_unzip> {};
    }

    template<template<class...> class C = std::vector, template<class> class A = std::allocator>
    inline constexpr detail::tag::unzip_<C, A> unzip_;

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
constexpr auto XTD_IMPL_TAG_(iter_unzip, iter::detail::tag::unzip_<CT, AT>)(I&& iter) {
    using traits = iter::detail::unzipped<CT, AT, iter::value_t<I>>;
    typename traits::type containers{};

    if constexpr (iter::concepts::random_access_iter<I>) {
        apply([size = iter::traits::random_access::size(iter)](auto&... c) {
            (c.reserve(size), ...);
        }, containers);
    }
    while (auto val = iter::traits::next(iter)) {
        [&]<std::size_t... Is>(std::index_sequence<Is...>) {
            (get<Is>(containers).push_back(get<Is>(iter::detail::consume(val))), ...);
        }(std::make_index_sequence<traits::size>{});
    }
    return containers;
}

template<template<class...> class CT, template<class> class AT, iter::assert_iter I>
constexpr auto XTD_IMPL_TAG_(iter_unzip, iter::detail::tag::unzip_<CT, AT>)(I&& iter, std::size_t reserve) {
    using traits = iter::detail::unzipped<CT, AT, iter::value_t<I>>;
    typename traits::type containers{};

    if constexpr (iter::concepts::random_access_iter<I>)
        reserve = std::max(reserve, iter::traits::random_access::size(iter));

    apply([=](auto&... c) {
        (c.reserve(reserve), ...);
    }, containers);

    while (auto val = iter::traits::next(iter)) {
        [&]<std::size_t... Is>(std::index_sequence<Is...>) {
            (get<Is>(containers).push_back(get<Is>(iter::detail::consume(val))), ...);
        }(std::make_index_sequence<traits::size>{});
    }

    return containers;
}

#endif /* INCLUDE_ITER_UNZIP_HPP */

namespace iter::collectors { using iter::unzip; using iter::unzip_; }

#endif /* ITER_COLLECTORS_COLLECTORS_HPP */

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
    auto size = iter::traits::random_access::size(i1);
    if (size != iter::traits::random_access::size(i2)) return false;

    for (std::size_t i = 0; i < size; ++i) {
        decltype(auto) item1 = iter::traits::random_access::get(i1, i);
        decltype(auto) item2 = iter::traits::random_access::get(i2, i);
        if (item1 != item2) return false;
    }

    return true;
}

template<iter::iter I1, iter::iter I2>
constexpr std::compare_three_way_result_t<iter::ref_t<I1>, iter::ref_t<I2>> operator<=>(I1 i1, I2 i2) {
    auto item1 = iter::no_next<I1>();
    auto item2 = iter::no_next<I2>();

    while (!!iter::detail::emplace_next(item1, i1)
         & !!iter::detail::emplace_next(item2, i2)) {
        if (auto rel = *item1 <=> *item2; rel != 0) return rel;
    }

    return !!item1 <=> !!item2;
}

#endif /* INCLUDE_ITER_COMPARISON_HPP */

// Must be included last
#ifndef INCLUDE_ITER_WRAP_HPP
#define INCLUDE_ITER_WRAP_HPP

namespace iter {
    template<iterable I>
    struct [[nodiscard]] wrap : wrap<iter_t<I>> {
        template<class II>
        constexpr wrap(II&& iterable) : wrap<iter_t<I>>{to_iter(FWD(iterable))} {}
    };

    namespace detail {
        template<xtd::concepts::Bindable Tag, class... Ts>
        static constexpr decltype(auto) wrap_invoke(Tag const& tag, Ts&&... args) {
            auto call = [&]() -> decltype(auto) { return tag(FWD(args)...); };
            if constexpr (iter<decltype(call())>)
                return wrap{call()};
            else
                return call();
        }
    }

    template<iter I>
    struct [[nodiscard]] wrap<I> {
        [[no_unique_address]] I i;

        using this_t = wrap;
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            return iter::traits::next(self.i);
        }
        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self)
            requires concepts::double_ended_iter<I>
        {
            return iter::traits::double_ended::next_back(self.i);
        }
        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index)
            requires concepts::random_access_iter<I>
        {
            return iter::traits::random_access::get(self.i, index);
        }
        constexpr std::size_t ITER_IMPL_SIZE (this_t const& self)
            requires concepts::random_access_iter<I>
        {
            return iter::traits::random_access::size(self.i);
        }

#define ITER_X(fun) \
        template<class... Ts>\
        constexpr decltype(auto) fun(Ts&&... args) & {\
            return detail::wrap_invoke(iter::fun, i, FWD(args)...);\
        }\
        template<class... Ts>\
        constexpr decltype(auto) fun(Ts&&... args) && {\
            return detail::wrap_invoke(iter::fun, std::move(i), FWD(args)...);\
        }
/* Do not modify, generated by scripts/update_x_macros.sh */

// Invoke iter::cycle on this iter
ITER_X(cycle)
// Invoke iter::flatten on this iter
ITER_X(flatten)
// Invoke iter::batching on this iter
ITER_X(batching)
// Invoke iter::box on this iter
ITER_X(box)
// Invoke iter::chain on this iter
ITER_X(chain)
// Invoke iter::take on this iter
ITER_X(take)
// Invoke iter::chunks (aka iter::chunks_<>) on this iter
ITER_X(chunks)
// Invoke iter::chunk_by on this iter
ITER_X(chunk_by)
// Invoke iter::consteval_only on this iter
ITER_X(consteval_only)
// Invoke iter::flatmap on this iter
ITER_X(flatmap)
// Invoke iter::flat_map (aka iter::flatmap) on this iter
ITER_X(flat_map)
// Invoke iter::filter_map on this iter
ITER_X(filter_map)
// Invoke iter::map on this iter
ITER_X(map)
// Invoke iter::keys on this iter
ITER_X(keys)
// Invoke iter::values on this iter
ITER_X(values)
// Invoke iter::zip_map on this iter
ITER_X(zip_map)
// Invoke iter::enumerate_map (aka iter::enumerate_map_<>) on this iter
ITER_X(enumerate_map)
// Invoke iter::zip on this iter
ITER_X(zip)
// Invoke iter::enumerate (aka iter::enumerate_<>) on this iter
ITER_X(enumerate)
// Invoke iter::filter on this iter
ITER_X(filter)
// Invoke iter::inspect on this iter
ITER_X(inspect)
// Invoke iter::map_while on this iter
ITER_X(map_while)
// Invoke iter::move on this iter
ITER_X(move)
// Invoke iter::reverse on this iter
ITER_X(reverse)
// Invoke iter::skip_eager on this iter
ITER_X(skip_eager)
// Invoke iter::skip_while on this iter
ITER_X(skip_while)
// Invoke iter::skip on this iter
ITER_X(skip)
// Invoke iter::split on this iter
ITER_X(split)
// Invoke iter::take_while on this iter
ITER_X(take_while)
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
// Invoke iter::partition on this iter
ITER_X(partition)
// Invoke iter::sorted (aka iter::sorted_<>) on this iter
ITER_X(sorted)
// Invoke iter::to_input_range on this iter
ITER_X(to_input_range)
// Invoke iter::unzip (aka iter::unzip_<>) on this iter
ITER_X(unzip)

#undef ITER_X

#define ITER_EXPAND(...) __VA_ARGS__
#define ITER_X(fun, tmplParams, tmplArgs) \
        template<ITER_EXPAND tmplParams, class... Ts>\
        constexpr decltype(auto) fun(Ts&&... args) & {\
            return detail::wrap_invoke(iter::fun<ITER_EXPAND tmplArgs>, i, FWD(args)...);\
        }\
        template<ITER_EXPAND tmplParams, class... Ts>\
        constexpr decltype(auto) fun(Ts&&... args) && {\
            return detail::wrap_invoke(iter::fun<ITER_EXPAND tmplArgs>, std::move(i), FWD(args)...);\
        }
/* Do not modify, generated by scripts/update_x_macros.sh */

// Invoke iter::chunks_ on this iter
ITER_X(chunks_, (std::size_t N = 0), (N))
// Invoke iter::elements on this iter
ITER_X(elements, (std::size_t N), (N))
// Invoke iter::enumerate_map_ on this iter
ITER_X(enumerate_map_, (class T = std::size_t), (T))
// Invoke iter::enumerate_ on this iter
ITER_X(enumerate_, (class T = std::size_t), (T))
// Invoke iter::window on this iter
ITER_X(window, (std::size_t N = 2), (N))
// Invoke iter::collect on this iter
ITER_X(collect, (template<class...> class C = std::vector, template<class> class A = std::allocator, template<class> class... Traits), (C, A, Traits...))
// Invoke iter::sorted_ on this iter
ITER_X(sorted_, (template<class...> class C = std::vector, template<class> class A = std::allocator), (C, A))
// Invoke iter::unzip_ on this iter
ITER_X(unzip_, (template<class...> class C = std::vector, template<class> class A = std::allocator), (C, A))

#undef ITER_EXPAND
#undef ITER_X
    };

    template<iter::iterable I>
    requires (!iter::iter<I>)
    wrap(I&&) -> wrap<I>;

    template<iter::iter I>
    wrap(I) -> wrap<I>;
}

#endif /* INCLUDE_ITER_WRAP_HPP */

#endif /* INCLUDE_ITER_ITER_HPP */

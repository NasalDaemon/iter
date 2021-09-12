#ifndef INCLUDE_ITER_CORE_HPP
#define INCLUDE_ITER_CORE_HPP

#include "iter/version.hpp"

#include "extend.hpp"

#include <optional>
#include <memory>
#include <limits>

#ifndef FWD
#  define FWD(arg) static_cast<decltype(arg)&&>(arg)
#endif

#include "iter/emplace_new.hpp"
#include "iter/tuple.hpp"

#ifndef ITER_GLOBAL_INVOKER
#  define ITER_INVOKER(name) XTD_INVOKER(iter_ ## name)
#  define ITER_FUNCTION(fun, ...) XTD_FUNCTION_(iter_ ## fun) fun
#  define ITER_IMPL(name) XTD_IMPL_(iter_ ## name, iter::name)
#  define ITER_IMPL_THIS(name) XTD_IMPL_THIS_(iter_ ## name, iter::name)
#  define ITER_UNSAFE_IMPL(name) XTD_IMPL_(iter_ ## name, iter::unsafe::name)
#  define ITER_UNSAFE_IMPL_THIS(name) XTD_IMPL_THIS_(iter_ ## name, iter::unsafe::name)
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
#  define ITER_UNSAFE_IMPL_THIS(name) XTD_IMPL_THIS(iter::unsafe::name)
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
ITER_INVOKER(next_back)

namespace iter {
    namespace unsafe {
        // Random access functions
        ITER_FUNCTION(get);
        ITER_FUNCTION(size);
        ITER_FUNCTION(next_back);
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
        concept double_ended_iter = iter<T> && requires (T it, std::size_t index) {
            { iter::unsafe::next_back(it) } -> std::same_as<decltype(iter::next(it))>;
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

    namespace unsafe {
        template<class I>
        auto get_type() -> void;
        template<concepts::random_access_iter I>
        auto get_type() -> decltype(iter::unsafe::get(std::declval<I&>(), 0ul));
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

            constexpr auto ITER_UNSAFE_SIZE (this_t const& base) {
                return iter::unsafe::size(static_cast<Self const&>(base).i);
            }
            constexpr auto ITER_IMPL_THIS(next) (this_t& base) {
                auto index = base.index++;
                auto& self = static_cast<Self&>(base);
                return iter::unsafe::get_option(self, index);
            }
            constexpr auto ITER_UNSAFE_IMPL_THIS(next_back) (this_t& base) {
                auto index = base.index++;
                auto& self = static_cast<Self&>(base);
                return iter::unsafe::get_option(self, iter::unsafe::size(self) - 1 - index);
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

            constexpr auto ITER_UNSAFE_SIZE (this_t const& base) {
                return base.size;
            }
            constexpr auto ITER_IMPL_THIS(next) (this_t& base) {
                auto index = base.index++;
                auto& self = static_cast<Self&>(base);
                return iter::unsafe::get_option(self, index);
            }
            constexpr auto ITER_UNSAFE_IMPL_THIS(next_back) (this_t& base) {
                auto index = base.index++;
                auto& self = static_cast<Self&>(base);
                return iter::unsafe::get_option(self, iter::unsafe::size(self) - 1 - index);
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

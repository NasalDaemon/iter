#ifndef INCLUDE_ITER_CORE_HPP
#define INCLUDE_ITER_CORE_HPP

#include "iter/version.hpp"

#include "extend.hpp"

#include <optional>
#include <memory>
#include <limits>

#include "iter/emplace_new.hpp"
#include "iter/item.hpp"
#include "iter/tuple.hpp"

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
        template<concepts::item T>
        auto get_value_t(T) -> typename T::value_type;
        template<concepts::pointer T>
        auto get_value_t(T) -> typename std::iterator_traits<T>::value_type;
    }

    namespace concepts {
        template<class T>
        concept iter = requires(T it) {
            { iter::detail::impl::next(it) } -> item;
        };

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
    using next_t = decltype(iter::detail::impl::next(std::declval<iter_t<I>&>()));

    template<iter I>
    struct iterator_traits {
        using next_t = iter::next_t<I>;
        using iter_t = I;
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

        static constexpr struct sentinel_t {} sentinel;

        // C++ style iterator_wrapper wrapper (sniff...)
        template<iter I>
        requires (!std::is_const_v<I>) && (!std::is_reference_v<I>)
        struct iterator_wrapper : iterator_traits<I> {
            using traits = iterator_traits<I>;
            using typename traits::value_type;

            explicit constexpr iterator_wrapper(I& it) : it{std::addressof(it)}
            {}
            constexpr iterator_wrapper() = default;

            auto operator<=>(const iterator_wrapper&) const = delete;

            // const to follow std::ranges::range concept,
            constexpr bool operator!=(sentinel_t) const {
                return detail::emplace_next(current, *it).has_value();
            }
            constexpr bool operator==(sentinel_t) const {
                return !operator!=(sentinel);
            }
            constexpr auto& operator*() {
                return *current;
            }
            constexpr auto& operator*() const {
                return *current;
            }
            constexpr auto* operator->() {
                return std::addressof(*current);
            }
            constexpr auto* operator->() const {
                return std::addressof(*current);
            }
            constexpr auto& operator++() {
                return *this;
            }
            constexpr void operator++(int) {}

        private:
            I* it;
            mutable next_t<I> current;
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
            return static_cast<T const&&>(in);
    }

    namespace concepts {
        template<class T>
        static constexpr bool is_iterator_v = false;
        template<class I>
        constexpr bool is_iterator_v<iter::detail::iterator_wrapper<I>> = true;

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
        auto get_type() -> decltype(iter::detail::impl::get(std::declval<I&>(), 0ul));
        template<class I>
        using get_t = decltype(get_type<I>());

        template<class I>
        [[nodiscard]] constexpr auto get_item(I&& iter, std::size_t index) {
            std::size_t size = impl::size(iter);
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
                auto index = base.index++;
                auto& self = static_cast<Self&>(base);
                return get_item(self, index);
            }
            constexpr auto ITER_IMPL_NEXT_BACK (this_t& base) {
                auto index = base.index++;
                auto& self = static_cast<Self&>(base);
                return get_item(self, impl::size(self) - 1 - index);
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
                return get_item(self, index);
            }
            constexpr auto ITER_IMPL_NEXT_BACK (this_t& base) {
                auto index = base.index++;
                auto& self = static_cast<Self&>(base);
                return get_item(self, impl::size(self) - 1 - index);
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

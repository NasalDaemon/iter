#ifndef ITER_CORE_CORE_HPP
#define ITER_CORE_CORE_HPP

#include "iter/core/version.hpp"

#include "extend/extend.hpp"

#include <compare>
#include <cstdint>
#include <limits>
#include <memory>
#include <utility>

#include "iter/core/macros.hpp"
#include "iter/core/emplace_new.hpp"
#include "iter/core/item.hpp"
#include "iter/core/tuple.hpp"

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

#ifndef ITER_ITERS_GENERATOR_HPP
#define ITER_ITERS_GENERATOR_HPP

#include "iter/core/core.hpp"

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

#include "iter/iters/generate.hpp"
#include "iter/adapters/flatten.hpp"

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

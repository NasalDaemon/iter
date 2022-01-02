#if __cpp_impl_coroutine >= 201902L && !defined(INCLUDE_ITER_GENERATOR_HPP)
#define INCLUDE_ITER_GENERATOR_HPP

#include "iter/core.hpp"

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

        constexpr item<T&> next() {
            if (!m_coroutine) [[unlikely]]
                return noitem;

            m_coroutine.resume();
            if (m_coroutine.done()) [[unlikely]] {
                m_coroutine.promise().rethrow_if_exception();
                return noitem;
            }

            return item_from_pointer(m_coroutine.promise().value());
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

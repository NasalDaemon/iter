#ifndef ITER_CORE_MACROS_HPP
#define ITER_CORE_MACROS_HPP

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
#define ITER_IMPL_GET       XTD_IMPL_THIS_(iter_get, iter::detail::impl::get)
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
#  define ITER_COMPILER_CLANG
#  define ITER_ASSUME(condition) __builtin_assume(!!(condition))
#  define ITER_UNREACHABLE() __builtin_unreachable()
#elif defined(__GNUC__) || defined (__GNUG__)
#  define ITER_COMPILER_GCC
#  define ITER_ASSUME(condition) do { if(!(condition)) __builtin_unreachable(); } while(0)
#  define ITER_UNREACHABLE() __builtin_unreachable()
#elif defined(_MSC_VER)
#  define ITER_COMPILER_MSVC
#  define ITER_ASSUME(condition) __assume(!!(condition))
#  define ITER_UNREACHABLE() __assume(0)
#else
#  define ITER_ASSUME(condition) do { } while(0)
#  define ITER_UNREACHABLE() do { } while(0)
#endif

#if __cpp_impl_coroutine >= 201902L
#  define ITER_COROUTINE
#endif

#endif /* ITER_CORE_MACROS_HPP */

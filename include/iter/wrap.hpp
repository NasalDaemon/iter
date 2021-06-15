#ifndef INCLUDE_ITER_WRAP_HPP
#define INCLUDE_ITER_WRAP_HPP

#include "iter.hpp"

namespace iter {
    template<iterable I>
    struct [[nodiscard]] wrap : wrap<iter_t<I>> {
        template<class II>
        wrap(II&& iterable) : wrap<iter_t<I>>{to_iter((II&&) iterable)} {}
    };
    template<iter I>
    struct [[nodiscard]] wrap<I> : I {

#define ITER_X(fun) \
        template<class... Ts>\
        decltype(auto) fun(Ts&&... args) {\
            return invoke(::iter::fun, std::forward<Ts>(args)...);\
        }
#include "iter/x_macros/iter_functions_simple.ipp"
#undef ITER_X

#define ITER_EXPAND(...) __VA_ARGS__
#define ITER_X(fun, tmplParams, tmplArgs) \
        template<ITER_EXPAND tmplParams, class... Ts>\
        decltype(auto) fun(Ts&&... args) {\
            return invoke(::iter::fun<ITER_EXPAND tmplArgs>, std::forward<Ts>(args)...);\
        }
#include "iter/x_macros/iter_functions_tmpl.ipp"
#undef ITER_EXPAND
#undef ITER_X

        template<xtd::concepts::Bindable Tag, class... Ts>
        decltype(auto) invoke(Tag const& tag, Ts&&... args) {
            auto call = [&]() -> decltype(auto) { return tag(*this, std::forward<Ts>(args)...); };
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

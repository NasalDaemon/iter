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
#include "iter/x_macros/iter_functions.hpp"
#undef ITER_X

        template<xtd::concepts::Bindable Tag, class... Ts>
        decltype(auto) invoke(Tag const& tag, Ts&&... args) {
            auto call = [&]() -> decltype(auto) { return tag(static_cast<I&>(*this), std::forward<Ts>(args)...); };
            if constexpr (iter<decltype(call())>)
                return ::iter::wrap{call()};
            else
                return call();
        }
    };

    template<iter::iterable I>
    wrap(I) -> wrap<I>;
}

#endif /* INCLUDE_ITER_WRAP_HPP */

#ifndef INCLUDE_ITER_WRAP_HPP
#define INCLUDE_ITER_WRAP_HPP

#include "iter.hpp"

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

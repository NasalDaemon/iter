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
        [[no_unique_address]] I i;

        using this_t = wrap;
        constexpr auto next() & { return iter::next(i); }
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) { return self.next(); }

        constexpr decltype(auto) ITER_UNSAFE_GET (this_t& self, std::size_t index)
            requires concepts::random_access_iter<I>
        {
            return iter::unsafe::get(self.i, index);
        }

        constexpr std::size_t ITER_UNSAFE_SIZE (this_t const& self)
            requires concepts::random_access_iter<I>
        {
            return iter::unsafe::size(self.i);
        }

#define ITER_X(fun) \
        template<class... Ts>\
        constexpr decltype(auto) fun(Ts&&... args) & {\
            return invoke(::iter::fun, i, FWD(args)...);\
        }\
        template<class... Ts>\
        constexpr decltype(auto) fun(Ts&&... args) && {\
            return invoke(::iter::fun, std::move(i), FWD(args)...);\
        }
#include "iter/x_macros/iter_functions_simple.ipp"
#undef ITER_X

#define ITER_EXPAND(...) __VA_ARGS__
#define ITER_X(fun, tmplParams, tmplArgs) \
        template<ITER_EXPAND tmplParams, class... Ts>\
        constexpr decltype(auto) fun(Ts&&... args) & {\
            return invoke(::iter::fun<ITER_EXPAND tmplArgs>, i, FWD(args)...);\
        }\
        template<ITER_EXPAND tmplParams, class... Ts>\
        constexpr decltype(auto) fun(Ts&&... args) && {\
            return invoke(::iter::fun<ITER_EXPAND tmplArgs>, std::move(i), FWD(args)...);\
        }
#include "iter/x_macros/iter_functions_tmpl.ipp"
#undef ITER_EXPAND
#undef ITER_X

    private:
        template<xtd::concepts::Bindable Tag, class Underlying, class... Ts>
        static constexpr decltype(auto) invoke(Tag const& tag, Underlying&& underlying, Ts&&... args) {
            auto call = [&]() -> decltype(auto) { return tag(FWD(underlying), FWD(args)...); };
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

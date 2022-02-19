#ifndef INCLUDE_ITER_WRAP_HPP
#define INCLUDE_ITER_WRAP_HPP

#include "iter.hpp"

namespace iter {
    template<iterable I>
    struct [[nodiscard]] wrap : wrap<iter_t<I>> {
        template<class II>
        wrap(II&& iterable) : wrap<iter_t<I>>{to_iter(FWD(iterable))} {}
    };

    namespace detail {
        template<xtd::concepts::Bindable Tag, class... Ts>
        static constexpr decltype(auto) wrap_invoke(Tag const& tag, Ts&&... args) {
            auto call = [&]() -> decltype(auto) { return tag(FWD(args)...); };
            if constexpr (iter<decltype(call())>)
                return wrap{call()};
            else
                return call();
        }
    }

    template<iter I>
    struct [[nodiscard]] wrap<I> {
        [[no_unique_address]] I i;

        using this_t = wrap;
        constexpr auto ITER_IMPL_NEXT (this_t& self) {
            return iter::detail::impl::next(self.i);
        }
        constexpr auto ITER_IMPL_NEXT_BACK (this_t& self)
            requires concepts::double_ended_iter<I>
        {
            return iter::detail::impl::next_back(self.i);
        }
        constexpr decltype(auto) ITER_IMPL_GET (this_t& self, std::size_t index)
            requires concepts::random_access_iter<I>
        {
            return iter::detail::impl::get(self.i, index);
        }
        constexpr std::size_t ITER_IMPL_SIZE (this_t const& self)
            requires concepts::random_access_iter<I>
        {
            return iter::detail::impl::size(self.i);
        }

#define ITER_X(fun) \
        template<class... Ts>\
        constexpr decltype(auto) fun(Ts&&... args) & {\
            return detail::wrap_invoke(iter::fun, i, FWD(args)...);\
        }\
        template<class... Ts>\
        constexpr decltype(auto) fun(Ts&&... args) && {\
            return detail::wrap_invoke(iter::fun, std::move(i), FWD(args)...);\
        }
#include "iter/macros/x/iter_functions_simple.ipp"
#undef ITER_X

#define ITER_EXPAND(...) __VA_ARGS__
#define ITER_X(fun, tmplParams, tmplArgs) \
        template<ITER_EXPAND tmplParams, class... Ts>\
        constexpr decltype(auto) fun(Ts&&... args) & {\
            return detail::wrap_invoke(iter::fun<ITER_EXPAND tmplArgs>, i, FWD(args)...);\
        }\
        template<ITER_EXPAND tmplParams, class... Ts>\
        constexpr decltype(auto) fun(Ts&&... args) && {\
            return detail::wrap_invoke(iter::fun<ITER_EXPAND tmplArgs>, std::move(i), FWD(args)...);\
        }
#include "iter/macros/x/iter_functions_tmpl.ipp"
#undef ITER_EXPAND
#undef ITER_X
    };

    template<iter::iterable I>
    requires (!iter::iter<I>)
    wrap(I&&) -> wrap<I>;

    template<iter::iter I>
    wrap(I) -> wrap<I>;
}

#endif /* INCLUDE_ITER_WRAP_HPP */

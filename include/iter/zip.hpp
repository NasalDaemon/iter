#ifndef INCLUDE_ITER_ZIP_HPP
#define INCLUDE_ITER_ZIP_HPP

#include "iter/core.hpp"

ITER_DECLARE(zip)

namespace iter::detail {
    // Simply dereference pointers to avoid copy/move construction
    // but unwrap optionals into new instances
    template<class T>
    static constexpr auto lazy_unwrap_next(T&& in) {
        using t = std::decay_t<T>;
        if constexpr (concepts::optional_next<t>)
            return [&] { return std::move(*in); };
        else
            return [&]() -> auto&& { return *in; };
    }

    template<assert_iter... I>
    requires (sizeof...(I) > 1)
    struct [[nodiscard]] zip_iter : enable_random_access<zip_iter<I...>, I...> {
        using this_t = zip_iter;

        [[no_unique_address]] tuple<I...> i;

        constexpr auto ITER_IMPL_THIS(next) (this_t& self)
            requires (!this_t::random_access)
        {
            return apply([](auto&... is) {
                return [](auto&&... vals) {
                    return (... && vals)
                        ? MAKE_OPTIONAL(make_tuple_lazy(lazy_unwrap_next(FWD(vals))...))
                        : std::nullopt;
                }(iter::next(is)...);
            }, self.i);
        }

        constexpr auto ITER_UNSAFE_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return apply([=](auto&... is) {
                return make_tuple_lazy([&, index]() -> decltype(auto) { return iter::unsafe::get(is, index); }...);
            }, self.i);
        }
    };

    template<class T> static constexpr bool is_zip = false;
    template<class... Ts> constexpr bool is_zip<zip_iter<Ts...>> = true;
    template<class T>
    concept decays_to_zip = is_zip<std::remove_cvref_t<T>>;
}

template<iter::assert_iterable... I>
constexpr auto ITER_IMPL(zip) (I&&... iterables) {
    auto zip = iter::detail::zip_iter<iter::iter_t<I>...>{.i = {iter::to_iter(FWD(iterables))...}};
    if constexpr(decltype(zip)::random_access) {
        zip.size = apply([](auto&... iters) {
            return std::min({iter::unsafe::size(iters)...});
        }, zip.i);
    }
    return zip;
}

template<iter::detail::decays_to_zip I, iter::assert_iterable... Is>
constexpr auto ITER_IMPL(zip) (I&& zip_iter, Is&&... iterables) {
    return apply([&](auto&&... zip_iters) {
        return iter::zip(FWD(zip_iters)..., FWD(iterables)...);
    }, FWD(zip_iter).i);
}

#endif /* INCLUDE_ITER_ZIP_HPP */

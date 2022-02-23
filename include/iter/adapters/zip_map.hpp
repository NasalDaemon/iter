#ifndef INCLUDE_ITER_ZIP_MAP_HPP
#define INCLUDE_ITER_ZIP_MAP_HPP

#include "iter/core/core.hpp"

ITER_DECLARE(zip_map)

namespace iter::detail {
    template<class F, assert_iter... I>
    requires (sizeof...(I) > 1) && std::invocable<F, consume_t<I>...>
    struct [[nodiscard]] zip_map_iter : enable_random_access<zip_map_iter<F, I...>, I...> {
        using this_t = zip_map_iter;

        [[no_unique_address]] F func;
        [[no_unique_address]] tuple<I...> i;

        constexpr auto ITER_IMPL_NEXT (this_t& self)
            requires (!this_t::random_access)
        {
            return apply([&](auto&... iters) {
                return [&](auto... items) {
                    return (... & items.has_value())
                        ? MAKE_ITEM_AUTO(self.func(consume(items)...))
                        : noitem;
                }(impl::next(iters)...);
            }, self.i);
        }

        constexpr auto ITER_IMPL_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return apply([=](auto&... iters) {
                return self.func(impl::get(iters, index)...);
            }, self.i);
        }
    };

    template<assert_iterable... Is, class F>
    constexpr decltype(auto) make_zip_map_iter(Is&&... iterables, F&& func)
    {
        return zip_map_iter<std::remove_cvref_t<F>, iter_t<Is>...>{
            .func{FWD(func)}, .i{to_iter(FWD(iterables))...}};
    }
}

// First args are iterables, last arg is function
template<class... Ts>
constexpr auto ITER_IMPL(zip_map) (Ts&&... args) {
    auto zip = [&]<std::size_t... I>(std::index_sequence<I...>) {
        // TODO: Avoid std::tuple_element_t
        return iter::detail::make_zip_map_iter<std::tuple_element_t<I, iter::tuple<Ts...>>...>(FWD(args)...);
    }(std::make_index_sequence<sizeof...(Ts) - 1>{});
    if constexpr(decltype(zip)::random_access) {
        zip.size = apply([](auto&... iters) {
            return std::min({iter::traits::random_access::size(iters)...});
        }, zip.i);
    }
    return zip;
}

#endif /* INCLUDE_ITER_ZIP_MAP_HPP */

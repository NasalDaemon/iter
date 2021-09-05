#ifndef INCLUDE_ITER_ZIP_HPP
#define INCLUDE_ITER_ZIP_HPP

#include "iter/core.hpp"

ITER_DECLARE(zip)

namespace iter::detail {
    // Tie only those arguments that are lvalue-references
    template<class... Ts>
    static constexpr std::tuple<Ts...> half_tie(Ts&&... ins) {
        return {FWD(ins)...};
    }

    // Simply dereference pointers to avoid copy/move construction
    // but unwrap optionals into new instances
    template<class T>
    static constexpr decltype(auto) unwrap_next(T&& in) {
        using t = std::decay_t<T>;
        if constexpr (concepts::optional_next<t>)
            return typename t::value_type(std::move(*in));
        else
            return (*in);
    }

    template<assert_iter... I>
    requires (sizeof...(I) > 1)
    struct [[nodiscard]] zip_iter : enable_random_access<zip_iter<I...>, I...> {
        using this_t = zip_iter;

        template<assert_iter... T>
        requires (sizeof...(T) > 1)
        friend struct zip_iter;

        template<class... Ts>
        requires (sizeof...(Ts) == sizeof...(I))
        constexpr zip_iter(Ts&&... ins)
            : i{FWD(ins)...}
        {
            if constexpr (this_t::random_access) {
                this->size = std::apply([](auto&... iters) {
                    return std::min({iter::unsafe::size(iters)...});
                }, i);
            }
        }

        template<class... Ts, class... Us>
        constexpr zip_iter(zip_iter<Ts...>&& zi, Us&&... ins)
            : zip_iter(std::index_sequence_for<Ts...>{}, std::move(zi), FWD(ins)...)
        {}
        template<class... Ts, class... Us>
        constexpr zip_iter(const zip_iter<Ts...>& zi, Us&&... ins)
            : zip_iter(std::index_sequence_for<Ts...>{}, std::move(zi), FWD(ins)...)
        {}

    private:
        template<size_t... Is, class... Ts, class... Us>
        constexpr zip_iter(std::index_sequence<Is...>, zip_iter<Ts...>&& zi, Us&&... ins)
            : zip_iter(std::move(std::get<Is>(zi.i))..., FWD(ins)...)
        {}
        template<size_t... Is, class... Ts, class... Us>
        constexpr zip_iter(std::index_sequence<Is...>, const zip_iter<Ts...>& zi, Us&&... ins)
            : zip_iter(std::get<Is>(zi.i)..., FWD(ins)...)
        {}

        [[no_unique_address]] std::tuple<I...> i;

        constexpr auto ITER_IMPL_THIS(next) (this_t& self)
            requires (!this_t::random_access)
        {
            return std::apply([](auto&&... is) {
                return std::invoke([]<class... Ts>(Ts&&... vals)  {
                    return (... && vals)
                        ? MAKE_OPTIONAL(half_tie(unwrap_next(FWD(vals))...))
                        : std::nullopt;
                }, iter::next(is)...);
            }, self.i);
        }

        constexpr auto ITER_UNSAFE_GET (this_t& self, std::size_t index)
            requires this_t::random_access
        {
            return std::apply([=](auto&&... is) {
                return half_tie(iter::unsafe::get(is, index)...);
            }, self.i);
        }
    };

    template<iter... I>
    zip_iter(I...) -> zip_iter<I...>;
    template<iter... ZI, iter... I>
    zip_iter(zip_iter<ZI...>, I...) -> zip_iter<ZI..., I...>;
}

template<iter::assert_iterable... I>
constexpr auto ITER_IMPL(zip) (I&&... iterables) {
    return iter::detail::zip_iter{iter::to_iter(FWD(iterables))...};
}

#endif /* INCLUDE_ITER_ZIP_HPP */

#ifndef ITER_ADAPTERS_ELEMENTS_HPP
#define ITER_ADAPTERS_ELEMENTS_HPP

#include "iter/core/core.hpp"
#include "iter/adapters/map.hpp"

XTD_INVOKER(iter_elements)
ITER_DECLARE(keys)
ITER_DECLARE(values)

namespace iter {
    namespace detail::tag {
        template<std::size_t N>
        struct elements : xtd::tagged_bindable<elements<N>, xtd::invokers::iter_elements> {};
    }

    template<std::size_t N>
    inline constexpr detail::tag::elements<N> elements;
}

template<std::size_t N, iter::assert_iterable I>
constexpr auto XTD_IMPL_TAG_(iter_elements, iter::detail::tag::elements<N>)(I&& iterable) {
    using std::get;
    return iter::map(FWD(iterable), [](auto&& value) {
        constexpr bool stable = iter::concepts::stable_iter<iter::iter_t<I>>;
        using element_t = std::tuple_element_t<N, iter::value_t<I>>;
        if constexpr (!std::is_reference_v<element_t> && !std::is_reference_v<iter::item_t<I>>)
            return MAKE_STABILITY(stable, get<N>(FWD(value)));
        else
            return MAKE_STABILITY_AUTO(stable, get<N>(FWD(value)));
    });
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(keys)(I&& iterable) {
    return iter::elements<0>(FWD(iterable));
}

template<iter::assert_iterable I>
constexpr auto ITER_IMPL(values)(I&& iterable) {
    return iter::elements<1>(FWD(iterable));
}

#endif /* ITER_ADAPTERS_ELEMENTS_HPP */

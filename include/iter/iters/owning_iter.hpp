#ifndef ITER_ITERS_OWNING_ITER_HPP
#define ITER_ITERS_OWNING_ITER_HPP

#include "iter/core/assert_consteval.hpp"
#include "iter/core/relocation.hpp"
#include "iter/iters/span.hpp"

namespace iter {
    // Owning iter that is disallowed from being relocated (copied/moved)
    // unless the relocation is elided or in a constant expression
    template<class T, tag::concepts::relocation Tag = tag::non_relocatable_t>
    struct owning_iter;

    template<concepts::random_access_container T, tag::concepts::relocation Tag>
    struct owning_iter<T, Tag> {
        using this_t = owning_iter;

        T container;
        [[no_unique_address]] typename Tag::template type<this_t> _relocation_enforcement{};
        std::size_t pos = 0;

        constexpr auto ITER_IMPL_NEXT(this_t& self) {
            return self.pos < std::size(self.container)
                ? iter::item_ref(self.container[self.pos++])
                : iter::noitem;
        }

        constexpr auto ITER_IMPL_NEXT_BACK(this_t& self) {
            const auto size = std::size(self.container);
            return self.pos < size
                ? iter::item_ref(size - 1 - self.container[self.pos++])
                : iter::noitem;
        }

        constexpr decltype(auto) ITER_IMPL_GET(this_t& self, std::size_t index) {
            return self.container[index];
        }
        constexpr decltype(auto) ITER_IMPL_SIZE(this_t const& self) {
            return std::size(self.container);
        }
    };

    template<class T>
    owning_iter(T) -> owning_iter<T>;
    template<class T, class Tag>
    owning_iter(T, Tag) -> owning_iter<T, Tag>;
}

#endif /* ITER_ITERS_OWNING_ITER_HPP */

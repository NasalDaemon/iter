#ifndef ITER_ADAPTERS_CHUNK_BY_HPP
#define ITER_ADAPTERS_CHUNK_BY_HPP

#include "iter/core/core.hpp"
#include "iter/iters/iter_ref.hpp"

ITER_DECLARE(chunk_by)

namespace iter::concepts {
    template<class F, class I>
    concept chunk_by_invocable_proj = requires (F func, cref_t<I> ref) {
        func(ref);
    };
    template<class F, class I>
    concept chunk_by_invocable_adj = requires (F func, cref_t<I> ref) {
        { func(ref, ref) } -> std::same_as<bool>;
    };

    template<class F, class I>
    concept chunk_by_invocable = chunk_by_invocable_proj<F, I> || chunk_by_invocable_adj<F, I>;
}

namespace iter::detail {
    template<concepts::stable_iter I, concepts::chunk_by_invocable<I> F>
    struct chunk_by_iter_base { // chunk_by_invocable_adj
        using this_t = chunk_by_iter_base;

        [[no_unique_address]] I i;
        [[no_unique_address]] F func;
        next_t<I> items[2]{};
        // 0, 1 : in middle of chunk, index to emplace into
        // 2 : end of chunk (index 0)
        // 3 : end of chunk (index 1)
        // 4 : no items left
        // 5 : iteration not started
        std::uint8_t index = 5;

        static constexpr bool stable = !concepts::owned_item<next_t<I>>;
        using item_t = item<ref_t<I>, stable>;
        using stability_t = typename item_t::stability_t;

        constexpr item_t ITER_IMPL_NEXT(this_t& self) {
            if (self.index > 1) { // start of next chunk
                self.index -= 2;
                // return item which failed chunk predicate
                return stability_t{*self.items[self.index ^ 1]};
            }
            if (auto& current = emplace_next(self.items[self.index], self.i)) {
                self.index ^= 1;
                if (auto& prev = self.items[self.index]) {
                    if (!self.func(std::as_const(*prev), std::as_const(*current))) {
                        self.index += 2; // predicate failed, make sure to return current value next time
                        return noitem;
                    }
                }
                return stability_t{*current};
            }
            self.index = 4;
            return noitem;
        }
    };

    template<concepts::stable_iter I, concepts::chunk_by_invocable_proj<I> F>
    struct chunk_by_iter_base<I, F> {
        using this_t = chunk_by_iter_base;
        using projection_t = make_item_t<std::invoke_result_t<F, cref_t<I>>>;
        static_assert(concepts::stable_item<projection_t>);
        
        static constexpr bool stable = !concepts::owned_item<next_t<I>>;
        using item_t = item<ref_t<I>, stable>;
        using stability_t = typename item_t::stability_t;

        [[no_unique_address]] I i;
        [[no_unique_address]] F func;
        next_t<I> last;
        projection_t projection;
        bool end = false;

        constexpr item_t ITER_IMPL_NEXT(this_t& self) {
            if (self.end && self.last) [[unlikely]] {
                self.end = false;
                return stability_t(*self.last);
            }

            if (emplace_next(self.last, self.i)) {
                if (self.projection) [[likely]] {
                    auto projected = MAKE_ITEM_AUTO(self.func(std::as_const(*self.last)));
                    if (*projected != *self.projection) {
                        self.projection = std::move(projected);
                        self.end = true;
                        return noitem;
                    }
                } else {
                    EMPLACE_NEW(self.projection, MAKE_ITEM_AUTO(self.func(std::as_const(*self.last))));
                }
                return stability_t(*self.last);
            } else {
                self.end = true;
            }
            return noitem;
        }
    };
    template<iter I, class F>
    struct [[nodiscard]] chunk_by_iter : chunk_by_iter_base<I, F> {
        using this_t = chunk_by_iter;
        using base_t = chunk_by_iter_base<I, F>;

        constexpr unstable_item<iter_ref<base_t>> ITER_IMPL_NEXT(this_t& self)
            requires concepts::chunk_by_invocable_proj<F, I> 
        {
            auto& base = static_cast<base_t&>(self);
            if (base.end == base.last.has_value())
                return iter_ref(base);
            else if (base.last) [[unlikely]] {
                while(auto n = impl::next(base));
                if (base.last.has_value())
                    return iter_ref(base);
            }
            return noitem;
        }

        constexpr unstable_item<iter_ref<base_t>> ITER_IMPL_NEXT(this_t& self)
            requires concepts::chunk_by_invocable_adj<F, I>
        {
            auto& base = static_cast<base_t&>(self);
            if (base.index == 5) [[unlikely]] { // kick start iteration
                base.index = 0;
            } else if (base.index == 4) [[unlikely]] { // no more items
                return noitem;
            } else if (base.index < 2) [[unlikely]] { // unfinished chunk
                while(auto n = impl::next(base));
                if (base.index == 4)
                    return noitem;
            }
            return iter_ref(base);
        }
    };
}

template<iter::assert_iterable I, iter::concepts::chunk_by_invocable<I> F>
constexpr auto ITER_IMPL(chunk_by)(I&& iterable, F&& func) {
    return iter::detail::chunk_by_iter{iter::to_iter(FWD(iterable)), FWD(func)};
}

#endif /* ITER_ADAPTERS_CHUNK_BY_HPP */

#ifndef INCLUDE_ITER_BOX_HPP
#define INCLUDE_ITER_BOX_HPP

#include "iter/core.hpp"

ITER_DECLARE(box)

namespace iter {
    template<concepts::next Next, class GetType = void>
    struct virtual_iter : virtual_iter<Next, void> {
        virtual std::size_t size() const = 0;
        virtual GetType get(std::size_t index) = 0;
    private:
        using this_t = virtual_iter;
        constexpr auto ITER_UNSAFE_GET (this_t& self, std::size_t index) { return self.get(index); }
        constexpr auto ITER_UNSAFE_SIZE (this_t const& self) { return self.size(); }
    };
    template<concepts::next Next>
    struct virtual_iter<Next, void> {
        virtual Next next() = 0;
        virtual ~virtual_iter() = default;
    private:
        using this_t = virtual_iter;
        constexpr auto ITER_IMPL_THIS(next) (this_t& self) { return self.next(); }
    };

    namespace detail {
        template<iter I>
        struct virtual_iter_impl final : virtual_iter<next_t<I>>, I {
            template<class... Ts>
            constexpr virtual_iter_impl(Ts&&... in) : I{FWD(in)...} {}
            next_t<I> next() final { return iter::next(static_cast<I&>(*this)); }
        };
        template<concepts::random_access_iter I>
        struct virtual_iter_impl<I> final : virtual_iter<next_t<I>, unsafe::get_t<I>>, I {
            template<class... Ts>
            constexpr virtual_iter_impl(Ts&&... in) : I{FWD(in)...} {}
            next_t<I> next() final { return iter::next(static_cast<I&>(*this)); }
            std::size_t size() const final {
                return iter::unsafe::size(static_cast<I const&>(*this));
            }
            unsafe::get_t<I> get(std::size_t index) final {
                return iter::unsafe::get(static_cast<I&>(*this), index);
            }
        };

        struct alignas(char) deleter {
            char const heap = 1;
            template<class T>
            void operator()(T* ptr) const {
                if (heap) delete ptr;
                else ptr->~T();
            }
        };
    }

    template<std::size_t Size, std::size_t Align = 8>
    struct scratch : void_t {
        template<class T, class... Ts>
        requires (sizeof(T) <= Size) && (alignof(T) <= Align) && (Align % alignof(T) == 0)
        T* make(Ts&&... ins) { return std::launder(new (std::addressof(storage)) T(FWD(ins)...)); }
    private:
        std::aligned_storage_t<Size, Align> storage;
    };

    template<concepts::next Next, class Get = void>
    struct boxed {
        using this_t = boxed;
        static constexpr bool random_access = !std::same_as<Get, void>;

        template<iter I>
        requires std::same_as<Next, next_t<I>>
             && (!random_access || std::same_as<Get, unsafe::get_t<I>>)
        constexpr boxed(I&& to_box)
            : it{new detail::virtual_iter_impl<std::remove_cvref_t<I>>(FWD(to_box))}
        {}

        template<iter I, std::size_t Size, std::size_t Align>
        requires std::same_as<Next, next_t<I>>
             && (!random_access || std::same_as<Get, unsafe::get_t<I>>)
        constexpr boxed(I&& to_box, scratch<Size, Align>& scratch)
            : it{scratch.template make<detail::virtual_iter_impl<std::remove_cvref_t<I>>>(FWD(to_box)), {0}}
        {}

        template<class OU> requires (!random_access)
        constexpr boxed(boxed<Next, OU>&& other) : it{std::move(other.it)} {}

    private:
        template<concepts::next, class> friend struct boxed;
        constexpr Next ITER_IMPL_THIS(next) (this_t& self) { return self.it->next(); }
        constexpr std::size_t ITER_UNSAFE_SIZE (this_t const& self) requires random_access {
            return self.it->size();
        }
        constexpr Get ITER_UNSAFE_GET (this_t& self, std::size_t index) requires random_access {
            return self.it->get(index);
        }

        std::unique_ptr<virtual_iter<Next, Get>, detail::deleter> it;
    };

    template<iter::iter I>
    boxed(I) -> boxed<next_t<I>, unsafe::get_t<I>>;
    template<iter::iter I, std::size_t Size, std::size_t Align>
    boxed(I, scratch<Size, Align>&) -> boxed<next_t<I>, unsafe::get_t<I>>;

    template<iter I>
    using virtual_t = iter::virtual_iter<iter::next_t<I>, iter::unsafe::get_t<I>>;
    template<iter I>
    using boxed_t = boxed<next_t<I>, unsafe::get_t<I>>;
}

template<iter::assert_iter I>
constexpr auto ITER_IMPL(box) (I&& iter) {
    return iter::boxed(FWD(iter));
}

template<iter::assert_iter I, std::size_t Size, std::size_t Align>
constexpr auto ITER_IMPL(box) (I&& iter, iter::scratch<Size, Align>& scratch) {
    return iter::boxed(FWD(iter), scratch);
}

#endif /* INCLUDE_ITER_BOX_HPP */

#ifndef ITER_CORE_RELOCATION_HPP
#define ITER_CORE_RELOCATION_HPP

// Disable relocation (copying/moving) unless it is completely elided
// or executed in a constexpr context
namespace iter {
    namespace detail {
        template<class>
        struct non_copiable;
        template<class>
        struct non_movable;
        template<class>
        struct non_relocatable;
    }
    namespace tag {
        static constexpr struct non_copiable_t {
            template<class T>
            using type = detail::non_copiable<T>;
        } non_copiable;
        static constexpr struct non_movable_t {
            template<class T>
            using type = detail::non_movable<T>;
        } non_movable;
        static constexpr struct non_relocatable_t {
            template<class T>
            using type = detail::non_relocatable<T>;
        } non_relocatable;
        static constexpr struct relocatable_t {
            template<class T>
            using type = void_t;
        } relocatable;
        namespace detail {
            template<class T>
            static constexpr bool is_relocation = false;
            template<>
            inline constexpr bool is_relocation<tag::non_copiable_t> = true;
            template<>
            inline constexpr bool is_relocation<tag::non_movable_t> = true;
            template<>
            inline constexpr bool is_relocation<tag::non_relocatable_t> = true;
            template<>
            inline constexpr bool is_relocation<tag::relocatable_t> = true;
        }
        namespace concepts {
            template<class T>
            concept relocation = detail::is_relocation<std::remove_cvref_t<T>>;
        }
    }
    namespace detail {
        template<class>
        struct non_copiable {
            constexpr non_copiable(tag::non_copiable_t){}
            non_copiable() = default;
            constexpr non_copiable(non_copiable const&) { detail::assert_consteval<non_copiable>(); }
            non_copiable(non_copiable&&) = default;
        };
        template<class>
        struct non_movable {
            constexpr non_movable(tag::non_movable_t){}
            non_movable() = default;
            non_movable(non_movable const&) = default;
            constexpr non_movable(non_movable&&) { detail::assert_consteval<non_movable>(); }
        };
        template<class T>
        struct non_relocatable : non_copiable<T>, non_movable<T> {
            constexpr non_relocatable(tag::non_relocatable_t){}
            non_relocatable() = default;
        };
    }
}

#endif /* ITER_CORE_RELOCATION_HPP */

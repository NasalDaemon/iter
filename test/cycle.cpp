#include "test.hpp"

#include <vector>

TEST(TestCycle, indices_take) {
    constexpr auto c = indices |take| 5 | cycle();
    using type = std::remove_cvref_t<decltype(c)>;
    static_assert(concepts::random_access_iter<type>);
    static_assert(unsafe::size(c) == std::numeric_limits<std::size_t>::max());
    auto c1 = c;
    ASSERT_EQ(c1 |nth| 0, 0);
    ASSERT_EQ(c1 |nth| 1, 1);
    ASSERT_EQ(c1 |nth| 2, 2);
    ASSERT_EQ(c1 |nth| 3, 3);
    ASSERT_EQ(c1 |nth| 4, 4);
    ASSERT_EQ(c1 |nth| 5, 0);
    ASSERT_EQ(c1 |nth| 6, 1);
    ASSERT_EQ(c1 |nth| 7, 2);
    ASSERT_EQ(c1 |nth| 8, 3);
    ASSERT_EQ(c1 |nth| 9, 4);
    ASSERT_EQ(c1 |nth| 10, 0);
    for (int i = 0; i < 10; ++i) {
        ASSERT_EQ(next(c1), i % 5);
    }
}

TEST(TestCycle, once) {
    constexpr auto o = once(9) | cycle();
    using type = std::remove_cvref_t<decltype(o)>;
    static_assert(concepts::random_access_iter<type>);
    static_assert(unsafe::size(o) == std::numeric_limits<std::size_t>::max());
    auto o1 = o;
    ASSERT_EQ(*nth(o1, 0), 9);
    ASSERT_EQ(*nth(o1, 1), 9);
    ASSERT_EQ(*nth(o1, 999), 9);
    for (int i = 0; i < 10; ++i)
        ASSERT_EQ(*next(o1), 9);
}

TEST(TestCycle, once_ref) {
    auto expected = 9;
    auto o = once_ref(expected) | cycle();
    using type = std::remove_cvref_t<decltype(o)>;
    static_assert(concepts::random_access_iter<type>);
    static_assert(unsafe::size(o) == std::numeric_limits<std::size_t>::max());
    auto o1 = o;
    ASSERT_EQ(*nth(o1, 0), expected);
    ASSERT_EQ(*nth(o1, 1), expected);
    ASSERT_EQ(*nth(o1, 999), expected);
    for (int i = 0; i < 10; ++i)
        ASSERT_EQ(*next(o1), expected);
}

template<class T>
void test_container(T&& v) {
    using type = std::remove_cvref_t<T>;
    static_assert(concepts::random_access_iter<type>);
    ASSERT_EQ(unsafe::size(v), std::numeric_limits<std::size_t>::max());

    ASSERT_EQ(*nth(v, 0), 0);
    ASSERT_EQ(*nth(v, 1), 3);
    ASSERT_EQ(*nth(v, 2), 2);
    ASSERT_EQ(*nth(v, 3), 6);
    for (int i = 0; i < 16; ++i) {
        auto n = *next(v);
        switch (i % 4) {
        case 0: ASSERT_EQ(n, 0); break;
        case 1: ASSERT_EQ(n, 3); break;
        case 2: ASSERT_EQ(n, 2); break;
        case 3: ASSERT_EQ(n, 6); break;
        }
    }
}

TEST(TestCycle, container) {
    test_container(std::vector{0, 3, 2, 6} | cycle());
    test_container(std::vector{0, 3, 2, 6} | to_iter() | cycle());

    std::vector v{0, 3, 2, 6};
    test_container(v | cycle());
    test_container(v | to_iter() | cycle());
}

TEST(TestCycle, rvo) {
    auto gen = generate {
        [i = 0]() mutable {
            return MAKE_OPTIONAL(ctor_count(i++));
        }
    };
    auto s = gen
        | take | 5
        | cycle()
        | take | 10
        | inspect | [](auto& c) {
            ASSERT_TRUE(c.value < 5);
            ASSERT_EQ(c.total(), 0); }
        | map | counter_unwrap
        | sum();

    ASSERT_EQ(s, 20);
}

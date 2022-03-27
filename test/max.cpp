#include "test.hpp"

TEST(TestMax, optional) {
    int iterations = 0;
    auto arr = std::array{9, -8, 1, 25, 7, 0, -25};
    auto m = arr
        | map | [](int i) { return i < 0 ? ctor_count{ctor_count{-i}} : ctor_count{i}; }
        | inspect | [&](auto&&) { ++iterations; }
        | max();
    ASSERT_EQ(m->value, 25);
    NO_CLANG(ASSERT_EQ(m->moves, 1));
    ASSERT_EQ(m->copies, 0);
    ASSERT_EQ(iterations, arr.size());
}

TEST(TestMax, optional_by) {
    int iterations = 0;
    auto arr = std::array{9, -8, 1, 25, 7, 0, 25};
    auto m = arr
        | map | [](int i) { return i < 0 ? ctor_count{ctor_count{-i}} : ctor_count{i}; }
        | inspect | [&](auto&&) { ++iterations; }
        | max_by | counter_unwrap;
    ASSERT_EQ(m->value, 25);
    NO_CLANG(ASSERT_EQ(m->moves, 1));
    ASSERT_EQ(m->copies, 0);
    ASSERT_EQ(iterations, arr.size());
}

TEST(TestMax, optional_empty) {
    auto m = empty<int>
        | map | counter_wrap
        | max();
    ASSERT_FALSE(m.has_value());
}

TEST(TestMax, optional_empty_by) {
    auto m = empty<int>
        | map | counter_wrap
        | max_by | counter_unwrap;
    ASSERT_FALSE(m.has_value());
}

TEST(TestMax, pointer) {
    int iterations = 0;
    auto arr = std::array<ctor_count<int>, 7>{9, -8, 1, 25, 7, 0, ctor_count{ctor_count{25}}};
    auto m = arr
        | inspect | [&](auto&&) { ++iterations; }
        | max();
    ASSERT_EQ(m->value, 25);
    NO_CLANG(ASSERT_EQ(m->moves, 0));
    ASSERT_EQ(m->copies, 0);
    ASSERT_EQ(iterations, arr.size());
}

TEST(TestMax, pointer_by) {
    int iterations = 0;
    auto arr = std::array<ctor_count<int>, 7>{9, -8, 1, 25, 7, 0, ctor_count{ctor_count{25}}};
    auto m = arr
        | inspect | [&](auto&&) { ++iterations; }
        | max_by | counter_unwrap;
    ASSERT_EQ(m->value, 25);
    NO_CLANG(ASSERT_EQ(m->moves, 0));
    ASSERT_EQ(m->copies, 0);
    ASSERT_EQ(iterations, arr.size());
}

TEST(TestMax, pointer_empty) {
    auto m = empty<ctor_count<int>> | max();
    ASSERT_FALSE(m.has_value());
}

TEST(TestMax, pointer_empty_by) {
    auto m = empty<ctor_count<int>> | max_by | counter_unwrap;
    ASSERT_FALSE(m.has_value());
}

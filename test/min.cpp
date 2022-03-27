#include "test.hpp"

TEST(TestMin, optional) {
    int iterations = 0;
    auto arr = std::array{9, -8, 10, 5, 7, 20, -5};
    auto m = arr
        | map | [](int i) { return i < 0 ? ctor_count{ctor_count{-i}} : ctor_count{i}; }
        | inspect | [&](auto&&) { ++iterations; }
        | min();
    ASSERT_EQ(m->value, 5);
    NO_CLANG(ASSERT_EQ(m->moves, 1));
    ASSERT_EQ(m->copies, 0);
    ASSERT_EQ(iterations, arr.size());
}

TEST(TestMin, optional_by) {
    int iterations = 0;
    auto arr = std::array{9, -8, 10, 5, 7, 20, -5};
    auto m = arr
        | map | [](int i) { return i < 0 ? ctor_count{ctor_count{-i}} : ctor_count{i}; }
        | inspect | [&](auto&&) { ++iterations; }
        | min_by | counter_unwrap;
    ASSERT_TRUE(m.has_value());
    ASSERT_EQ(m->value, 5);
    NO_CLANG(ASSERT_EQ(m->moves, 1));
    ASSERT_EQ(m->copies, 0);
    ASSERT_EQ(iterations, arr.size());
}

TEST(TestMin, optional_empty) {
    auto m = empty<int>
        | map | counter_wrap
        | min();
    ASSERT_FALSE(m.has_value());
}

TEST(TestMin, optional_empty_by) {
    auto m = empty<int>
        | map | counter_wrap
        | min_by | counter_unwrap;
    ASSERT_FALSE(m.has_value());
}

TEST(TestMin, pointer) {
    int iterations = 0;
    auto arr = std::array<ctor_count<int>, 7>{9, 8, 10, 5, 7, 20, ctor_count{ctor_count{5}}};
    auto m = arr
        | inspect | [&](auto&&) { ++iterations; }
        | min();
    ASSERT_EQ(m->value, 5);
    NO_CLANG(ASSERT_EQ(m->moves, 0));
    ASSERT_EQ(m->copies, 0);
    ASSERT_EQ(iterations, arr.size());
}

TEST(TestMin, pointer_by) {
    int iterations = 0;
    auto arr = std::array<ctor_count<int>, 7>{9, 8, 10, 5, 7, 20, ctor_count{ctor_count{5}}};
    auto m = arr
        | inspect | [&](auto&&) { ++iterations; }
        | min_by | counter_unwrap;
    ASSERT_EQ(m->value, 5);
    NO_CLANG(ASSERT_EQ(m->moves, 0));
    ASSERT_EQ(m->copies, 0);
    ASSERT_EQ(iterations, arr.size());
}

TEST(TestMin, pointer_empty) {
    auto m = empty<ctor_count<int>> | min();
    ASSERT_FALSE(m.has_value());
}

TEST(TestMin, pointer_empty_by) {
    auto m = empty<ctor_count<int>> | min_by | counter_unwrap;
    ASSERT_FALSE(m.has_value());
}

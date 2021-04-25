#include "test.hpp"

#include <vector>

TEST(TestCollect, vector_optional_move) {
    std::vector<ctor_count<int>> vec1 = 0 |til| 10 |map| counter_wrap | collect<>();
    std::vector<ctor_count<int>> vec2 = 0 |til| 10 |map| counter_wrap | to_vector();

    zip(vec1, vec2) | enumerate() | foreach |
        xtd::apply([](auto& i1, auto& i2, auto index) {
            ASSERT_EQ(i1.value, i2.value);
            ASSERT_EQ(i1.value, index);
            ASSERT_EQ(i1.copies, 0);
            ASSERT_EQ(i2.copies, 0);
            ASSERT_EQ(i1.moves, 1);
            ASSERT_EQ(i2.moves, 1);
        });
}

TEST(TestCollect, vector_pointer_copy) {
    std::vector<ctor_count<int>> vec1 = 0 |til| 10 |map| counter_wrap | to_pointer_iter() | collect<>(_, 10);
    std::vector<ctor_count<int>> vec2 = 0 |til| 10 |map| counter_wrap | to_pointer_iter() | to_vector(_, 10);

    zip(vec1, vec2) | enumerate() | foreach |
        xtd::apply([](auto& i1, auto& i2, auto index) {
            ASSERT_EQ(i1.value, i2.value);
            ASSERT_EQ(i1.value, index);
            ASSERT_EQ(i1.copies, 1);
            ASSERT_EQ(i2.copies, 1);
            ASSERT_EQ(i1.moves, 0);
            ASSERT_EQ(i2.moves, 0);
        });
}

TEST(TestCollect, vector_pointer_move) {
    std::vector<ctor_count<int>> vec1 = 0 |til| 10 |map| counter_wrap | to_pointer_iter() | move() | collect<>(_, 10);
    std::vector<ctor_count<int>> vec2 = 0 |til| 10 |map| counter_wrap | to_pointer_iter() | move() | to_vector(_, 10);

    zip(vec1, vec2) | enumerate() | foreach |
        xtd::apply([](auto& i1, auto& i2, auto index) {
            ASSERT_EQ(i1.value, i2.value);
            ASSERT_EQ(i1.value, index);
            ASSERT_EQ(i1.copies, 0);
            ASSERT_EQ(i2.copies, 0);
            ASSERT_EQ(i1.moves, 1);
            ASSERT_EQ(i2.moves, 1);
        });
}

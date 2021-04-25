#include "test.hpp"

TEST(FilterTest, 1) {
    auto s = indices
        | map | counter_wrap
        | filter | [](auto& c) {
            return c.value > 5; }
        | inspect | [](auto& c) {
            ASSERT_EQ(c.copies, 0);
            ASSERT_EQ(c.moves, 0); // from creating an optional
            ASSERT_TRUE(c.value > 5); }
        | map | counter_unwrap
        | take | 5
        | sum();
    ASSERT_EQ(s, 6 + 7 + 8 + 9 + 10);
}

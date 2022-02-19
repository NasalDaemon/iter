#include "test.hpp"

TEST(TestPartition, oddEven) {
    auto [even, odd] = range{0, 10} | partition | [](auto i) { return i % 2 == 0; };
    ASSERT_EQ(even, (std::vector{0, 2, 4, 6, 8}));
    ASSERT_EQ(odd, (std::vector{1, 3, 5, 7, 9}));
}

TEST(TestPartition, mod3) {
    auto [zero, one, two] = range{0, 10} | partition | [](auto i) { return maximum<2>::values[i % 3]; };
    ASSERT_EQ(zero, (std::vector{0, 3, 6, 9}));
    ASSERT_EQ(one, (std::vector{1, 4, 7}));
    ASSERT_EQ(two, (std::vector{2, 5, 8}));
}

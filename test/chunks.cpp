#include "test.hpp"

TEST(TestChunks, dynamic) {
    auto s = range(0, 10)
        | chunks | 2
        | flatten()
        | sum();

    ASSERT_EQ(s, 45);
}

TEST(TestChunks, static) {
    auto s = range(0, 10)
        | chunks_<2>()
        | flatten()
        | sum();

    ASSERT_EQ(s, 45);
}

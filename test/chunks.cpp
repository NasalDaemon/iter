#include "test.hpp"

TEST(TestChunks, 1) {
    auto s = range(0, 10)
        | chunks | 2
        | flatten()
        | sum();

    ASSERT_EQ(s, 45);
}
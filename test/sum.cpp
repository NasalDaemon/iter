#include "test.hpp"

TEST(TestSum, int) {
    auto s = range{0, 10} | sum();
    ASSERT_EQ(s, 45);
}

TEST(TestSum, float) {
    auto s = range{0, 10} |map| [](int i) { return float(i); } | sum();
    ASSERT_EQ(s, 45.f);
}

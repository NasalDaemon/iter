#include "test.hpp"

TEST(TestFold, sum) {
    auto fold_sum = range{0, 10} | fold(_, 0, [](auto acc, auto i) { return acc + i; });
    auto s = range{0, 10} | sum();
    ASSERT_EQ(fold_sum, s);
}

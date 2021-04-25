#include "test.hpp"

#include <vector>

TEST(TestAny, predicate) {
    auto a = std::vector{-1, -2, -3};
    ASSERT_TRUE(a |any| [](int i) { return i == -1; });
    ASSERT_TRUE(a |any| [](int i) { return i == -2; });
    ASSERT_TRUE(a |any| [](int i) { return i == -3; });
    ASSERT_FALSE(a |any| [](int i) { return i == 0; });
}

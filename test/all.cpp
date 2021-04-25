#include "test.hpp"

#include <vector>

TEST(TestAll, predicate) {
    auto a = std::vector{-1, -2, -3};
    ASSERT_TRUE(a |all| [](int i) { return i < 0; });
    ASSERT_FALSE(a |all| [](int i) { return i != -1; });
    ASSERT_FALSE(a |all| [](int i) { return i != -2; });
    ASSERT_FALSE(a |all| [](int i) { return i != -3; });
}

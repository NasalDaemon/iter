#include "test.hpp"

#include "iter/wrap.hpp"
#include <array>

TEST(TestWrap, 1) {
    auto s = wrap{std::array{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}}.map([](auto i) { return i; }).sum();
    ASSERT_EQ(s, 45);
}

#include "test.hpp"

#include "iter/wrap.hpp"
#include <array>
#include <vector>

TEST(TestWrap, simple) {
    const auto arr = std::array{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    auto s = wrap{arr}
        .map([](auto i) { return i; })
        .sum();
    ASSERT_EQ(s, 45);
}

TEST(TestWrap, templated) {
    auto [evens, odds] = wrap{std::array{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}}
        .partition_<>([](auto i) { return i % 2 == 0;});
    ASSERT_EQ(evens, (std::vector{0, 2, 4, 6, 8}));
    ASSERT_EQ(odds, (std::vector{1, 3, 5, 7, 9}));
}

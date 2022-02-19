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
    auto arr = std::array{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    auto sum = wrap{arr}
        .enumerate_map_<>([](auto a, int i) { return a + i*0; })
        .sum();
    ASSERT_EQ(sum, 45);
}

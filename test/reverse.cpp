#include "test.hpp"

TEST(TestReverse, array) {
    std::array array{0, 1, 2, 3};
    auto s = array
        | reverse()
        | zip | array
        | inspect | xtd::apply([](auto r, auto a) {
            ASSERT_EQ(r, 3 - a); })
        | map | xtd::apply([](auto r, auto a) {
            return r + a; })
        | inspect | [](auto s) {
            ASSERT_EQ(s, 3); }
        | sum();
    ASSERT_EQ(s, 12);
}

TEST(TestReverse, array_filter) {
    std::array array{0, 1, 2, 3};
    auto s = array
        | reverse()
        | filter | [](auto&&) { return true; }
        | zip | array
        | map | xtd::apply([](auto r, auto a) {
            return r + a; })
        | inspect | [](auto s) {
            ASSERT_EQ(s, 3); }
        | sum();
    ASSERT_EQ(s, 12);
}

TEST(TestReverse, exclusive_range) {
    auto result = range{2, 5}
        | reverse()
        | to_vector();
    auto expected = std::vector{4, 3, 2};
    ASSERT_EQ(expected, result);
}

TEST(TestReverse, inclusive_range) {
    auto result = inclusive_range{2, 5}
        | reverse()
        | to_vector();
    auto expected = std::vector{5, 4, 3, 2};
    ASSERT_EQ(expected, result);
}

TEST(TestReverse, indices) {
    constexpr auto max = std::numeric_limits<std::size_t>::max() - 1;
    auto result = indices | reverse() | take | 4 | to_vector();
    auto expected = std::vector{max, max-1, max-2, max-3};
    ASSERT_EQ(expected, result);
}

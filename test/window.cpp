#include "test.hpp"

TEST(TestWindow, twos) {
    auto it = range{0, 10} | iter::window<2>();

    ASSERT_TRUE(*next(it) == to_iter(std::array{0, 1}));
    ASSERT_TRUE(*next(it) == to_iter(std::array{1, 2}));
    ASSERT_TRUE(*next(it) == to_iter(std::array{2, 3}));
    ASSERT_TRUE(*next(it) == to_iter(std::array{3, 4}));
    ASSERT_TRUE(*next(it) == to_iter(std::array{4, 5}));
    ASSERT_TRUE(*next(it) == to_iter(std::array{5, 6}));
    ASSERT_TRUE(*next(it) == to_iter(std::array{6, 7}));
    ASSERT_TRUE(*next(it) == to_iter(std::array{7, 8}));
    ASSERT_TRUE(*next(it) == to_iter(std::array{8, 9}));
    ASSERT_FALSE(next(it).has_value());
}

TEST(TestWindow, fives) {
    auto it = range{0, 10} | iter::window<5>();

    ASSERT_TRUE(*next(it) == to_iter(std::array{0, 1, 2, 3, 4}));
    ASSERT_TRUE(*next(it) == to_iter(std::array{1, 2, 3, 4, 5}));
    ASSERT_TRUE(*next(it) == to_iter(std::array{2, 3, 4, 5, 6}));
    ASSERT_TRUE(*next(it) == to_iter(std::array{3, 4, 5, 6, 7}));
    ASSERT_TRUE(*next(it) == to_iter(std::array{4, 5, 6, 7, 8}));
    ASSERT_TRUE(*next(it) == to_iter(std::array{5, 6, 7, 8, 9}));
    ASSERT_FALSE(next(it).has_value());
}

TEST(TestWindow, too_small) {
    auto it = range{0, 10} | iter::window<11>();
    ASSERT_FALSE(next(it).has_value());
}

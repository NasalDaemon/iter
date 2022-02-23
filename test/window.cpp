#include "test.hpp"

TEST(TestWindow, twos) {
    auto it = range{0, 10} | iter::window<2>();

                            auto a = std::array{0, 1};
    ASSERT_TRUE(*impl::next(it) == to_iter(a));
    ASSERT_TRUE(*impl::next(it) == to_iter(a = {1, 2}));
    ASSERT_TRUE(*impl::next(it) == to_iter(a = {2, 3}));
    ASSERT_TRUE(*impl::next(it) == to_iter(a = {3, 4}));
    ASSERT_TRUE(*impl::next(it) == to_iter(a = {4, 5}));
    ASSERT_TRUE(*impl::next(it) == to_iter(a = {5, 6}));
    ASSERT_TRUE(*impl::next(it) == to_iter(a = {6, 7}));
    ASSERT_TRUE(*impl::next(it) == to_iter(a = {7, 8}));
    ASSERT_TRUE(*impl::next(it) == to_iter(a = {8, 9}));
    ASSERT_FALSE(impl::next(it).has_value());
}

TEST(TestWindow, fives) {
    auto it = range{0, 10} | iter::window<5>();
                            auto a = std::array{0, 1, 2, 3, 4};
    ASSERT_TRUE(*impl::next(it) == to_iter(a));
    ASSERT_TRUE(*impl::next(it) == to_iter(a = {1, 2, 3, 4, 5}));
    ASSERT_TRUE(*impl::next(it) == to_iter(a = {2, 3, 4, 5, 6}));
    ASSERT_TRUE(*impl::next(it) == to_iter(a = {3, 4, 5, 6, 7}));
    ASSERT_TRUE(*impl::next(it) == to_iter(a = {4, 5, 6, 7, 8}));
    ASSERT_TRUE(*impl::next(it) == to_iter(a = {5, 6, 7, 8, 9}));
    ASSERT_FALSE(impl::next(it).has_value());
}

TEST(TestWindow, too_small) {
    auto it = range{0, 10} | iter::window<11>();
    ASSERT_FALSE(impl::next(it).has_value());
}

#include "test.hpp"

static_assert(concepts::random_access_iter<decltype(range())>);

TEST(TestRange, last) {
    auto r = range{0, 10};
    ASSERT_EQ(r | last(), 9);
    ASSERT_EQ(r | impl::next(), 0);
}

TEST(TestRange, default) {
    auto r = range();
    ASSERT_EQ(*(r | last()), std::numeric_limits<int>::max() - 1);
    ASSERT_EQ(r | impl::next(), 0);
}

TEST(TestRange, fuse) {
    auto r = range{0, 10};
    while(impl::next(r));
    ASSERT_FALSE(impl::next(r).has_value());
    ASSERT_FALSE(impl::next(r).has_value());
}

TEST(TestRange, empty) {
    auto r = range{0, 0};
    ASSERT_FALSE(impl::next(r).has_value());
    ASSERT_FALSE(impl::next(r).has_value());
}

static_assert(concepts::random_access_iterable<decltype(indices)>);

TEST(TestRange, indices) {
    auto r = indices | to_iter();
    ASSERT_EQ(r | impl::next(), 0);
    ASSERT_EQ(r | last(), std::numeric_limits<std::size_t>::max() - 1);
}

TEST(TestRange, inclusive) {
    auto r = inclusive_range{9, 10};
    ASSERT_EQ(r | impl::next(), 9);
    ASSERT_EQ(r | impl::next(), 10);
    ASSERT_FALSE(impl::next(r).has_value());
    ASSERT_FALSE(impl::next(r).has_value());
}

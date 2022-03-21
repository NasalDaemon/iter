#include "test.hpp"

TEST(TestLast, empty) {
    ASSERT_EQ(noitem, empty<int> | last());
    std::vector<int> v;
    ASSERT_EQ(noitem, v | last());
    ASSERT_EQ(noitem, indices | take | 0 | last());
}

TEST(TestLast, empty_fallback) {
    ASSERT_EQ(-1, empty<int> | last | -1);
    std::vector<int> v;
    ASSERT_EQ(-1, v | last | -1);
    ASSERT_EQ(-1, indices | take | 0 | last | -1);
}

TEST(TestLast, random_pointer) {
    auto array = std::array<int, 4>{9, 32, 3, 7};
    int iteration_count = 0;
    auto it = array
        | map | counter_wrap
        | inspect | [&](auto&) { ++iteration_count; }
        | to_pointer_iter();
    static_assert(concepts::random_access_iter<decltype(it)>);
    static_assert(!concepts::owned_item<next_t<decltype(it)>>);

    auto l1 = it | last();
    static_assert(!concepts::owned_item<decltype(l1)>);
    ASSERT_EQ(7, l1->value);
    ASSERT_EQ(1, iteration_count);

    iteration_count = 0;
    auto l2 = std::move(it) | last();
    static_assert(concepts::owned_item<decltype(l2)>);
    ASSERT_EQ(7, l2->value);
    ASSERT_EQ(1, iteration_count);
}

TEST(TestLast, random_pointer_fallback) {
    auto array = std::array<int, 4>{9, 32, 3, 7};
    int iteration_count = 0;
    auto it = array
        | map | counter_wrap
        | inspect | [&](auto&) { ++iteration_count; }
        | to_pointer_iter();
    static_assert(concepts::random_access_iter<decltype(it)>);
    static_assert(!concepts::owned_item<next_t<decltype(it)>>);
    auto l1 = it | last | -1;
    NO_CLANG(ASSERT_EQ(0, l1.moves));
    ASSERT_EQ(1, l1.copies);
    ASSERT_EQ(7, l1.value);
    ASSERT_EQ(1, iteration_count);

    iteration_count = 0;
    auto l2 = it | take | 0 | last | -1;
    ASSERT_EQ(0, l2.total());
    ASSERT_EQ(-1, l2.value);
    ASSERT_EQ(0, iteration_count);
}

TEST(TestLast, random_optional) {
    int iteration_count = 0;
    auto l = indices
        | take | 10
        | map | counter_wrap
        | inspect | [&](auto& c) {
            ++iteration_count;
            ASSERT_EQ(c.total(), 0); }
        | last();

    ASSERT_TRUE(l.has_value());
    ASSERT_EQ(9, l->value);
    ASSERT_EQ(0, l->copies);
    NO_CLANG(ASSERT_EQ(0, l->moves));
    ASSERT_EQ(1, iteration_count);
}

TEST(TestLast, random_optional_fallback) {
    int iteration_count = 0;
    auto it = indices
        | map | counter_wrap
        | inspect | [&](auto& c) {
            ++iteration_count;
            ASSERT_EQ(c.total(), 0); };
    auto l1 = it | take | 0 | last | -1;

    ASSERT_EQ(-1, l1);
    ASSERT_EQ(0, l1.total());
    ASSERT_EQ(0, iteration_count);

    iteration_count = 0;
    auto l2 = it | take | 10 | last | -1;

    ASSERT_EQ(9, l2.value);
    ASSERT_EQ(0, l2.copies);
    ASSERT_EQ(1, l2.moves);
    ASSERT_EQ(1, iteration_count);
}

TEST(TestLast, optional) {
    int iteration_count = 0;
    auto it = indices
        | take | 10
        | filter | [](auto&&) { return true; }
        | map | counter_wrap
        | inspect | [&](auto& c) {
            ++iteration_count;
            ASSERT_EQ(c.total(), 0); };
    static_assert(!concepts::random_access_iter<decltype(it)>);
    static_assert(concepts::owned_item<next_t<decltype(it)>>);
    auto l = it | last();

    ASSERT_TRUE(l.has_value());
    ASSERT_EQ(9, l->value);
    ASSERT_EQ(0, l->copies);
    NO_CLANG(ASSERT_EQ(1, l->moves));
    ASSERT_EQ(10, iteration_count);
}

TEST(TestLast, optional_fallback) {
    int iteration_count = 0;
    auto it = indices
        | map | counter_wrap
        | filter | [](auto&&) { return true; }
        | inspect | [&](auto& c) {
            ++iteration_count;
            ASSERT_EQ(c.total(), 0); };
    static_assert(!concepts::random_access_iter<decltype(it)>);
    static_assert(concepts::owned_item<next_t<decltype(it)>>);
    auto l1 = it | take | 0 | last | -1;

    ASSERT_EQ(-1, l1.value);
    ASSERT_EQ(0, l1.copies);
    ASSERT_EQ(0, l1.moves);
    ASSERT_EQ(0, iteration_count);

    iteration_count = 0;
    auto l2 = it | take | 10 | last | -1;

    ASSERT_EQ(9, l2.value);
    ASSERT_EQ(0, l2.copies);
    ASSERT_EQ(1, l2.moves);
    ASSERT_EQ(10, iteration_count);
}

TEST(TestLast, pointer) {
    auto array = std::array<int, 4>{9, 32, 3, 7};
    int iteration_count = 0;
    auto it = array
        | map | counter_wrap
        | filter | [](auto&&) { return true; }
        | inspect | [&](auto&) { ++iteration_count; }
        | to_pointer_iter()
        | inspect | [](auto& wrapper) {
            ASSERT_EQ(0, wrapper.copies);
            NO_CLANG(ASSERT_EQ(0, wrapper.moves));
            };
    static_assert(!concepts::random_access_iter<decltype(it)>);
    static_assert(!concepts::owned_item<next_t<decltype(it)>>);

    auto l = it | last();
    static_assert(concepts::owned_item<decltype(l)>);
    ASSERT_EQ(7, l->value);
    ASSERT_EQ(1, l->copies);
    NO_CLANG(ASSERT_EQ(0, l->moves));
    ASSERT_EQ(4, iteration_count);
}

TEST(TestLast, pointer_fallback) {
    auto array = std::array<int, 4>{9, 32, 3, 7};
    int iteration_count = 0;
    auto it = array
        | map | counter_wrap
        | filter | [](auto&&) { return true; }
        | inspect | [&](auto&) { ++iteration_count; }
        | to_pointer_iter();
    static_assert(!concepts::random_access_iter<decltype(it)>);
    static_assert(!concepts::owned_item<next_t<decltype(it)>>);
    auto l1 = it | last | -1;
    NO_CLANG(ASSERT_EQ(0, l1.moves));
    ASSERT_EQ(1, l1.copies);
    ASSERT_EQ(7, l1.value);
    ASSERT_EQ(4, iteration_count);

    iteration_count = 0;
    auto l2 = it | take | 0 | last | -1;
    NO_CLANG(ASSERT_EQ(0, l2.moves));
    ASSERT_EQ(0, l2.copies);
    ASSERT_EQ(-1, l2.value);
    ASSERT_EQ(0, iteration_count);
}

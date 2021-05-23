#include "test.hpp"

TEST(TestMove, random_pointer) {
    auto arr = std::array<ctor_count<int>, 4>{0, 4, 6, 28};
    auto it1 = arr | to_iter();
    auto it2 = it1 | move();
    static_assert(concepts::random_access_iter<decltype(it1)>);
    static_assert(concepts::random_access_iter<decltype(it2)>);
    auto vec1 = it1 | to_vector();
    auto vec2 = it2 | to_vector();
    zip(vec1, vec2) | foreach | xtd::apply([](auto& c1, auto& c2) {
        ASSERT_EQ(c1.copies, 1);
        ASSERT_EQ(c2.copies, 0);
        ASSERT_EQ(c1.moves, 0);
        ASSERT_EQ(c2.moves, 1);
    });
}

TEST(TestMove, non_random_pointer) {
    auto arr = std::array<ctor_count<int>, 4>{0, 4, 6, 28};
    auto it1 = arr | filter | [](auto&&){ return true; };
    auto it2 = it1 | move();
    static_assert(!concepts::random_access_iter<decltype(it1)>);
    static_assert(!concepts::random_access_iter<decltype(it2)>);
    auto vec1 = it1 | to_vector | arr.size();
    auto vec2 = it2 | to_vector | arr.size();
    zip(vec1, vec2) | foreach | xtd::apply([](auto& c1, auto& c2) {
        ASSERT_EQ(c1.copies, 1);
        ASSERT_EQ(c2.copies, 0);
        ASSERT_EQ(c1.moves, 0);
        ASSERT_EQ(c2.moves, 1);
    });
}

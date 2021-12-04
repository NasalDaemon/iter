#include "test.hpp"

#include <vector>

TEST(TestEnumerate, random_access) {
    auto v = std::vector{1, 2, 3};
    auto it = v | enumerate();
    static_assert(concepts::random_access_iter<decltype(it)>);

    it | foreach | xtd::apply([expected = 0](int, int i) mutable {
        ASSERT_EQ(i, expected++); });
}

TEST(TestEnumerate, not_random_access) {
    auto gen = generate {
        [expected = 0]() mutable {
            return std::optional(expected++);
        }
    };
    auto it = gen | enumerate();
    static_assert(!concepts::random_access_iter<decltype(it)>);

    it | take | 20 | foreach | xtd::apply([](int expected, int i) {
        ASSERT_EQ(i, expected); });
}

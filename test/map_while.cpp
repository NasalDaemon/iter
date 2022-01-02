#include "test.hpp"

TEST(MapWhileTest, 1) {
    int iteration_count = 0;
    auto it = range{0, 10}
        | map_while | [](auto i) {
            return i < 5 ? MAKE_ITEM(ctor_count{5 + i}) : noitem; }
        | inspect | [&, exp = 5](auto& c) mutable {
            ++iteration_count;
            ASSERT_EQ(c.value, exp++);
            ASSERT_EQ(c.total(), 0); };
    static_assert(concepts::owned_item<next_t<decltype(it)>>);
    it | foreach();
    ASSERT_EQ(iteration_count, 5);
}

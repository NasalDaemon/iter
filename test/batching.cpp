#include "test.hpp"

#include <vector>

TEST(TestBatching, predicate) {
    auto s = range{0, 10} 
        | batching | [array = std::array<int, 3>{}](auto it) mutable {
            std::size_t size = 0;
            for (auto i : iter::take(it, 3))
                array[size++] = i;
            return size ? iter::item{iter::unstable{iter::take(array, size)}} : iter::noitem; }
        | inspect | [expected = 0](auto it) mutable {
            std::size_t size = 0;
            for (auto i : it) {
                ASSERT_EQ(expected, i);
                expected++;
                size++;
            }
            ASSERT_GT(size, 0);
            ASSERT_LE(size, 3); }
        | flatten()
        | inspect | [expected = 0](auto i) mutable {
            ASSERT_EQ(expected, i);
            expected++; }
        | sum();

    ASSERT_EQ(s, 45);
}

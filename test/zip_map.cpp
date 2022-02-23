#include "test.hpp"

#include <vector>

TEST(TestZipMap, 1) {
    auto s = range{0, 5}
        | zip_map(_, range{5, 10}, [](auto l, auto r) { return l + r; })
        | sum();

    ASSERT_EQ(s, 45);
}

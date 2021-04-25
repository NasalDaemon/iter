#include "test.hpp"

static_assert(concepts::random_access_iterable<decltype(empty<int>)>);

TEST(EmptyTest, 1) {
    //TODO
}
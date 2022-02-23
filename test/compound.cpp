#include "test.hpp"

TEST(TestCompound, interest_5th) {
    auto c = compound {
        1.f,
        [](auto acc) {
            return acc * 1.01f;
        }
    };
    auto fifth = c |nth| 5;
    ASSERT_FLOAT_EQ(fifth ? *fifth : 0.f, 1.01f * 1.01f * 1.01f * 1.01f * 1.01f);
}

TEST(TestCompound, interest_last) {
    auto c = compound {
        1.f,
        [](float acc) {
            return acc < 2.f ? iter::item(acc * 1.01f) : iter::noitem;
        }
    };
    auto l = c | last();
    ASSERT_FLOAT_EQ(l ? *l : 0.f, 2.006762f);
}

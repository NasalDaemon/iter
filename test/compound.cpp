#include "test.hpp"

TEST(TestCompound, interest_5th) {
    auto c = compound {
        1.f,
        [](auto acc) {
            return acc * 1.01f;
        }
    };
    auto fifth = c |nth| 5;
    ASSERT_FLOAT_EQ(fifth.value_or(0.f), 1.01f * 1.01f * 1.01f * 1.01f * 1.01f);
}

TEST(TestCompound, interest_last) {
    auto c = compound {
        1.f,
        [](auto acc) {
            return acc < 2.f ? std::optional(acc * 1.01) : std::nullopt;
        }
    };
    auto l = c | last();
    ASSERT_FLOAT_EQ(l.value_or(0.f), 2.0067635f);
}

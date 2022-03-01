#include "test.hpp"

#include <vector>

TEST(TestFlatten, nested_vector) {
    std::vector<std::vector<ctor_count<int>>> v;
    v.reserve(3);
    auto& v1 = v.emplace_back();
    v1.reserve(3);
    v1.emplace_back(0);
    v1.emplace_back(1);
    v1.emplace_back(2);
    auto& v2 = v.emplace_back();
    v2.reserve(3);
    v2.emplace_back(3);
    v2.emplace_back(4);
    v2.emplace_back(5);
    auto& v3 = v.emplace_back();
    v3.reserve(4);
    v3.emplace_back(6);
    v3.emplace_back(7);
    v3.emplace_back(8);
    v3.emplace_back(9);

    auto s = v
        | flatten()
        | inspect | [i = 0](auto& c) mutable {
            ASSERT_EQ(c.value, i++);
            ASSERT_EQ(c.total(), 0); }
        | fold(_, 0, [](auto acc, auto& i) {
            return acc + i.value; });

    ASSERT_EQ(s, 45);
}

#ifdef ITER_COROUTINE

generator<ctor_count<int>> inner(int n) {
    while (true) {
        co_yield n++;
    }
}

generator<generator<ctor_count<int>>> outer() {
    co_yield inner(10);
}

TEST(TestFlatten, nested_generator) {
    auto s = outer()
        | flatten()
        | inspect | [i = 10](auto& c) mutable {
            ASSERT_EQ(c.value, i++);
            ASSERT_EQ(c.total(), 0); }
        | take | 10
        | fold(_, 0, [](auto acc, auto& i) {
            return acc + i.value; });

    ASSERT_EQ(s, 145);
}

#endif // ITER_COROUTINE

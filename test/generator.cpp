#include "test.hpp"
#ifdef ITER_COROUTINE

generator<int> range_gen() {
    int n = 0;
    while (true) {
        co_yield n++;
    }
}

static_assert(iterable<generator<>>);
static_assert(!concepts::random_access_iterable<generator<>>);

TEST(GeneratorTest, simple) {
    auto s = range_gen() |take| 10 |sum| _;
    ASSERT_EQ(s, 45);
}

TEST(GeneratorTest, range_for) {
    int s = 0;
    for (auto i : range_gen() |take| 10) {
        s += i;
    }
    ASSERT_EQ(s, 45);
}

generator<> append(std::vector<int>& vec) {
    int n = 0;
    while (true) {
        vec.push_back(n++);
        co_yield {};
    }
}

#include <numeric>

TEST(GeneratorTest, void_generator) {
    std::vector<int> v;
    append(v) |take| 10 |foreach| _;
    auto s = std::accumulate(v.begin(), v.end(), 0);
    ASSERT_EQ(s, 45);
}

generator<int> make_range_to(int limit) {
    if (limit != 10) throw "";
    for (int i = 0; i < limit; ++i)
        co_yield i;
}

generator<int> make_range_10() { return make_range_to(10); }

TEST(GeneratorTest, cycle) {
    ASSERT_EQ(45, make_range_10() | sum());
    ASSERT_EQ(45, make_range_to(10) | sum());
    ASSERT_EQ(90, make_range_10 |cycle|  _ |take| 20 | sum());
    ASSERT_EQ(90, make_range_to |cycle| 10 |take| 20 | sum());

    // Prove that args captured by value
    int i = 10;
    auto gen = make_range_to |cycle| i;
    i = 100;
    ASSERT_EQ(90, std::move(gen) |take| 20 | sum());
}

#endif // ITER_COROUTINE

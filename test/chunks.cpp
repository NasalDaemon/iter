#include "test.hpp"
#include "iter/macros/dollar/define.hpp"

TEST(TestChunks, dynamic) {
    auto s = range{0, 10}
        | chunks | 2
        | flatten()
        | sum();

    ASSERT_EQ(s, 45);
}

TEST(TestChunks, dynamic_no_inner_iteration) {
    std::size_t count = 0;
    range{0, 10}
        | chunks | 2
        | foreach | [&](auto&&) { ++count; };

    ASSERT_EQ(count, 5);
}

TEST(TestChunks, static) {
    auto s = range{0, 10}
        | chunks_<2>()
        | flatten()
        | sum();

    ASSERT_EQ(s, 45);
}

TEST(TestChunks, static_nontrivial) {
    auto a = std::array<std::string, 5>{"0", "12", "324", "", "02"};
    auto s = a
        | to_iter()
        | chunks_<2>()
        | flatten()
        | fold(_, 0, [](auto acc, auto& s) { return acc + s.length(); });

    ASSERT_EQ(s, 8);
}

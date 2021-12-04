#include "test.hpp"

#include "iter/wrap.hpp"

#include <vector>

TEST(TestFlatmap, 1) {
    auto v = std::vector{1, 2, 3};
    auto s = wrap(v)
        .flatmap([](int i) {
            return std::vector{i, 2*i}; }) // (1 2) (2 4) (3 6)
        .map([](int i) {
            return std::vector{i * 2}; }) // (2) (4) (4) (8) (6) (12)
        .filter([](auto){ return true; })
        .flatten()
        .sum();

    ASSERT_EQ(s, 36);
}

TEST(TestFlatMap, rvo) {
    auto s = indices
        | take | 10
        | flatmap | [](auto i) {
            std::vector<ctor_count<int>> v;
            v.reserve(2);
            v.emplace_back(2*i);
            v.emplace_back(2*i+1);
            return v; }
        | inspect | [](auto& c) {
            ASSERT_EQ(c.moves, 0);
            ASSERT_EQ(c.copies, 0); }
        | fold(_, 0, [](auto acc, auto& i) {
            return acc + i.value; });

    ASSERT_EQ(s, 190);
}

TEST(TestFlatmap, nested_flatmap) {
    wrap{range{0, 10}}
        .map(counter_wrap)
        .inspect([](auto& c1) {
            ASSERT_EQ(c1.total(), 0); })
        .flatmap([](auto&& c1) {
            return wrap{range{0, c1.value}}
                .map(counter_wrap)
                .inspect([](auto& c2) {
                    ASSERT_EQ(c2.total(), 0); })
                .flatmap([c1 = std::move(c1)](auto&& c2) {
                    using type = tuple<ctor_count<int>, ctor_count<int>>;
                    return once<type>{{c1, std::move(c2)}}; }); })
        .foreach(xtd::apply([](auto& c1, auto& c2) {
            ASSERT_LT(c2.value, c1.value);
            ASSERT_EQ(c1.copies, 1); // constructing tuple in once
            NO_CLANG(ASSERT_EQ(c1.moves, 2)); // capturing in lambda + moving lambda into flatmap
            ASSERT_EQ(c2.copies, 0);
            NO_CLANG(ASSERT_EQ(c2.moves, 1)); // constructing tuple in once
            }));
}

// Ensure that map | flatten translates to better optimised flatmap
constexpr auto mapper = [](auto i) { return std::vector{i}; };
using map_flatten_t = decltype(indices |map| mapper | flatten());
using flatmap_t = decltype(indices |flatmap| mapper);
static_assert(std::same_as<map_flatten_t, flatmap_t>);

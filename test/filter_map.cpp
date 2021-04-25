#include "test.hpp"

TEST(FilterMapTest, optional) {
    auto s = indices
        | filter_map | [](auto i) {
            return i % 2 == 0 ? MAKE_OPTIONAL(ctor_count(-i)) : std::nullopt; }
        | filter | [](auto&) {
            return true; } // extra no-op filter step shouldn't add any moves/copies
        | inspect | [](auto& i) {
            ASSERT_EQ(i.value % 2, 0);
            ASSERT_TRUE(i.value <= 0);
            ASSERT_EQ(i.total(), 0); }
        | take | 10
        | inspect | [](auto& i) {
            ASSERT_EQ(i.total(), 0); }
        | map | [](auto&& i) {
            return i.value; }
        | sum();
    ASSERT_EQ(-90, s);
}

TEST(FilterMapTest, pointer) {
    auto s = indices
        | take | 20
        | filter_map | [x = 0](auto i) mutable {
            x = -i;
            return i % 2 == 0 ? &x : nullptr; }
        | inspect | [](auto i) {
            ASSERT_TRUE(i % 2 == 0);
            ASSERT_TRUE(i <= 0); }
        | sum();
    ASSERT_EQ(-90, s);
}

// Ensure that flatmap on optional translates to better optimised filter_map
constexpr auto mk_opt = [](auto i) { return std::optional(i); };
using flatmap_option_t = decltype(indices |flatmap| mk_opt);
using filter_map_t = decltype(indices |filter_map| mk_opt);
static_assert(std::same_as<flatmap_option_t, filter_map_t>);

// Ensure this also holds for map | flatten -> flatmap -> filter_map
using map_flatten_option_t = decltype(indices |map| mk_opt |flatten| _);
static_assert(std::same_as<map_flatten_option_t, filter_map_t>);

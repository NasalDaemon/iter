#include "test.hpp"

TEST(TestFindLinear, not_found_optional) {
    auto r = indices
        | take | 100
        | map | [](auto i) { return 2*i; }
        | find_linear | [](auto i) { return i % 2 == 1; };
    ASSERT_EQ(noitem, r);
}

TEST(TestFindLinear, not_found_pointer_optional) {
    auto r = indices
        | take | 100
        | map | [](auto i) { return 2*i; }
        | to_pointer_iter()
        | find_linear | [](auto i) { return i % 2 == 1; };
    ASSERT_EQ(noitem, r);
}

TEST(TestFindLinear, not_found_pointer_pointer) {
    auto it = indices
        | take | 100
        | map | [](auto i) { return 2*i; }
        | to_pointer_iter();
    auto r = it
        | find_linear | [](auto i) { return i % 2 == 1; };
    ASSERT_EQ(noitem, r);
}

TEST(TestFindLinear, find_first_optional) {
    int iterated = 0;
    auto r = indices
        | map | counter_wrap
        | inspect | [&](auto&) {
            ++iterated; }
        | find_linear | [](auto const& i) {
            return i.value == 21; };

    ASSERT_EQ(iterated, 22);
    ASSERT_TRUE(r.has_value());
    ASSERT_EQ(r->value, 21);
    ASSERT_EQ(r->copies, 0);
    NO_CLANG(ASSERT_EQ(r->moves, 0));
}

TEST(TestFindLinear, find_first_pointer) {
    auto vector = std::vector<ctor_count<int>>(100);
    vector[78].value = 99;

    int iterated = 0;
    auto r = vector
        | inspect | [&](auto& i) {
            ASSERT_EQ(i.moves, 0);
            ASSERT_EQ(i.copies, 0);
            ++iterated; }
        | find_linear | [](auto const& i) {
            return i.value == 99; };

    ASSERT_EQ(iterated, 79);
    ASSERT_TRUE(r.has_value());
    ASSERT_EQ(r->value, 99);
    ASSERT_EQ(r->moves, 0);
    // pointer to counter is copied into result's optional
    ASSERT_EQ(r->copies, 1);
}

TEST(TestFindLinear, find_first_pointer_move) {
    auto vector = std::vector<ctor_count<int>>(100);
    vector[78].value = 99;

    int iterated = 0;
    auto r = vector
        | inspect | [&](auto& i) {
            ASSERT_EQ(i.moves, 0);
            ASSERT_EQ(i.copies, 0);
            ++iterated; }
        | move()
        | find_linear | [](auto const& i) {
            return i.value == 99; };

    ASSERT_EQ(iterated, 79);
    ASSERT_TRUE(r.has_value());
    ASSERT_EQ(r->value, 99);
    ASSERT_EQ(r->copies, 0);
    // pointer to counter is moved into result's optional
    ASSERT_EQ(r->moves, 1);
}

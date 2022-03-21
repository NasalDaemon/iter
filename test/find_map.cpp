#include "test.hpp"

TEST(TestFindMap, not_found_optional) {
    auto r = indices
        | take | 100
        | find_map | [](auto) { return item<int>(); };
    ASSERT_EQ(noitem, r);
}

TEST(TestFindMap, not_found_pointer) {
    auto r = indices
        | take | 100
        | find_map | [](auto) { return item<int&>(); };
    ASSERT_EQ(noitem, r);
}

TEST(TestFindMap, find_first_optional) {
    int iterated = 0;
    auto r = indices
        | map | counter_wrap
        | inspect | [&](auto&) {
            ++iterated; }
        | find_map | [](auto const& i) {
            return i.value == 21 ? item(i) : noitem; };

    ASSERT_EQ(iterated, 22);
    ASSERT_TRUE(r.has_value());
    ASSERT_EQ(r->value, 21);
    NO_CLANG(ASSERT_EQ(r->moves, 0)); // no copies or moves into result
    ASSERT_EQ(r->copies, 1); // copy from find_map
}

TEST(TestFindMap, find_first_pointer) {
    auto vector = std::vector<ctor_count<int>>(100);
    vector[78].value = 99;

    int iterated = 0;
    auto r = vector
        | inspect | [&](auto& i) {
            ASSERT_EQ(i.moves, 0);
            ASSERT_EQ(i.copies, 0);
            ++iterated; }
        | find_map | [c = std::aligned_storage_t<sizeof(ctor_count<int>)>{}](auto const& i) mutable {
            return i.value == 99
                ? item(unstable_ref(*new (reinterpret_cast<ctor_count<int>*>(&c)) ctor_count<int>(i)))
                : noitem; };

    ASSERT_EQ(iterated, 79);
    ASSERT_TRUE(r.has_value());
    ASSERT_EQ(r->value, 99);
    ASSERT_EQ(r->moves, 0);
    // pointer to counter is copied into result's optional
    ASSERT_EQ(r->copies, 2);
}

TEST(TestFindMap, find_first_pointer_move) {
        auto vector = std::vector<ctor_count<int>>(100);
    vector[78].value = 99;

    int iterated = 0;
    auto r = vector
        | inspect | [&](auto& i) {
            ASSERT_EQ(i.moves, 0);
            ASSERT_EQ(i.copies, 0);
            ++iterated; }
        | find_map | [c = std::aligned_storage_t<sizeof(ctor_count<int>)>{}](auto const& i) mutable {
            return move_item {
                i.value == 99
                    ? item(unstable_ref(*new (reinterpret_cast<ctor_count<int>*>(&c)) ctor_count<int>(i)))
                    : noitem }; };

    ASSERT_EQ(iterated, 79);
    ASSERT_TRUE(r.has_value());
    ASSERT_EQ(r->value, 99);
    ASSERT_EQ(r->moves, 1);
    // pointer to counter is copied into result's optional
    ASSERT_EQ(r->copies, 1);
}

#include "test.hpp"

TEST(TestChunkBy, proj) {
    auto a = std::array {
        tuple{1, 'a'},
        tuple{2, 'a'},
        tuple{6, 'b'},
        tuple{2, 'b'},
        tuple{5, 'c'},
        tuple{9, 'a'},
        tuple{0, 'b'}
    };
    auto actual = a
        | chunk_by | [](auto& kv) -> auto& {
            return get<1>(kv); }
        | map | [](auto&& it) {
            return FWD(it) | keys() | sum(); }
        | to_vector();
    
    auto expected = std::vector{3, 8, 5, 9, 0};
    ASSERT_EQ(actual, expected);
}

TEST(TestChunkBy, adj) {
    auto a = std::array {
        tuple{1, 'a'},
        tuple{2, 'a'},
        tuple{6, 'b'},
        tuple{2, 'b'},
        tuple{5, 'c'},
        tuple{9, 'a'},
        tuple{0, 'b'}
    };
    auto actual = a
        | chunk_by | [](auto& l, auto& r) {
            return get<1>(l) == get<1>(r); }
        | map | [](auto&& it) {
            return FWD(it) | keys() | sum(); }
        | to_vector();

    auto expected = std::vector{3, 8, 5, 9, 0};
    ASSERT_EQ(actual, expected);
}

TEST(TestChunkBy, proj_empty) {
    auto actual = empty<int>
        | chunk_by | [](int l) {
            return l == 0; }
        | map | to_vector
        | to_vector();
    auto expected = std::vector<std::vector<int>>{std::vector<int>{}};
    ASSERT_EQ(actual, expected);
}

TEST(TestChunkBy, adj_empty) {
    auto actual = empty<int>
        | chunk_by | [](int l, int r) {
            return l == r; }
        | map | to_vector
        | to_vector();
    auto expected = std::vector<std::vector<int>>{std::vector<int>{}};
    ASSERT_EQ(actual, expected);
}

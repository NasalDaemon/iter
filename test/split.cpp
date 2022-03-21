#include "test.hpp"

TEST(TestSplit, string_comma) {
    std::string str = "test,test,,testy";
    auto s = str |split| ',' |map| to_string | to_vector();
    std::vector<std::string> expected = {"test", "test", "", "testy"};
    ASSERT_EQ(s, expected);
}

TEST(TestSplit, empty) {
    std::string str = "";
    auto s = str |split| ',' |map| to_string | to_vector();
    std::vector<std::string> expected = {""};
    ASSERT_EQ(s, expected);
}

TEST(TestSplit, no_inner_iteration) {
    std::string str = "test,test,,testy";
    std::size_t count = 0;
    str |split| ',' |foreach| [&](auto&&) { ++count; };
    ASSERT_EQ(count, 4);
}

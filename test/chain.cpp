#include "test.hpp"
#include "iter/wrap.hpp"

TEST(TestChain, pointer_random_access) {
    constexpr auto c = once{0} |chain| once{1} |chain| once{2};
    using type = std::remove_cvref_t<decltype(c)>;
    static_assert(!concepts::owned_item<next_t<type>>);
    static_assert(concepts::random_access_iter<type>);
    static_assert(impl::size(c) == 3);

    auto c1 = c;
    ASSERT_EQ(0, *impl::next(c1));
    ASSERT_EQ(1, *impl::next(c1));
    auto c2 = c1;
    ASSERT_EQ(2, *impl::next(c1));
    ASSERT_EQ(2, *impl::next(c2));
    ASSERT_EQ(noitem, impl::next(c1));
    ASSERT_EQ(noitem, impl::next(c2));
    ASSERT_EQ(noitem, impl::next(c1));
    ASSERT_EQ(noitem, impl::next(c2));
}

TEST(TestChain, pointer_random_access_rvo) {
    auto a = std::array<ctor_count<int>, 3>{0, 1, 2};
    auto c1 = wrap{once_ref(a[0])}.chain(once_ref(a[1])).chain(once_ref(a[2]));
    using type = std::remove_cvref_t<decltype(c1)>;
    static_assert(!concepts::owned_item<next_t<type>>);
    static_assert(concepts::random_access_iter<type>);

    ASSERT_EQ(impl::size(c1), 3);

    auto assert_next = [&](auto& it, int value, int copies = 0, int moves = 0) {
        auto n = impl::next(it);
        ASSERT_EQ(value, n->value);
        ASSERT_EQ(copies, n->copies);
        ASSERT_EQ(moves, n->moves);
    };

    assert_next(c1, 0, 0, 0);
    assert_next(c1, 1, 0, 0);
    auto c2 = c1;
    assert_next(c1, 2, 0, 0);
    assert_next(c2, 2, 0, 0);
    ASSERT_EQ(noitem, impl::next(c1));
    ASSERT_EQ(noitem, impl::next(c2));
    ASSERT_EQ(noitem, impl::next(c1));
    ASSERT_EQ(noitem, impl::next(c2));
}

TEST(TestChain, optional_random_access) {
    constexpr auto c = once{0} |chain| once{1} |chain| range{2, 3};
    using type = std::remove_cvref_t<decltype(c)>;
    static_assert(concepts::owned_item<next_t<type>>);
    static_assert(concepts::random_access_iter<type>);
    static_assert(impl::size(c) == 3);

    auto c1 = c;
    ASSERT_EQ(0, *impl::next(c1));
    ASSERT_EQ(1, *impl::next(c1));
    auto c2 = c1;
    ASSERT_EQ(2, *impl::next(c1));
    ASSERT_EQ(2, *impl::next(c2));
    ASSERT_EQ(noitem, impl::next(c1));
    ASSERT_EQ(noitem, impl::next(c2));
    ASSERT_EQ(noitem, impl::next(c1));
    ASSERT_EQ(noitem, impl::next(c2));
}

TEST(TestChain, optional_random_access_rvo) {
    auto a = ctor_count<int>{1};
    auto c1 = range{0, 1} |map| counter_wrap
        |chain| once_ref(a)
        |chain| (range{2, 4} |map| counter_wrap);
    using type = std::remove_cvref_t<decltype(c1)>;
    static_assert(concepts::owned_item<next_t<type>>);
    static_assert(concepts::random_access_iter<type>);

    ASSERT_EQ(impl::size(c1), 4);

    auto assert_next = [&](auto& it, int value, int copies = 0, int moves = 0) {
        auto n = impl::next(it);
        ASSERT_EQ(value, n->value);
        ASSERT_EQ(copies, n->copies);
        ASSERT_EQ(moves, n->moves);
    };

    assert_next(c1, 0, 0, 0);
    auto c2 = c1;
    assert_next(c1, 1, 1, 0);
    assert_next(c2, 1, 1, 0);
    assert_next(c1, 2, 0, 0);
    assert_next(c1, 3, 0, 0);
    assert_next(c2, 2, 0, 0);
    assert_next(c2, 3, 0, 0);
    ASSERT_EQ(noitem, impl::next(c1));
    ASSERT_EQ(noitem, impl::next(c2));
    ASSERT_EQ(noitem, impl::next(c1));
    ASSERT_EQ(noitem, impl::next(c2));
}

TEST(TestChain, not_random_access) {
    auto g = generate {
        [i=2] () mutable {
            return i++ == 2 ? item(2) : noitem;
        }
    };
    auto c = once{0} |chain| once{1} |chain| g;
    using type = std::remove_cvref_t<decltype(c)>;
    static_assert(concepts::owned_item<next_t<type>>);
    static_assert(!concepts::random_access_iter<type>);

    auto c1 = c;
    ASSERT_EQ(0, *impl::next(c1));
    ASSERT_EQ(1, *impl::next(c1));
    auto c2 = c1;
    ASSERT_EQ(2, *impl::next(c1));
    ASSERT_EQ(2, *impl::next(c2));
    ASSERT_EQ(noitem, impl::next(c1));
    ASSERT_EQ(noitem, impl::next(c2));
    ASSERT_EQ(noitem, impl::next(c1));
    ASSERT_EQ(noitem, impl::next(c2));
}


TEST(TestChain, not_random_access_rvo) {
    auto a = ctor_count<int>{1};
    auto c1 = range{0, 1} |map| counter_wrap |filter| [](auto&) {return true;}
        |chain| once_ref(a)
        |chain| (range{2, 4} |map| counter_wrap);
    using type = std::remove_cvref_t<decltype(c1)>;
    static_assert(concepts::owned_item<next_t<type>>);
    static_assert(!concepts::random_access_iter<type>);

    auto assert_next = [&](auto& it, int value, int copies = 0, int moves = 0) {
        auto n = impl::next(it);
        ASSERT_EQ(value, n->value);
        ASSERT_EQ(copies, n->copies);
        ASSERT_EQ(moves, n->moves);
    };

    assert_next(c1, 0, 0, 0);
    assert_next(c1, 1, 1, 0);
    auto c2 = c1;
    assert_next(c1, 2, 0, 0);
    assert_next(c1, 3, 0, 0);
    assert_next(c2, 2, 0, 0);
    assert_next(c2, 3, 0, 0);
    ASSERT_EQ(noitem, impl::next(c1));
    ASSERT_EQ(noitem, impl::next(c2));
    ASSERT_EQ(noitem, impl::next(c1));
    ASSERT_EQ(noitem, impl::next(c2));
}

TEST(TestChain, empty) {
    auto e = empty<int> |chain| empty<int> |chain| once{99} |chain| empty<int> |chain| iter::optional{101};
    using type = std::remove_cvref_t<decltype(e)>;
    static_assert(!concepts::owned_item<next_t<type>>);
    static_assert(concepts::random_access_iter<type>);
    auto e1 = e;
    ASSERT_EQ(*impl::next(e1), 99);
    ASSERT_EQ(*impl::next(e1), 101);
    ASSERT_EQ(impl::next(e1), noitem);
}

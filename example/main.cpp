#include "iter/wrap.hpp"
#include "iter/macros/dollar/define.hpp"
#include "iter/enable_ranges.hpp"

#include <algorithm>
#include <string>
#include <iostream>
#include <map>

using namespace xtd::literals;
using namespace iter;

float getsum1(const std::array<float, 64>& a) {
    float fsum = 0;
    for (auto [a, i] : iter::zip(a, iter::indices)) {
        fsum += a * i;
    }
    return fsum;
}

float getsum2(const std::array<float, 64>& a, const std::array<float, 64>& b) {
    return a
        $(map) ([](auto i) {
            return i * 2; })
        $(zip) (b)
        $(filter) ([](auto ab) {
            auto& [a, b] = ab;
            return a < 3; })
        $(enumerate) ()
        $(flatmap) ([](auto abi) {
            auto& [ab, i] = abi;
            auto& [a, b] = ab;
            auto r = i*b - a > 0;
            return r > 0 ? iter::item(r) : iter::noitem; })
        $(sum) ();
}

constexpr auto fib(size_t max) {
    return generate {
        [=, a = 0ul, b = 1ul]() mutable {
            a = std::exchange(b, b + a);
            return a <= max ? iter::item(a) : iter::noitem;
        }
    };
}

constexpr auto get1(size_t max) {
    std::size_t r = 0;
    for (auto i : fib(max)) {
        r = i;
    }
    return r;
}

constexpr auto triples() {
    return range{1} |flatmap| [](int z) {
        return range{1, z+1} |flatmap| [=](int x) {
            return range{x, z+1} |flatmap| [=](int y) {
                return x*x + y*y == z*z
                    ? iter::item(tuple{x, y, z})
                    : iter::noitem;
            };
        };
    };
}

constexpr int sum_triples() {
    return triples()
        | take(_, 10)
        | fold(_, 0, [](auto acc, auto xyz) {
            auto& [x, y, z] = xyz;
            return acc + x + y + z; });
}

consteval auto get2(size_t max) {
    return *iter::wrap{fib(max)}.last();
}

#include <ranges>

int main() {

    std::cout << "==: " << (once{1} $take(1) < once{1}) << "\n";
    std::cout << "!=: " << (once{1} $take(1) <= once{1}) << "\n";
    auto cmp = repeat{1} $take(0) <=> once{1};
    auto cmp_int = cmp == 0 ? 0 : cmp < 0 ? -1 : 1;
    std::cout << "<=>: " << cmp_int << "\n";

    std::cout << "fib " << get1(100) << "\n";
    std::cout << "trips " << sum_triples() << "\n";
    for (auto [x, y, z] : triples() |take| 10) {
        std::cout << x << ", " << y << ", " << z << "\n";
    }

    auto collected = 0 |until| 10
        |map| [](auto i) {
            return std::pair(i, -i); }
        |move| _
        |sorted| _;

    for (auto [k, v] : collected) {
        std::cout << k << ", " << v << "\n";
    }

    auto [l, r] = range{10, 20}
        |enumerate| _
        |unzip| _;

    std::cout << "l";
    for (auto i : l) {
        std::cout << i << ", ";
    }
    std::cout << "\nr";
    for (auto i : r) {
        std::cout << i << ", ";
    }

    auto ar = std::array<int, 8>{9, -10, 3, 55, 66, 2, 15, 5};
    auto m = ar
        |skip| 3
        |skip_while| [](auto a) {
            return a > 2; }
        |min| _;
    std::cout << "\nmax: " << *m;

    auto last = ar |iter::last| _;

    std::cout << "\nlast: " << *last << "\n";

    auto [_1, _2, _3] = std::tuple(1, 2, 3);
    auto two = once{_1} |chain| once{_2} |chain| generate{[i = 3]()mutable{ return iter::item(i++); }};
    // static_assert(concepts::random_access_iter<decltype(two)>);
    static_assert(concepts::owned_item<next_t<decltype(two)>>);

    for (auto [x, i] : two | enumerate(_) | take(_, 10) ) {
        std::cout << i << ": " << x << "\n";
    }

    auto once_arr = std::array{std::array{_1}, std::array{_2}};
    auto once_flatten = once_arr | flatten(_);
    static_assert(!iter::concepts::owned_item<next_t<decltype(once_flatten)>>);
    auto copy = once_flatten;
    for (auto i : copy | cycle(_) | take(_, 6)) {
        std::cout << "once: " << i << "\n";
    }

    auto a123 = std::array{1, 2, 3};
    for (auto i : a123 | cycle(_) | take(_, 10)) {
        std::cout << "cycle: " << i << "\n";
    }

    for (auto i : empty<int> |chain| once_ref(_1) |cycle| _ |take| 7) {
        std::cout << "repeat: " << i << "\n";
    }

    for (auto i : compound{1, [](auto i) { return i * 10; }} |take_while| [](auto i) { return i < 100'000'000; }) {
        std::cout << "compound: " << i << "\n";
    }

    std::cout << "\n";

    iter::scratch<64> s;
    static_assert(sizeof(detail::deleter) == 1);
    static_assert(alignof(detail::deleter) == 1);

    range{0, 10}
        | inspect | [](int i) {
            std::cout << "entering flatmap: " << i << "\n"; }
        | flatmap | [&s](int i) -> boxed<int&> {
            if (i % 2 == 0)
                return empty<int> | box | s;
            else
                return once{i} |filter| [](auto&&){return true;} | box | s;
            }
        | foreach | [](int i) {
            std::cout << "made it: " << i << "\n"; };

    for (auto i : range{0, 10}
            | map(_, [](auto i) { return i*i; })
            | std::views::transform(std::identity{})
            | std::views::drop(2)
        )
        std::cout << "range i: " << i << "\n";
}

#if !defined(ITER_COMPILER_GCC) || __GNUC__ >= 12
static constexpr auto sum_0_to_9 = iter::wrap{iter::indices}
    .take(10)
    .flatmap([](auto i) {
        // return iter::once{i};
        return iter::to_iter_consteval(std::array{i, i + 1, i + 2});
        // return iter::generate{[array = std::array{i, i + 1, i + 2}, i = 0]() mutable {
        //     return i < array.size() ? iter::item_ref(array[i++]) : iter::noitem;
        // }};

    })
    // .chunks(2).flatten()
    .sum();

static_assert(sum_0_to_9 == 165);
#endif

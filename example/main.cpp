#include "iter.hpp"
#include "iter/dollar_macros/define.hpp"

#include <string>
#include <iostream>
#include <map>

using namespace xtd::literals;
using namespace iter;

float getsum1(const std::array<float, 64>& a) {
    float fsum = 0;
    for (auto [a, i] : iter::zip(a, iter::indices) ) {
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
            return r > 0 ? std::optional(r) : std::nullopt; })
        $(sum) ();
}

constexpr auto fib(size_t max) {
    return generate {
        [=, a = 0ul, b = 1ul]() mutable {
            a = std::exchange(b, b + a);
            return a <= max ? std::optional(a) : std::nullopt;
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
    return range(1) |flatmap| [](int z) {
        return range(1, z+1) |flatmap| [=](int x) {
            return range(x, z+1) |flatmap| [=](int y) {
                return x*x + y*y == z*z
                    ? std::optional(std::tuple(x, y, z))
                    : std::nullopt;
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
    return *(fib(max) | last(_));
}

#include <algorithm>

int main() {

    std::cout << "==: " << (once(1) $take(1) < once(1)) << "\n";
    std::cout << "!=: " << (once(1) $take(1) <= once(1)) << "\n";
    auto cmp = repeat{1} $take(0) <=> once(1);
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
    auto two = once(_1) |chain| once(_2) |chain| generate{[i = 3]()mutable{ return std::optional(i++); }};
    // static_assert(concepts::random_access_iter<decltype(two)>);
    static_assert(concepts::optional_iter<decltype(two)>);

    for (auto [x, i] : two | enumerate(_) | take(_, 10) ) {
        std::cout << i << ": " << x << "\n";
    }

    // auto once_arr = ;
    auto once_flatten = std::array{std::array{_1}, std::array{_2}} | flatten(_);
    static_assert(iter::concepts::pointer_iter<decltype(once_flatten)>);
    auto copy = once_flatten;
    for (auto i : copy | cycle(_) | take(_, 6)) {
        std::cout << "once: " << i << "\n";
    }

    for (auto i : std::array{1, 2, 3} | cycle(_) | take(_, 10)) {
        std::cout << "cycle: " << i << "\n";
    }

    for (auto i : empty<int> |chain| once_ref(_1) |cycle| _ |take| 7) {
        std::cout << "repeat: " << i << "\n";
    }

    for (auto i : compound(1, [](auto i) { return i * 10; }) |take_while| [](auto i) { return i < 100'000'000; }) {
        std::cout << "compound: " << i << "\n";
    }

    std::cout << "\n";

    iter::scratch<64> s;
    static_assert(sizeof(detail::deleter) == 1);
    static_assert(alignof(detail::deleter) == 1);

    range(0, 10)
        | inspect | [](int i) {
            std::cout << "entering flatmap: " << i << "\n"; }
        | flatmap | [&s](int i) -> boxed<int*> {
            if (i % 2 == 0)
                return empty<int> | box | s;
            else
                return once(i) |filter|[](auto&&){return true;} | box | s;
            }
        | foreach | [](int i) {
            std::cout << "made it: " << i << "\n"; };


}
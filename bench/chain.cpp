#include "benchmark/benchmark.h"
#include "iter.hpp"

using namespace xtd::literals;

void bench_iter_chain(benchmark::State& state)
{
    std::array<int, 128> a{};
    std::array<int, 128> b{};
    std::array<int, 128> c{};

    for (auto s : state) {
        auto it = a |iter::chain| b |iter::chain| c;
        static_assert(iter::concepts::random_access_iter<decltype(it)>);
        static_assert(iter::concepts::pointer_iter<decltype(it)>);
        auto sum = it |iter::sum| _;
        benchmark::DoNotOptimize(sum > 10);
    }
}

void bench_std_chain(benchmark::State& state)
{
    std::array<int, 128> a{};
    std::array<int, 128> b{};
    std::array<int, 128> c{};
    constexpr int size = 3 * std::size(a);

    for (auto _ : state) {
        int sum = 0;
        for (int i = 0; i < size; ++i) {
            sum += i < 128
                ? a[i]
                : i < 256
                    ? b[i-128]
                    : c[i-256];
        }
        benchmark::DoNotOptimize(sum > 10);
    }
}

BENCHMARK(bench_iter_chain);
BENCHMARK(bench_std_chain);

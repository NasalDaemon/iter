#include "benchmark/benchmark.h"
#include "iter.hpp"
#include "extend/dollar_macros/define.hpp"

#include <numeric>
#include <ranges>

using namespace xtd::literals;

void bench_iter_mult(benchmark::State& state)
{
    std::array<int, 64> a{};
    std::array<int, 64> b{};
    std::array<int, 64> c{};

    for (auto s : state) {
        for (auto [x, y, z] : iter::zip(a, b, c) | iter::cycle() | iter::take | 128) {
            z = x * y;
            benchmark::DoNotOptimize(c.data());
        }
        benchmark::ClobberMemory();
    }
}

void bench_std_mult(benchmark::State& state)
{
    std::array<int, 64> a{};
    std::array<int, 64> b{};
    std::array<int, 64> c{};

    for (auto _ : state) {
        for (size_t i = 0; i < 128; ++i) {
            size_t j = i % 64;
            c[j] = a[j] * b[j];
            benchmark::DoNotOptimize(c.data());
        }
        benchmark::ClobberMemory();
    }
}

BENCHMARK(bench_iter_mult);
BENCHMARK(bench_std_mult);

void bench_iter_zip_sum(benchmark::State& state)
{
    std::array<int, 128> a{};
    std::array<int, 128> b{};
    std::array<int, 128> c{};

    for (auto s : state) {
        auto it = iter::zip(a, b, c);
        static_assert(iter::concepts::random_access_iter<decltype(it)>);
        auto sum = it | iter::fold(_, 0, [](auto acc, auto abc) {
            auto& [a, b, c] = abc;
            return acc + a + b + c; });
        benchmark::DoNotOptimize(sum > 10);
    }
}

void bench_std_zip_sum(benchmark::State& state)
{
    std::array<int, 128> a{};
    std::array<int, 128> b{};
    std::array<int, 128> c{};

    for (auto _ : state) {
        int sum = 0;
        for (size_t i = 0; i < 128; ++i) {
            sum += a[i] + b[i] + c[i];
        }
        benchmark::DoNotOptimize(sum > 10);
    }
}

void bench_iter_seq_sum(benchmark::State& state)
{
    std::array<int, 128> a{};
    std::array<int, 128> b{};
    std::array<int, 128> c{};

    for (auto _ : state) {
        auto sum = a $$ iter::sum() +
                   b $$ iter::sum() +
                   c $$ iter::sum();
        benchmark::DoNotOptimize(sum > 10);
    }
}

void bench_std_seq_sum(benchmark::State& state)
{
    std::array<int, 128> a{};
    std::array<int, 128> b{};
    std::array<int, 128> c{};

    for (auto _ : state) {
        auto sum = std::accumulate(a.begin(), a.end(), 0) +
                   std::accumulate(b.begin(), b.end(), 0) +
                   std::accumulate(c.begin(), c.end(), 0);
        benchmark::DoNotOptimize(sum > 10);
    }
}

void bench_iter_chain_sum(benchmark::State& state)
{
    std::array<int, 128> a{};
    std::array<int, 128> b{};
    std::array<int, 128> c{};

    for (auto _ : state) {
        auto it = iter::wrap(a).chain(b).chain(c);
        static_assert(iter::concepts::random_access_iter<decltype(it)>);
        auto sum = it.sum();
        benchmark::DoNotOptimize(sum > 10);
    }
}

BENCHMARK(bench_iter_zip_sum);
BENCHMARK(bench_std_zip_sum);
BENCHMARK(bench_iter_seq_sum);
BENCHMARK(bench_std_seq_sum);
BENCHMARK(bench_iter_chain_sum);
BENCHMARK(bench_iter_chain_sum);

void bench_iter_enumerate(benchmark::State& state)
{
    std::vector<float> a(1024);
    std::iota(a.begin(), a.end(), 10);

    for (auto s : state) {
        auto sum = a
            | iter::enumerate_<int>()
            | iter::map | [](auto ai) {
                auto& [a, i] = ai;
                return a + i; }
            | iter::sum();
        benchmark::DoNotOptimize(sum > 10);
    }
}

void bench_std_enumerate(benchmark::State& state)
{
    std::vector<float> a(1024);
    std::iota(a.begin(), a.end(), 10);

    for (auto _ : state) {
        float sum = 0;
        int i = 0;
        for (auto f : a) {
            sum += f + i++;
        }
        benchmark::DoNotOptimize(sum > 10);
    }
}

BENCHMARK(bench_iter_enumerate);
BENCHMARK(bench_std_enumerate);

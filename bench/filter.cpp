#include "benchmark/benchmark.h"
#include "iter/iter.hpp"

#ifndef ITER_COMPILER_CLANG
#include <ranges>
#endif
#include <numeric>

using namespace xtd::literals;

auto mapper = [](auto i) { return 2 * i; };
auto filterer = [](auto i) { return i % 4 == 0; };

void bench_iter_filtermap_mapfirst(benchmark::State& state)
{
    std::vector<int> a(2048);
    std::iota(a.begin(), a.end(), 10);

    for (auto s : state) {
        auto sum = a
            | iter::filter_map(_, [&](auto f) {
                auto r = mapper(f);
                return filterer(r) ? iter::item(r) : iter::noitem; })
            | iter::sum(_);
        benchmark::DoNotOptimize(sum > 10);
    }
}

void bench_iter_map_filter(benchmark::State& state)
{
    std::vector<int> a(2048);
    std::iota(a.begin(), a.end(), 10);

    for (auto s : state) {
        auto sum = a
            | iter::map(_, mapper)
            | iter::filter(_, filterer)
            | iter::sum(_);
        benchmark::DoNotOptimize(sum > 10);
    }
}

void bench_iter_map_flatten(benchmark::State& state)
{
    std::vector<int> a(2048);
    std::iota(a.begin(), a.end(), 10);

    for (auto s : state) {
        auto sum = a
            | iter::map(_, [&](auto f) {
                auto r = mapper(f);
                return filterer(r) ? iter::item(r) : iter::noitem; })
            | iter::flatten(_)
            | iter::sum(_);
        benchmark::DoNotOptimize(sum > 10);
    }
}

#ifndef ITER_COMPILER_CLANG
void bench_std_map_filter(benchmark::State& state)
{
    std::vector<int> a(2048);
    std::iota(a.begin(), a.end(), 10);

    for (auto _ : state) {
        auto it = a
            | std::views::transform(mapper)
            | std::views::filter(filterer);
        auto sum = std::accumulate(it.begin(), it.end(), 0.f);
        benchmark::DoNotOptimize(sum > 10);
    }
}
#endif

BENCHMARK(bench_iter_filtermap_mapfirst);
BENCHMARK(bench_iter_map_flatten);
BENCHMARK(bench_iter_map_filter);
#ifndef ITER_COMPILER_CLANG
BENCHMARK(bench_std_map_filter);
#endif

void bench_iter_filter_map(benchmark::State& state)
{
    std::vector<int> a(2048);
    std::iota(a.begin(), a.end(), 10);

    for (auto s : state) {
        auto sum = a
            | iter::filter(_, filterer)
            | iter::map(_, mapper)
            | iter::sum(_);
        benchmark::DoNotOptimize(sum > 10);
    }
}

void bench_iter_filtermap_filterfirst(benchmark::State& state)
{
    std::vector<int> a(2048);
    std::iota(a.begin(), a.end(), 10);

    for (auto s : state) {
        auto sum = a
            | iter::filter_map | [&](auto f) {
                return filterer(f) ? iter::item(mapper(f)) : iter::noitem; }
            | iter::sum(_);
        benchmark::DoNotOptimize(sum > 10);
    }
}

#ifndef ITER_COMPILER_CLANG
void bench_std_filter_map(benchmark::State& state)
{
    std::vector<int> a(2048);
    std::iota(a.begin(), a.end(), 10);

    for (auto _ : state) {
        auto it = a
            | std::views::filter(filterer)
            | std::views::transform(mapper);
        auto sum = std::accumulate(it.begin(), it.end(), 0.f);
        benchmark::DoNotOptimize(sum > 10);
    }
}
#endif

BENCHMARK(bench_iter_filtermap_filterfirst);
BENCHMARK(bench_iter_filtermap_filterfirst);
BENCHMARK(bench_iter_filter_map);
BENCHMARK(bench_iter_filter_map);
#ifndef ITER_COMPILER_CLANG
BENCHMARK(bench_std_filter_map);
BENCHMARK(bench_std_filter_map);
#endif

BENCHMARK(bench_iter_filtermap_filterfirst);
BENCHMARK(bench_iter_filter_map);
#ifndef ITER_COMPILER_CLANG
BENCHMARK(bench_std_filter_map);
#endif

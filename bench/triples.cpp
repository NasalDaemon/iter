#include "benchmark/benchmark.h"
#include "iter/iter.hpp"

using namespace xtd::literals;
using namespace iter;

auto triples_filter_map() {
    return range{1} |flatmap| [](int z) {
        return range{1, z} |flatmap| [=](int x) {
            return range{x, z}
                |filter| [=](int y) {
                    return x*x + y*y == z*z; }
                |map| [=](int y) {
                    return tuple{x, y, z}; };
        };
    };
}

auto triples_map_filter() {
    return range{1} |flatmap| [](int z) {
        return range{1, z} |flatmap| [=](int x) {
            return range{x, z}
                |map| [=](int y) {
                    return tuple{x, y, z}; }
                |filter| xtd::apply([](int x, int y, int z) {
                    return x*x + y*y == z*z; });
        };
    };
}

auto triples_flatmap() {
    return range{1} |flatmap| [](int z) {
        return range{1, z} |flatmap| [=](int x) {
            return range{x, z} |flatmap| [=](int y) {
                return x*x + y*y == z*z
                    ? MAKE_ITEM(tuple{x, y, z})
                    : noitem;
            };
        };
    };
}

static scratch<24> s = {};

auto triples_flatmap_virtual() {
    return range{1} |flatmap| [](int z) {
        return range{1, z} |flatmap| [=](int x) {
            return range{x, z} |flatmap| [=](int y) {
                return x*x + y*y == z*z
                    ? box(once<std::tuple<int, int, int>>{{x, y, z}}, s)
                    : box(empty<std::tuple<int, int, int>>, s);
            };
        };
    };
}

int sum_triples_filter_map(int n) {
    return triples_filter_map()
        | take | n
        | fold (_, 0, [](int acc, auto xyz) {
            auto& [x, y, z] = xyz;
            return acc + x + y + z; });
}

int sum_triples_map_filter(int n) {
    return triples_map_filter()
        | take | n
        | fold (_, 0, [](int acc, auto xyz) {
            auto& [x, y, z] = xyz;
            return acc + x + y + z; });
}

int sum_triples_flatmap(int n) {
    return triples_flatmap()
        | take | n
        | fold (_, 0, [](int acc, auto xyz) {
            auto& [x, y, z] = xyz;
            return acc + x + y + z; });
}

int sum_triples_flatmap_virtual(int n) {
    return triples_flatmap_virtual()
        | take | n
        | fold (_, 0, [](int acc, auto xyz) {
            auto& [x, y, z] = xyz;
            return acc + x + y + z; });
}

auto make_triples_filter_map(int n) {
    return triples_filter_map()
        | take | n
        | to_vector | n;
}

auto make_triples_map_filter(int n) {
    return triples_map_filter()
        | take | n
        | to_vector | n;
}

auto make_triples_flatmap(int n) {
    return triples_flatmap()
        | take | n
        | to_vector | n;
}

auto make_triples_flatmap_virtual(int n) {
    return triples_flatmap_virtual()
        | take | n
        | to_vector | n;
}

template<class F>
void c_triples(int n, F&& func) {
    int i = 0;
    for (int z = 1; true; ++z) {
        for (int x = 1; x < z; ++x) {
            for (int y = x; y < z; ++y) {
                if (x*x + y*y == z*z) {
                    std::invoke(FWD(func), x, y, z);
                    if (++i == n) return;
                }
            }
        }
    }
}

int sum_c_triples(int n) {
    int sum = 0;
    c_triples(n, [&](auto x, auto y, auto z) {
        sum += x + y + z; });
    return sum;
}

auto make_c_triples(int n) {
    auto v = std::vector<std::tuple<int, int, int>>();
    v.reserve(n);
    c_triples(n, [&](auto x, auto y, auto z) {
        v.emplace_back(x, y, z); });
    return v;
}

void bench_iter_filter_map_sum_triples(benchmark::State& state)
{
    for (auto _ : state) {
        benchmark::DoNotOptimize(sum_triples_filter_map(state.range(0)));
    }
}
void bench_iter_map_filter_sum_triples(benchmark::State& state)
{
    for (auto _ : state) {
        benchmark::DoNotOptimize(sum_triples_map_filter(state.range(0)));
    }
}
void bench_iter_flatmap_sum_triples(benchmark::State& state)
{
    for (auto _ : state) {
        benchmark::DoNotOptimize(sum_triples_flatmap(state.range(0)));
    }
}
void bench_iter_flatmap_virtual_sum_triples(benchmark::State& state)
{
    for (auto _ : state) {
        benchmark::DoNotOptimize(sum_triples_flatmap_virtual(state.range(0)));
    }
}
void bench_c_sum_triples(benchmark::State& state)
{
    for (auto _ : state) {
        benchmark::DoNotOptimize(sum_c_triples(state.range(0)));
    }
}

auto sum_(auto& v) {
    return v | fold(_, 0, [](auto acc, auto xyz) {
        auto& [x, y, z] = xyz;
        return acc + x + y + z;
    });
}

void bench_iter_filter_map_make_triples(benchmark::State& state)
{
    for (auto _ : state) {
        benchmark::DoNotOptimize(make_triples_filter_map(state.range(0)));
    }
}
void bench_iter_map_filter_make_triples(benchmark::State& state)
{
    for (auto _ : state) {
        benchmark::DoNotOptimize(make_triples_map_filter(state.range(0)));
    }
}
void bench_iter_flatmap_make_triples(benchmark::State& state)
{
    for (auto _ : state) {
        benchmark::DoNotOptimize(make_triples_flatmap(state.range(0)));
    }
}
void bench_iter_flatmap_virtual_make_triples(benchmark::State& state)
{
    for (auto _ : state) {
        benchmark::DoNotOptimize(make_triples_flatmap_virtual(state.range(0)));
    }
}
void bench_c_make_triples(benchmark::State& state)
{
    for (auto _ : state) {
        benchmark::DoNotOptimize(make_c_triples(state.range(0)));
    }
}

BENCHMARK(bench_iter_filter_map_sum_triples)->Arg(10);
BENCHMARK(bench_iter_map_filter_sum_triples)->Arg(10);
BENCHMARK(bench_iter_flatmap_sum_triples)->Arg(10);
BENCHMARK(bench_iter_flatmap_virtual_sum_triples)->Arg(10);
BENCHMARK(bench_c_sum_triples)->Arg(10);
BENCHMARK(bench_iter_filter_map_sum_triples)->Arg(100);
BENCHMARK(bench_iter_map_filter_sum_triples)->Arg(100);
BENCHMARK(bench_iter_flatmap_sum_triples)->Arg(100);
BENCHMARK(bench_iter_flatmap_virtual_sum_triples)->Arg(100);
BENCHMARK(bench_c_sum_triples)->Arg(100);
BENCHMARK(bench_iter_filter_map_sum_triples)->Arg(1000);
BENCHMARK(bench_iter_map_filter_sum_triples)->Arg(1000);
BENCHMARK(bench_iter_flatmap_sum_triples)->Arg(1000);
BENCHMARK(bench_iter_flatmap_virtual_sum_triples)->Arg(1000);
BENCHMARK(bench_c_sum_triples)->Arg(1000);

BENCHMARK(bench_iter_filter_map_make_triples)->Arg(10);
BENCHMARK(bench_iter_map_filter_make_triples)->Arg(10);
BENCHMARK(bench_iter_flatmap_make_triples)->Arg(10);
BENCHMARK(bench_iter_flatmap_virtual_make_triples)->Arg(10);
BENCHMARK(bench_c_make_triples)->Arg(10);
BENCHMARK(bench_iter_filter_map_make_triples)->Arg(100);
BENCHMARK(bench_iter_map_filter_make_triples)->Arg(100);
BENCHMARK(bench_iter_flatmap_make_triples)->Arg(100);
BENCHMARK(bench_iter_flatmap_virtual_make_triples)->Arg(100);
BENCHMARK(bench_c_make_triples)->Arg(100);
BENCHMARK(bench_iter_filter_map_make_triples)->Arg(1000);
BENCHMARK(bench_iter_map_filter_make_triples)->Arg(1000);
BENCHMARK(bench_iter_flatmap_make_triples)->Arg(1000);
BENCHMARK(bench_iter_flatmap_virtual_make_triples)->Arg(1000);
BENCHMARK(bench_c_make_triples)->Arg(1000);

#ifdef INCLUDE_ITER_GENERATOR_HPP

generator<tuple<int, int, int>> gen_triples() {
    for (int z = 1; true; ++z) {
        for (int x = 1; x < z; ++x) {
            for (int y = x; y < z; ++y) {
                if (x*x + y*y == z*z) {
                    co_yield {x, y, z};
                }
            }
        }
    }
}

int sum_gen_triples(int n) {
    return gen_triples()
        | take | n
        | fold (_, 0, [](int acc, auto xyz) {
            auto& [x, y, z] = xyz;
            return acc + x + y + z; });
}

auto make_gen_triples(int n) {
    return gen_triples()
        | take | n
        | to_vector | n;
}

void bench_iter_gen_sum_triples(benchmark::State& state)
{
    for (auto _ : state) {
        benchmark::DoNotOptimize(sum_gen_triples(state.range(0)));
    }
}

void bench_iter_gen_make_triples(benchmark::State& state)
{
    for (auto _ : state) {
        benchmark::DoNotOptimize(make_gen_triples(state.range(0)));
    }
}

BENCHMARK(bench_iter_gen_sum_triples)->Arg(10);
BENCHMARK(bench_iter_gen_sum_triples)->Arg(100);
BENCHMARK(bench_iter_gen_sum_triples)->Arg(1000);
BENCHMARK(bench_iter_gen_make_triples)->Arg(10);
BENCHMARK(bench_iter_gen_make_triples)->Arg(100);
BENCHMARK(bench_iter_gen_make_triples)->Arg(1000);

#endif // INCLUDE_ITER_GENERATOR_HPP

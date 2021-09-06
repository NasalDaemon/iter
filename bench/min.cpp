#include "benchmark/benchmark.h"
#include "iter.hpp"

#include <random>
#include <ranges>

namespace {
std::random_device rnd;
std::default_random_engine eng(rnd());

static auto ints = [] {
    std::array<int, 2048> a{};
    std::uniform_int_distribution<int> uid1(-2048, 2048);
    std::generate(a.begin(), a.end(), [&] { return uid1(eng); });
    return a;
}();

static auto strings = [] {
    std::array<std::string, 2048> a{};
    std::uniform_int_distribution<int> ranlen(0, 20);
    std::generate(a.begin(), a.end(), [&] {
        auto len = ranlen(eng);
        return std::string(len, 'a');
    });
    return a;
}();
}

void bench_iter_min_int(benchmark::State& state) {
    for (auto s : state) {
        auto m = ints | iter::min();
        benchmark::DoNotOptimize(m);
    }
}

void bench_std_min_int(benchmark::State& state) {
    for (auto s : state) {
        auto m = *std::min_element(ints.begin(), ints.end());
        benchmark::DoNotOptimize(m);
    }
}

void bench_c_min_int(benchmark::State& state) {
    for (auto s : state) {
        int* min = &ints[0];
        for (auto& i : ints) {
            if (i < *min) {
                min = &i;
            }
        }
        benchmark::DoNotOptimize(min);
    }
}

BENCHMARK(bench_iter_min_int);
BENCHMARK(bench_std_min_int);
BENCHMARK(bench_c_min_int);

void bench_iter_min_int_optional1(benchmark::State& state) {
    for (auto s : state) {
        auto m = ints
            | iter::map | std::identity{}
            | iter::min();
        benchmark::DoNotOptimize(m);
    }
}

void bench_iter_min_int_optional2(benchmark::State& state) {
    for (auto s : state) {
        auto m = iter::range{-state.range(0), state.range(0)}
            | iter::map | [](auto i) {
                return i*i; }
            | iter::min();
        benchmark::DoNotOptimize(m);
    }
}

BENCHMARK(bench_iter_min_int_optional1);
BENCHMARK(bench_iter_min_int_optional2)->Arg(1000);

#ifndef __clang__

void bench_std_min_int_optional1(benchmark::State& state) {
    for (auto s : state) {
        auto m = std::ranges::min(ints | std::views::transform(std::identity{}));
        benchmark::DoNotOptimize(m);
    }
}
void bench_std_min_int_optional2(benchmark::State& state) {
    for (auto s : state) {
        auto v = std::views::iota(-state.range(0), state.range(0))
            | std::views::transform([](auto i) {
                return i*i; });
        auto m = std::ranges::min(v);
        benchmark::DoNotOptimize(m);
    }
}

BENCHMARK(bench_std_min_int_optional1);
BENCHMARK(bench_std_min_int_optional2)->Arg(1000);

#endif

void bench_iter_min_string(benchmark::State& state) {
    for (auto s : state) {
        auto m = strings | iter::min();
        benchmark::DoNotOptimize(m);
    }
}

void bench_std_min_string(benchmark::State& state) {
    for (auto s : state) {
        auto m = *std::min_element(strings.begin(), strings.end());
        benchmark::DoNotOptimize(m);
    }
}

void bench_c_min_string(benchmark::State& state) {
    for (auto s : state) {
        std::string* min = &strings[0];
        for (auto& s : strings) {
            if (s < *min) {
                min = &s;
            }
        }
        benchmark::DoNotOptimize(min);
    }
}

BENCHMARK(bench_iter_min_string);
BENCHMARK(bench_std_min_string);
BENCHMARK(bench_c_min_string);

void bench_iter_min_by_int(benchmark::State& state) {
    for (auto s : state) {
        auto m = ints | iter::min_by | [](auto i) { return -i; };
        benchmark::DoNotOptimize(m);
    }
}

void bench_c_min_by_int(benchmark::State& state) {
    for (auto s : state) {
        int* min = &ints[0];
        int min_len = -*min;
        for (auto& i : ints) {
            auto newlen = -i;
            if (newlen < min_len) {
                min = &i;
                min_len = newlen;
            }
        }
        benchmark::DoNotOptimize(min);
    }
}

BENCHMARK(bench_iter_min_by_int);
BENCHMARK(bench_iter_min_by_int);
BENCHMARK(bench_c_min_by_int);
BENCHMARK(bench_c_min_by_int);

void bench_iter_min_by_string(benchmark::State& state) {
    for (auto s : state) {
        auto m = strings | iter::min_by | [](auto& s) { return s.length(); };
        benchmark::DoNotOptimize(m);
    }
}

void bench_c_min_by_string(benchmark::State& state) {
    for (auto s : state) {
        std::string* min = &strings[0];
        size_t min_len = min->length();
        for (auto& s : strings) {
            auto newlen = s.length();
            if (newlen < min_len) {
                min = &s;
                min_len = newlen;
            }
        }
        benchmark::DoNotOptimize(min);
    }
}

BENCHMARK(bench_iter_min_by_string);
BENCHMARK(bench_iter_min_by_string);
BENCHMARK(bench_c_min_by_string);
BENCHMARK(bench_c_min_by_string);

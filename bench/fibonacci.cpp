#include "benchmark/benchmark.h"
#include "iter.hpp"

using namespace xtd::literals;

constexpr auto iter_fib_generate(size_t max) {
    return iter::generate {
        [=, a = 0ul, b = 1ul]() mutable {
            a = std::exchange(b, b + a);
            return a <= max ? iter::item(a) : iter::noitem;
        }
    };
}

constexpr auto iter_generate_fib(size_t max) {
    return iter_fib_generate(max) | iter::sum();
}

constexpr auto std_fib(size_t max) {
    size_t sum = 0;
    auto a = 0ul, b = 1ul;
    for (;;) {
        a = std::exchange(b, b + a);
        if (a > max) break;
        sum += a;
    }
    return sum;
}

void bench_iter_generate_fib(benchmark::State& state)
{
    for (auto s : state) {
        auto sum = iter_generate_fib(state.range(0));
        benchmark::DoNotOptimize(sum > 10);
    }
}

void bench_std_fib(benchmark::State& state)
{
    for (auto _ : state) {
        auto sum = std_fib(state.range(0));
        benchmark::DoNotOptimize(sum > 10);
    }
}

BENCHMARK(bench_iter_generate_fib)->Arg(10'000);
BENCHMARK(bench_iter_generate_fib)->Arg(100'000'000);
BENCHMARK(bench_std_fib)->Arg(10'000);
BENCHMARK(bench_std_fib)->Arg(100'000'000);

#ifdef INCLUDE_ITER_GENERATOR_HPP

iter::generator<std::size_t> iter_fib_generator(size_t max) {
    std::size_t a = 0, b = 1;
    while (true) {
        a = std::exchange(b, b + a);
        if (a > max) co_return;
        co_yield b;
    }
}

auto iter_generator_fib(size_t max) {
    return iter_fib_generator(max) | iter::sum();
}

void bench_iter_generator_fib(benchmark::State& state)
{
    for (auto s : state) {
        auto sum = iter_generator_fib(state.range(0));
        benchmark::DoNotOptimize(sum > 10);
    }
}

BENCHMARK(bench_iter_generator_fib)->Arg(10'000);
BENCHMARK(bench_iter_generator_fib)->Arg(100'000'000);

#endif

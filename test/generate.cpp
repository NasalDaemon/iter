#include "test.hpp"

constexpr auto iter_fib_generate(size_t max) {
    return iter::generate {
        [=, a = 0ul, b = 1ul]() mutable {
            a = std::exchange(b, b + a);
            return a <= max ? std::optional(a) : std::nullopt;
        }
    };
}

constexpr auto iter_generate_fib(size_t max) {
    return iter_fib_generate(max) | iter::sum();
}


TEST(TestGenerate, sum) {
    ASSERT_EQ(iter_generate_fib(10), 20);
    static_assert(iter_generate_fib(10) == 20);
}

#ifndef ITER_TEST_TEST_HPP
#define ITER_TEST_TEST_HPP

#include "gtest/gtest.h"
#include "iter/iter.hpp"

/* Clang won't do NRVO with auto return type deduction due to a bug.
 * Where explicit return types don't make code too ugly, use them.
 * Otherwise, if it makes the code unreadible, let clang fail the test.
 * I have absolutely no intention of sullying this library with clang workarounds. */
#ifdef __clang__
#  define NO_CLANG(...) void()
#else
#  define NO_CLANG(...) __VA_ARGS__
#endif

using namespace iter;
using namespace xtd::literals;
namespace impl = iter::detail::impl;

template<class T>
struct ctor_count {
    T value;
    std::size_t copies = 0;
    std::size_t moves = 0;
    std::size_t total() const { return copies + moves; }

    auto operator<=>(ctor_count const&) const = default;

    constexpr ctor_count() : value{}, copies{0}, moves{0} {}
    constexpr ctor_count(T&& value) : value{std::move(value)}, copies{0}, moves{0} {}
    constexpr ctor_count(T const& value) : value{value}, copies{0}, moves{0} {}

    constexpr ctor_count(ctor_count const& other)
        : value{other.value}
        , copies{other.copies + 1}
        , moves{other.moves}
    {}
    constexpr ctor_count& operator=(ctor_count const& other) {
        value = other.value;
        copies = other.copies + 1;
        return *this;
    }

    constexpr ctor_count(ctor_count&& other)
        : value{std::move(other.value)}
        , copies{other.copies}
        , moves{other.moves + 1}
    {}

    constexpr ctor_count& operator=(ctor_count&& other) {
        value = std::move(other.value);
        moves = other.moves + 1;
        return *this;
    }
};

template<class T>
ctor_count(T) -> ctor_count<T>;

constexpr auto counter_wrap = []<class T>(T&& in) {
    return ctor_count(FWD(in));
};
constexpr auto counter_unwrap = []<class T>(ctor_count<T> const& in) {
    return in.value;
};

#endif /* ITER_TEST_TEST_HPP */

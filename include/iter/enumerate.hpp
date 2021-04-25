#ifndef INCLUDE_ITER_ENUMERATE_HPP
#define INCLUDE_ITER_ENUMERATE_HPP

#include "iter/zip.hpp"
#include "iter/range.hpp"

ITER_DECLARE(enumerate)

template<iter::iterable I>
constexpr auto ITER_IMPL(enumerate) (I&& iterable) {
    return iter::zip((I&&)iterable, iter::indices);
}

#endif /* INCLUDE_ITER_ENUMERATE_HPP */

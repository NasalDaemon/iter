cmake_minimum_required(VERSION 3.0)

project(iter VERSION 0.1)

set(CMAKE_CXX_STANDARD 20)

include_directories(include extern/extend/include)

set(GNU_COMPILE_OPTIONS
  "-Wall" "-Wpedantic" "-Wextra"
  "-Werror"
  "-march=native"
  # "-fsanitize=address,undefined"
  "$<$<CONFIG:Release>:-Ofast>"
  "$<$<CONFIG:Debug>:-O0>"
  "$<$<CONFIG:Debug>:-g>"
  )

if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
  add_compile_options(
    ${GNU_COMPILE_OPTIONS}
    "-Wno-missing-braces"
    "-fcoroutines-ts"
    "-Wno-dollar-in-identifier-extension"
    "-stdlib=libstdc++"
    )
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  add_compile_options(
    ${GNU_COMPILE_OPTIONS}
    "-fcoroutines"
    "-ftemplate-backtrace-limit=0"
    "-fconcepts-diagnostics-depth=4"
    )
  add_link_options(
    # "-fsanitize=address,undefined"
  )
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
  # using Intel C++
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
  add_compile_options(
    "/std:c++latest"
    "$<$<CONFIG:Release>:/O2>"
    )
endif()

option(ITER_GLOBAL_INVOKER "Overload same xtd_invoke_main for all iter xtd functions" OFF)

if(ITER_GLOBAL_INVOKER)
  add_definitions(-DITER_GLOBAL_INVOKER)
endif()

add_library(iter STATIC
  example/empty.cpp
  )

add_executable(iter_example
  example/main.cpp
  )

find_package(benchmark REQUIRED)

add_executable(iter_bench
  bench/autovec.cpp
  bench/chain.cpp
  bench/fibonacci.cpp
  bench/filter.cpp
  bench/min.cpp
  bench/triples.cpp
  bench/main.cpp
  )

target_link_libraries(iter_bench
  benchmark::benchmark
  )

find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})

add_executable(iter_test
  test/all.cpp
  test/any.cpp
  test/chain.cpp
  test/chunks.cpp
  test/collect.cpp
  test/compound.cpp
  test/cycle.cpp
  test/empty.cpp
  test/enumerate.cpp
  test/filter_map.cpp
  test/filter.cpp
  test/find_linear.cpp
  test/find_map.cpp
  test/flatmap.cpp
  test/flatten.cpp
  test/fold.cpp
  test/generate.cpp
  test/generator.cpp
  test/last.cpp
  test/map_while.cpp
  test/max.cpp
  test/min.cpp
  test/move.cpp
  test/range.cpp
  test/reverse.cpp
  test/split.cpp
  test/sum.cpp
  test/window.cpp
  test/wrap.cpp
  test/zip_map.cpp
  )

target_link_libraries(iter_test
  ${GTEST_LIBRARIES} ${GTEST_MAIN_LIBRARIES} pthread
  )

# Enable pre-compiled headers when cmake supports it
if(${CMAKE_VERSION} VERSION_GREATER "3.16.0")
  target_precompile_headers(iter PUBLIC
    include/iter/dollar_macros/define.hpp
    include/iter.hpp
    )

  target_precompile_headers(iter_example REUSE_FROM iter)

  target_precompile_headers(iter_bench REUSE_FROM iter)

  target_precompile_headers(iter_test PRIVATE
    test/test.hpp
    )
endif()

enable_testing()

add_test(main iter_test)

#ifndef INCLUDE_ITER_ITER_HPP
#define INCLUDE_ITER_ITER_HPP

#include "iter/core.hpp"

// Iters
#include "iter/iters/to_iter.hpp"
#include "iter/iters/once.hpp"
#include "iter/iters/optional.hpp"
#include "iter/iters/range.hpp"
#include "iter/iters/generate.hpp"
#include "iter/iters/generator.hpp"
#include "iter/iters/compound.hpp"
#include "iter/iters/repeat.hpp"
#include "iter/iters/empty.hpp"

// Adapters
#include "iter/adapters/filter.hpp"
#include "iter/adapters/take.hpp"
#include "iter/adapters/take_while.hpp"
#include "iter/adapters/skip.hpp"
#include "iter/adapters/skip_while.hpp"
#include "iter/adapters/map.hpp"
#include "iter/adapters/map_while.hpp"
#include "iter/adapters/filter_map.hpp"
#include "iter/adapters/flatmap.hpp"
#include "iter/adapters/flatten.hpp"
#include "iter/adapters/zip.hpp"
#include "iter/adapters/zip_map.hpp"
#include "iter/adapters/enumerate.hpp"
#include "iter/adapters/enumerate_map.hpp"
#include "iter/adapters/cycle.hpp"
#include "iter/adapters/reverse.hpp"

#include "iter/adapters/chain.hpp"
#include "iter/adapters/chunks.hpp"
#include "iter/adapters/split.hpp"
#include "iter/adapters/window.hpp"
#include "iter/adapters/inspect.hpp"
#include "iter/adapters/to_pointer_iter.hpp"
#include "iter/adapters/move.hpp"
#include "iter/adapters/box.hpp"

// Consumers
#include "iter/consumers/foreach.hpp"
#include "iter/consumers/fold.hpp"
#include "iter/consumers/reduce.hpp"
#include "iter/consumers/sum.hpp"
#include "iter/consumers/product.hpp"

#include "iter/consumers/last.hpp"
#include "iter/consumers/nth.hpp"
#include "iter/consumers/max.hpp"
#include "iter/consumers/min.hpp"
#include "iter/consumers/find_linear.hpp"
#include "iter/consumers/find_map.hpp"

#include "iter/consumers/any.hpp"
#include "iter/consumers/all.hpp"

// Collectors
#include "iter/collectors/collect.hpp"
#include "iter/collectors/partition.hpp"
#include "iter/collectors/unzip.hpp"
#include "iter/collectors/sorted.hpp"

// Misc
#include "iter/comparison.hpp"

#endif /* INCLUDE_ITER_ITER_HPP */

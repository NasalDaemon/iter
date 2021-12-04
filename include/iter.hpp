#ifndef ITER_INCLUDE_ITER_HPP
#define ITER_INCLUDE_ITER_HPP

#include "iter/core.hpp"

// Iterables
#include "iter/to_iter.hpp"
#include "iter/once.hpp"
#include "iter/optional.hpp"
#include "iter/range.hpp"
#include "iter/generate.hpp"
#include "iter/generator.hpp"
#include "iter/compound.hpp"
#include "iter/repeat.hpp"
#include "iter/empty.hpp"

// Adaptors
#include "iter/filter.hpp"
#include "iter/take.hpp"
#include "iter/take_while.hpp"
#include "iter/skip.hpp"
#include "iter/skip_while.hpp"
#include "iter/map.hpp"
#include "iter/map_while.hpp"
#include "iter/filter_map.hpp"
#include "iter/flatmap.hpp"
#include "iter/flatten.hpp"
#include "iter/zip.hpp"
#include "iter/enumerate.hpp"
#include "iter/cycle.hpp"
#include "iter/reverse.hpp"

#include "iter/chain.hpp"
#include "iter/chunks.hpp"
#include "iter/window.hpp"
#include "iter/inspect.hpp"
#include "iter/to_pointer.hpp"
#include "iter/move.hpp"
#include "iter/box.hpp"

// Consumers
#include "iter/foreach.hpp"
#include "iter/fold.hpp"
#include "iter/reduce.hpp"
#include "iter/sum.hpp"
#include "iter/product.hpp"

#include "iter/last.hpp"
#include "iter/nth.hpp"
#include "iter/max.hpp"
#include "iter/min.hpp"
#include "iter/find_linear.hpp"
#include "iter/find_map.hpp"

#include "iter/any.hpp"
#include "iter/all.hpp"

// Collectors
#include "iter/collect.hpp"
#include "iter/partition.hpp"
#include "iter/unzip.hpp"
#include "iter/sorted.hpp"

// Misc
#include "iter/comparison.hpp"

// Must be last
#include "iter/wrap.hpp"

#endif /* ITER_INCLUDE_ITER_HPP */

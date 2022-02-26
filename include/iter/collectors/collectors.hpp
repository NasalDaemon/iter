#ifndef ITER_COLLECTORS_COLLECTORS_HPP
#define ITER_COLLECTORS_COLLECTORS_HPP

#include "iter/collectors/collect.hpp"
namespace iter::collectors { using iter::collect; }
#include "iter/collectors/partition.hpp"
namespace iter::collectors { using iter::partition; }
#include "iter/collectors/unzip.hpp"
namespace iter::collectors { using iter::unzip; using iter::unzip_; }
#include "iter/collectors/sorted.hpp"
namespace iter::collectors { using iter::sorted; using iter::sorted_; }

#endif /* ITER_COLLECTORS_COLLECTORS_HPP */

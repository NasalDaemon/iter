#ifndef ITER_ITERS_ITERS_HPP
#define ITER_ITERS_ITERS_HPP

#include "iter/iters/random_access_container_iter.hpp"

#include "iter/iters/span.hpp"
namespace iter::iters { using iter::span; }
#include "iter/iters/owning_iter.hpp"
namespace iter::iters { using iter::owning_iter; }
#include "iter/iters/once.hpp"
namespace iter::iters { using iter::once; }
#include "iter/iters/optional.hpp"
namespace iter::iters { using iter::optional; }
#include "iter/iters/range.hpp"
namespace iter::iters { using iter::range; }
#include "iter/iters/generate.hpp"
namespace iter::iters { using iter::generate; }
#include "iter/iters/generator.hpp"
#ifdef ITER_COROUTINE
namespace iter::iters { using iter::generator; }
#endif
#include "iter/iters/compound.hpp"
namespace iter::iters { using iter::compound; }
#include "iter/iters/repeat.hpp"
namespace iter::iters { using iter::repeat; }
#include "iter/iters/empty.hpp"
namespace iter::iters { using iter::empty; }

#endif /* ITER_ITERS_ITERS_HPP */

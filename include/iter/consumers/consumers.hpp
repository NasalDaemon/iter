#ifndef ITER_CONSUMERS_CONSUMERS_HPP
#define ITER_CONSUMERS_CONSUMERS_HPP

// Reducing
#include "iter/consumers/foreach.hpp"
namespace iter::consumers { using iter::foreach; }
#include "iter/consumers/fold.hpp"
namespace iter::consumers { using iter::fold; }
#include "iter/consumers/reduce.hpp"
namespace iter::consumers { using iter::reduce; }
#include "iter/consumers/sum.hpp"
namespace iter::consumers { using iter::sum; }
#include "iter/consumers/product.hpp"
namespace iter::consumers { using iter::product; }

// Finding
#include "iter/consumers/last.hpp"
namespace iter::consumers { using iter::last; }
#include "iter/consumers/nth.hpp"
namespace iter::consumers { using iter::nth; }
#include "iter/consumers/max.hpp"
namespace iter::consumers { using iter::max; }
#include "iter/consumers/min.hpp"
namespace iter::consumers { using iter::min; }
#include "iter/consumers/find_linear.hpp"
namespace iter::consumers { using iter::find_linear; }
#include "iter/consumers/find_map.hpp"
namespace iter::consumers { using iter::find_map; }

// Predicates
#include "iter/consumers/any.hpp"
namespace iter::consumers { using iter::any; }
#include "iter/consumers/all.hpp"
namespace iter::consumers { using iter::all; }

#endif /* ITER_CONSUMERS_CONSUMERS_HPP */

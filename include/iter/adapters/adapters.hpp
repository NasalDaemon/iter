#ifndef ITER_ADAPTERS_ADAPTERS_HPP
#define ITER_ADAPTERS_ADAPTERS_HPP

#include "iter/adapters/box.hpp"
namespace iter::adapters { using iter::box; }
#include "iter/adapters/chain.hpp"
namespace iter::adapters { using iter::chain; }
#include "iter/adapters/chunks.hpp"
namespace iter::adapters { using iter::chunks; using iter::chunks_; }
#include "iter/adapters/chunk_by.hpp"
namespace iter::adapters { using iter::chunk_by; }
#include "iter/adapters/consteval_only.hpp"
namespace iter::adapters { using iter::consteval_only; }
#include "iter/adapters/cycle.hpp"
namespace iter::adapters { using iter::cycle; }
#include "iter/adapters/elements.hpp"
namespace iter::adapters { using iter::elements; using iter::keys; using iter::values; }
#include "iter/adapters/enumerate_map.hpp"
namespace iter::adapters { using iter::enumerate_map; using iter::enumerate_map_; }
#include "iter/adapters/enumerate.hpp"
namespace iter::adapters { using iter::enumerate; using iter::enumerate_; }
#include "iter/adapters/filter_map.hpp"
namespace iter::adapters { using iter::filter_map; }
#include "iter/adapters/filter.hpp"
namespace iter::adapters { using iter::filter; }
#include "iter/adapters/flatmap.hpp"
namespace iter::adapters { using iter::flatmap; }
#include "iter/adapters/flatten.hpp"
namespace iter::adapters { using iter::flatten; }
#include "iter/adapters/inspect.hpp"
namespace iter::adapters { using iter::inspect; }
#include "iter/adapters/map_while.hpp"
namespace iter::adapters { using iter::map_while; }
#include "iter/adapters/map.hpp"
namespace iter::adapters { using iter::map; }
#include "iter/adapters/move.hpp"
namespace iter::adapters { using iter::move; }
#include "iter/adapters/reverse.hpp"
namespace iter::adapters { using iter::reverse; }
#include "iter/adapters/skip_eager.hpp"
namespace iter::adapters { using iter::skip_eager; }
#include "iter/adapters/skip_while.hpp"
namespace iter::adapters { using iter::skip_while; }
#include "iter/adapters/skip.hpp"
namespace iter::adapters { using iter::skip; }
#include "iter/adapters/split.hpp"
namespace iter::adapters { using iter::split; }
#include "iter/adapters/take_while.hpp"
namespace iter::adapters { using iter::take_while; }
#include "iter/adapters/take.hpp"
namespace iter::adapters { using iter::take; }
#include "iter/adapters/to_pointer_iter.hpp"
namespace iter::adapters { using iter::to_pointer_iter; }
#include "iter/adapters/window.hpp"
namespace iter::adapters { using iter::window; }
#include "iter/adapters/zip_map.hpp"
namespace iter::adapters { using iter::zip_map; }
#include "iter/adapters/zip.hpp"
namespace iter::adapters { using iter::zip; }

#endif /* ITER_ADAPTERS_ADAPTERS_HPP */

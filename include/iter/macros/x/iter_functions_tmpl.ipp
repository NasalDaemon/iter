/* Do not modify, generated by scripts/update_x_macros.sh */

// Invoke iter::chunks_ on this iter
ITER_X(chunks_, (std::size_t N = 0), (N))
// Invoke iter::elements on this iter
ITER_X(elements, (std::size_t N), (N))
// Invoke iter::enumerate_map_ on this iter
ITER_X(enumerate_map_, (class T = std::size_t), (T))
// Invoke iter::enumerate_ on this iter
ITER_X(enumerate_, (class T = std::size_t), (T))
// Invoke iter::window on this iter
ITER_X(window, (std::size_t N = 2), (N))
// Invoke iter::collect on this iter
ITER_X(collect, (template<class...> class C = std::vector, template<class> class A = std::allocator, template<class> class... Traits), (C, A, Traits...))
// Invoke iter::unzip_ on this iter
ITER_X(unzip_, (template<class...> class C = std::vector, template<class> class A = std::allocator), (C, A))
// Invoke iter::sorted_ on this iter
ITER_X(sorted_, (template<class...> class C = std::vector, template<class> class A = std::allocator), (C, A))

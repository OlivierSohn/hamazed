#include <cstdlib>
#include <stdio.h>
#include <new>

#include "cpp.algorithms/include/public.h"

#ifdef __cplusplus

#  ifdef IMJ_LOG_MEMORY
template <typename Log>
void mayLog(Log l) {
  static int depth = 0;
  ++depth;
  if(1==depth) {
    l();
    // uncomment to see where the allocation / deallocation comes from.
    //imajuscule::logStack();
  }
  else {
    // don't log allocations / deallocations due to the memory log itself.
  }
  --depth;
}
void* operator new  ( std::size_t count ) {
  auto ptr = std::malloc(count);

  mayLog([&](){ printf("+++ %p, size = %zu\n",ptr, count); });

  return ptr;
}
void* operator new  ( std::size_t count, std::align_val_t al) {

  // not available on osx:
  // auto ptr = aligned_alloc(count, al);

  void*ptr;
  if (int rc = posix_memalign(&ptr, static_cast<std::underlying_type_t<std::align_val_t>>(al), count)) {
      ptr = nullptr;
  }

  mayLog([&](){ printf("+++ %p, size = %zu, alignment = %lu\n",ptr, count, al); });

  return ptr;
}
void operator delete(void* ptr) noexcept
{
  mayLog([&](){ printf("--- %p\n", ptr); });

  std::free(ptr);
}
void operator delete(void* ptr, std::align_val_t al) noexcept
{
  mayLog([&](){ printf("--- %p, alignment %lu\n", ptr, al); });

  std::free(ptr);
}
#  endif // IMJ_LOG_MEMORY

#endif // __cplusplus

extern "C" {

  void * imj_c_malloc(size_t count) {
      auto ptr = malloc(count);
#ifdef IMJ_LOG_MEMORY
      mayLog([&](){ printf("c +++ %p, size = %zu\n",ptr, count); });
#endif
      return ptr;
  }

  void imj_c_free(void*ptr) {
#ifdef IMJ_LOG_MEMORY
      mayLog([&](){ printf("c --- %p\n", ptr); });
#endif
      free(ptr);
  }
}

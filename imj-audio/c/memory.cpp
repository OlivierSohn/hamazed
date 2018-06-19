#include <cstdlib>
#include <stdio.h>
#include <new>

#include "cpp.algorithms/include/public.h"

#ifdef __cplusplus

#ifdef IMJ_LOG_MEMORY
void* operator new  ( std::size_t count ) {
  auto ptr = std::malloc(count);

  // we keep track of recursion depth to call logStack only at the first level.
  static int depth = 0;
  ++depth;
  if(1==depth) {
    using namespace imajuscule;
    LG(INFO,"+++ %p, size = %zu\n",ptr, count);
    //logStack();
  }
  --depth;
  return ptr;
}
#endif

#ifdef IMJ_LOG_MEMORY
void* operator new  ( std::size_t count, std::align_val_t al) {

  // not available on osx:
  // auto ptr = aligned_alloc(count, al);

  void*ptr;
  if (int rc = posix_memalign(&ptr, static_cast<std::underlying_type_t<std::align_val_t>>(al), count)) {
      ptr = nullptr;
  }

  printf("+++ %p, size = %zu, alignment = %lu\n",ptr, count, al);
  return ptr;
}
#endif


#ifdef IMJ_LOG_MEMORY
void operator delete(void* ptr) noexcept
{
    printf("--- %p\n", ptr);
    std::free(ptr);
}
#endif

#ifdef IMJ_LOG_MEMORY
void operator delete(void* ptr, std::align_val_t al) noexcept
{
    printf("--- %p, alignment %lu\n", ptr, al);
    std::free(ptr);
}
#endif

#endif

extern "C" {

  void * imj_c_malloc(size_t count) {
      auto ptr = malloc(count);
#ifdef IMJ_LOG_MEMORY
      printf("+++ %p, size = %zu\n",ptr, count);
#endif
      return ptr;
  }

  void imj_c_free(void*ptr) {
#ifdef IMJ_LOG_MEMORY
      printf("--- %p\n", ptr);
#endif
      free(ptr);
  }
}

#include <cstdlib>

#include "cpp.algorithms/include/public.h"

extern "C" {

  void * imj_c_malloc(size_t count) {
      auto ptr = malloc(count);
#ifdef IMJ_LOG_MEMORY
      imajuscule::logMemory([=](){ printf("c +++ %p, size = %zu\n",ptr, count); });
#endif
      return ptr;
  }

  void imj_c_free(void*ptr) {
#ifdef IMJ_LOG_MEMORY
      imajuscule::logMemory([=](){ printf("c --- %p\n", ptr); });
#endif
      free(ptr);
  }
}

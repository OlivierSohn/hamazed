#include <cstdlib>
#include <stdio.h>
#include <new>

#include "cpp.algorithms/include/public.h"

#ifdef __cplusplus

#  ifdef IMJ_LOG_MEMORY
template <typename Log>
void mayLog(Log l) {
  using namespace imajuscule;
  if(!shouldLogMemory()) {
    return;
  }
  ScopedNoMemoryLogs s; // do not log the memory allocated / deallocated for the log itself.

  if(threadNature() == ThreadNature::RealTime_OS) {
    printf("** dynamic memory allocated / deallocated in a realtime thread, outside program reach:\n");
  }
  else if(threadNature() == ThreadNature::RealTime_Program) {
    printf("******* Warning: dynamic memory allocated / deallocated in a realtime thread: *********\n");
    logStack();
  }
  l();
  if(threadNature() == ThreadNature::RealTime_Program) {
    printf("******* ******* ********\n");
  }
}
void* operator new  ( std::size_t count ) {
  auto ptr = std::malloc(count);

  mayLog([=](){ printf("+++ %p, size = %zu\n",ptr, count); });

  return ptr;
}
void* operator new  ( std::size_t count, std::align_val_t al) {

  // not available on osx:
  // auto ptr = aligned_alloc(count, al);

  void*ptr;
  if (int rc = posix_memalign(&ptr, static_cast<std::underlying_type_t<std::align_val_t>>(al), count)) {
      ptr = nullptr;
  }

  int c = count;
  int ali = static_cast<std::underlying_type_t<decltype(al)>>(al);
  mayLog([ptr, c, ali](){ printf("+++ %p, size = %d, alignment = %d\n",ptr, c, ali); });

  return ptr;
}
void operator delete(void* ptr) noexcept
{
  mayLog([=](){ printf("--- %p\n", ptr); });

  std::free(ptr);
}
void operator delete(void* ptr, std::align_val_t al) noexcept
{
  mayLog([=](){ printf("--- %p, alignment %lu\n", ptr, al); });

  std::free(ptr);
}
#  endif // IMJ_LOG_MEMORY

#endif // __cplusplus

extern "C" {

  void * imj_c_malloc(size_t count) {
      auto ptr = malloc(count);
#ifdef IMJ_LOG_MEMORY
      mayLog([=](){ printf("c +++ %p, size = %zu\n",ptr, count); });
#endif
      return ptr;
  }

  void imj_c_free(void*ptr) {
#ifdef IMJ_LOG_MEMORY
      mayLog([=](){ printf("c --- %p\n", ptr); });
#endif
      free(ptr);
  }
}

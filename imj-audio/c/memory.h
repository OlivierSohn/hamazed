#include <cstdlib>

extern "C" {
    void * imj_c_malloc(size_t count);
    void imj_c_free(void*ptr);
}

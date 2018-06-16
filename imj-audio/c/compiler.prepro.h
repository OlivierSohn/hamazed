//
// print compiler versions at compile time
//


// if version of visualc++ is needed, use this:
// https://github.com/boostorg/predef/blob/develop/include/boost/predef/compiler/visualc.h


#define COMPILER_VERSION0(compiler,a,b,c) "compiled with " compiler " " #a "." #b "." #c
#define COMPILER_VERSION(compiler,a,b,c) COMPILER_VERSION0(compiler,a,b,c)


#ifdef __clang_major__
#  pragma message (COMPILER_VERSION("clang",__clang_major__,__clang_minor__,__clang_patchlevel__))
#endif

#ifdef __GNUC__
    // note that clang 3.7 declares itself as a gcc 4.2"
#  pragma message (COMPILER_VERSION("gcc",__GNUC__,__GNUC_MINOR__,__GNUC_PATCHLEVEL__))
#endif

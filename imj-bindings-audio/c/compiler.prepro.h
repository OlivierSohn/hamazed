//
// print compiler versions at compile time
//


// if version of visualc++ is needed, use this:
// https://github.com/boostorg/predef/blob/develop/include/boost/predef/compiler/visualc.h

#ifdef __clang_major__
#  pragma message ("compiled with clang " __clang_major__ "." __clang_minor__"."__clang_patchlevel__);
#endif

#ifdef __GNUC__
    // note that clang 3.7 declares itself as a gcc 4.2"
#  pragma message ("compiled with gcc " __GNUC__ "." __GNUC_MINOR__"."__GNUC_PATCHLEVEL__);
#endif

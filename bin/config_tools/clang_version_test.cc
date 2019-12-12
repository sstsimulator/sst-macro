#include <clang/Basic/Version.h>

#if 0
#ifdef __apple_build_version__
# if CLANG_VERSION_MAJOR >= 7
#   error Apple Clang cannot be used with Clang libtooling >= 7
# endif
#endif
#endif

#ifdef __clang_major__

# if __clang_major__ <=6
#   if CLANG_VERSION_MAJOR >= 7
#     error LLVM/Clang compiler <= 6 cannot be used with Clang Libtooling >= 7
#   endif
# endif

# if __clang_major__ >= 7
#   if CLANG_VERSION_MAJOR <= 6
#     ifndef __apple_build_version__
#       error LLVM/Clang compiler >= 7 cannot be used with Clang Libtooling <= 6
#     endif
#   endif
# endif

#if 0
# if CLANG_VERSION_MAJOR >= 9
#   error LLVM/Clang compiler >= 9 cannot be used with sstmac
# endif
#endif

#endif

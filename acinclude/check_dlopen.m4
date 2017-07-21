
AC_DEFUN([CHECK_DLOPEN], [

AC_LANG_PUSH([C])

AC_CHECK_LIB([dl], [dlopen], [],
  [AC_MSG_ERROR([Could not locate libdl for dynamic library loading])])

AC_CHECK_HEADERS([dlfcn.h], [],
      [AC_MSG_ERROR([Could not locate dlfcn.h for dynaimc library loading])])

AC_LANG_POP([C])

])


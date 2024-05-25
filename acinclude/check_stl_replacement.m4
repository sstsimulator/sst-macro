
AC_DEFUN([CHECK_STL_REPLACEMENT_HEADERS], [

#no matter what, we need the sys type header
AC_SUBST([SYS_TYPES_FULL_PATH], "`$pyexe $srcdir/bin/config_tools/get_include_path sys/types.h $CXX $CPPFLAGS $CXXFLAGS`")
AC_CONFIG_FILES([
 sstmac/libraries/pthread/sstmac_sys_types.h
])
enable_repl_headers=yes

AM_CONDITIONAL([USE_REPLACEMENT_HEADERS], true)
AC_SUBST([PTHREAD_MACRO_DEF_MUTEX_INITIALIZER], "`$pyexe $srcdir/bin/config_tools/get_pthread_vars $CC mutex`")
AC_SUBST([PTHREAD_MACRO_DEF_COND_INITIALIZER], "`$pyexe $srcdir/bin/config_tools/get_pthread_vars $CC cond`")
AC_SUBST([PTHREAD_MACRO_DEF_ONCE_INITIALIZER], "`$pyexe $srcdir/bin/config_tools/get_pthread_vars $CC once`")
AC_CONFIG_FILES([
  sstmac/replacements/sstmac_pthread_clear.h
])

])

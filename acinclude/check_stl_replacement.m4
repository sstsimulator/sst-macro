
AC_DEFUN([CHECK_STL_REPLACEMENT_HEADERS], [
#-----------------------------
# Figure out the direct locations of all the STL header files
# These are needed for building fake headers
#-----------------------------
AC_ARG_ENABLE(replacement-headers,
  [AS_HELP_STRING(
    [--(dis|en)able-replacement-headers],
    [Control whether replacement headers are installed for pthreads, etc]
    )],
  [
    enable_repl_headers=$enableval
  ], [
    enable_repl_headers=yes
  ]
)

#no matter what, we need the sys type header
AC_SUBST([SYS_TYPES_FULL_PATH], "`$srcdir/bin/get_include_path $CXX sys/types.h $CPPFLAGS $CXXFLAGS`")
AC_CONFIG_FILES([
 sstmac/libraries/pthread/sstmac_sys_types.h
])

if test "X$enable_repl_headers" = "Xyes"; then
    AM_CONDITIONAL([USE_REPLACEMENT_HEADERS], true)
    AC_SUBST([PTHREAD_MACRO_DEF_MUTEX_INITIALIZER], "`$srcdir/bin/get_pthread_mutex_vars $CC`")
    AC_SUBST([PTHREAD_MACRO_DEF_COND_INITIALIZER], "`$srcdir/bin/get_pthread_cond_vars $CC`")
    AC_SUBST([STL_HEADER_FSTREAM_FULL_PATH], "`$srcdir/bin/get_include_path $CXX fstream $CPPFLAGS $CXXFLAGS`")
    AC_SUBST([STL_HEADER_ISTREAM_FULL_PATH], "`$srcdir/bin/get_include_path $CXX istream $CPPFLAGS $CXXFLAGS`")
    AC_SUBST([STL_HEADER_IOSTREAM_FULL_PATH], "`$srcdir/bin/get_include_path $CXX iostream $CPPFLAGS $CXXFLAGS`")
    AC_SUBST([STL_HEADER_LIST_FULL_PATH], "`$srcdir/bin/get_include_path $CXX list $CPPFLAGS $CXXFLAGS`")
    AC_SUBST([STL_HEADER_MAP_FULL_PATH], "`$srcdir/bin/get_include_path $CXX map $CPPFLAGS $CXXFLAGS`")
    AC_SUBST([STL_HEADER_MEMORY_FULL_PATH], "`$srcdir/bin/get_include_path $CXX memory $CPPFLAGS $CXXFLAGS`")
    AC_SUBST([STL_HEADER_PTHREAD_FULL_PATH], "`$srcdir/bin/get_include_path $CXX pthread.h $CPPFLAGS $CXXFLAGS`")
    AC_SUBST([STL_HEADER_QUEUE_FULL_PATH], "`$srcdir/bin/get_include_path $CXX queue $CPPFLAGS $CXXFLAGS`")
    AC_SUBST([STL_HEADER_SSTREAM_FULL_PATH], "`$srcdir/bin/get_include_path $CXX sstream $CPPFLAGS $CXXFLAGS`")
    AC_SUBST([STL_HEADER_STACK_FULL_PATH], "`$srcdir/bin/get_include_path $CXX stack $CPPFLAGS $CXXFLAGS`")
    AC_SUBST([STL_HEADER_STRING_FULL_PATH], "`$srcdir/bin/get_include_path $CXX string $CPPFLAGS $CXXFLAGS`")
    AC_SUBST([STL_HEADER_VECTOR_FULL_PATH], "`$srcdir/bin/get_include_path $CXX vector $CPPFLAGS $CXXFLAGS`")
    AC_SUBST([SYS_SIGNAL_FULL_PATH], "`$srcdir/bin/get_include_path $CXX sys/signal.h $CPPFLAGS $CXXFLAGS`")
    AC_SUBST([SYS_TIME_FULL_PATH], "`$srcdir/bin/get_include_path $CXX sys/time.h $CPPFLAGS $CXXFLAGS`")
    AC_SUBST([SIGNAL_FULL_PATH], "`$srcdir/bin/get_include_path $CXX signal.h $CPPFLAGS $CXXFLAGS`")
    AC_CONFIG_FILES([
      sstmac/replacements/sstmac_pthread_clear.h
      sstmac/replacements/sstmac_pthread_return.h
      sstmac/replacements/fstream
      sstmac/replacements/istream
      sstmac/replacements/iostream
      sstmac/replacements/list
      sstmac/replacements/map
      sstmac/replacements/memory
      sstmac/replacements/pthread.h
      sstmac/replacements/queue
      sstmac/replacements/sstream
      sstmac/replacements/stack
      sstmac/replacements/string
      sstmac/replacements/vector
      sstmac/replacements/sys/signal.h
      sstmac/replacements/sys/time.h
      sstmac/replacements/signal.h
      sstmac/replacements/sched.h
    ])
else
    AM_CONDITIONAL([USE_REPLACEMENT_HEADERS], false)
fi

])


AC_DEFUN([CHECK_STL_REPLACEMENT_HEADERS], [

#no matter what, we need the sys type header
AC_SUBST([SYS_TYPES_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX sys/types.h $CPPFLAGS $CXXFLAGS`")
AC_CONFIG_FILES([
 sstmac/libraries/pthread/sstmac_sys_types.h
])
enable_repl_headers=yes

AM_CONDITIONAL([USE_REPLACEMENT_HEADERS], true)
AC_SUBST([PTHREAD_MACRO_DEF_MUTEX_INITIALIZER], "`$srcdir/bin/config_tools/get_pthread_vars $CC mutex`")
AC_SUBST([PTHREAD_MACRO_DEF_COND_INITIALIZER], "`$srcdir/bin/config_tools/get_pthread_vars $CC cond`")
AC_SUBST([PTHREAD_MACRO_DEF_ONCE_INITIALIZER], "`$srcdir/bin/config_tools/get_pthread_vars $CC once`")
AC_SUBST([STL_HEADER_FSTREAM_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX fstream $CPPFLAGS $CXXFLAGS`")
AC_SUBST([STL_HEADER_ISTREAM_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX istream $CPPFLAGS $CXXFLAGS`")
AC_SUBST([STL_HEADER_IOSTREAM_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX iostream $CPPFLAGS $CXXFLAGS`")
AC_SUBST([STL_HEADER_OSTREAM_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX ostream $CPPFLAGS $CXXFLAGS`")
AC_SUBST([STL_HEADER_IOS_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX ios $CPPFLAGS $CXXFLAGS`")
AC_SUBST([STL_HEADER_THREAD_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX thread $CPPFLAGS $CXXFLAGS`")
AC_SUBST([STL_HEADER_MUTEX_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX mutex $CPPFLAGS $CXXFLAGS`")
AC_SUBST([STL_HEADER_COND_VAR_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX condition_variable $CPPFLAGS $CXXFLAGS`")
AC_SUBST([STL_HEADER_LIST_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX list $CPPFLAGS $CXXFLAGS`")
AC_SUBST([STL_HEADER_MAP_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX map $CPPFLAGS $CXXFLAGS`")
AC_SUBST([STL_HEADER_MEMORY_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX memory $CPPFLAGS $CXXFLAGS`")
AC_SUBST([STL_HEADER_PTHREAD_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX pthread.h $CPPFLAGS $CXXFLAGS`")
AC_SUBST([STL_HEADER_QUEUE_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX queue $CPPFLAGS $CXXFLAGS`")
AC_SUBST([STL_HEADER_SSTREAM_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX sstream $CPPFLAGS $CXXFLAGS`")
AC_SUBST([STL_HEADER_STACK_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX stack $CPPFLAGS $CXXFLAGS`")
AC_SUBST([STL_HEADER_STRING_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX string $CPPFLAGS $CXXFLAGS`")
AC_SUBST([STL_HEADER_VECTOR_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX vector $CPPFLAGS $CXXFLAGS`")
AC_SUBST([SYS_SIGNAL_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX sys/signal.h $CPPFLAGS $CXXFLAGS`")
AC_SUBST([SYS_TIME_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX sys/time.h $CPPFLAGS $CXXFLAGS`")
AC_SUBST([SIGNAL_FULL_PATH], "`$srcdir/bin/config_tools/get_include_path $CXX signal.h $CPPFLAGS $CXXFLAGS`")
AC_CONFIG_FILES([
  sstmac/replacements/sstmac_pthread_clear.h
  sstmac/replacements/fstream
  sstmac/replacements/istream
  sstmac/replacements/iostream
  sstmac/replacements/ostream
  sstmac/replacements/ios
  sstmac/replacements/list
  sstmac/replacements/map
  sstmac/replacements/memory
  sstmac/replacements/mutex
  sstmac/replacements/condition_variable
  sstmac/replacements/thread
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

])

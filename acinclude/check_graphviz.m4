
AC_DEFUN([CHECK_BACKTRACE_VIZ], [
# Graphviz
AC_CHECK_HEADERS([execinfo.h],
  HAVE_EXECINFO=yes,
  HAVE_EXECINFO=no
  AC_MSG_RESULT([No execinfo.h found.  Graphviz utility will not work.])
)
AC_CHECK_HEADERS([dlfcn.h],
  [HAVE_DLFCN=yes;
   LIBS="-ldl $LIBS"],
  HAVE_DLFCN=no
  AC_MSG_RESULT([No dlfcn.h found.  Graphviz utility will not work.])
)
AC_ARG_ENABLE(graphviz,
  [AS_HELP_STRING(
    [--(dis|en)able-graphviz],
    [Control whether or not the graphviz utility is able to generate call graphs],
    )],
  [
    enable_graphviz=$enableval
  ], [
    enable_graphviz=no
  ]
)
if test "X$enable_graphviz" = "Xyes"; then
  AC_DEFINE_UNQUOTED([HAVE_GRAPHVIZ], 1, "Graphviz utility is available for use")
  AM_CONDITIONAL([DO_GRAPHVIZ_TEST], true)
else
  AM_CONDITIONAL([DO_GRAPHVIZ_TEST], false)
  AC_DEFINE_UNQUOTED([HAVE_GRAPHVIZ], 0, "Graphviz utility is not available for use")
fi

# dot (from graphviz) is optional but can really help with doxygen documentation
AC_PATH_PROG([DOT], [dot])
if test -z "$DOT"; then
  HAVE_DOT=NO
else
  HAVE_DOT=YES
fi
AC_SUBST([HAVE_DOT])
])

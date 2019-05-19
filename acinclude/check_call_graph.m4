
AC_DEFUN([CHECK_CALL_GRAPH_VIZ], [
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
AC_ARG_ENABLE(call-graph,
  [AS_HELP_STRING(
    [--(dis|en)able-call-graph],
    [Control whether or not the graphviz utility is able to generate call graphs],
    )],
  [
    enable_call_graph=$enableval
  ], [
    enable_call_graph=no
  ]
)
if test "X$enable_call_graph" = "Xyes"; then
  AC_DEFINE_UNQUOTED([HAVE_CALL_GRAPH], 1, "Call graph utility is available for use")
  AM_CONDITIONAL([DO_CALL_GRAPH_TEST], true)
else
  AM_CONDITIONAL([DO_CALL_GRAPH_TEST], false)
  AC_DEFINE_UNQUOTED([HAVE_CALL_GRAPH], 0, "Call graph utility is not available for use")
fi

])



AC_DEFUN([CHECK_GNI], [

AC_ARG_ENABLE(gni,
  [AS_HELP_STRING(
    [--(dis|en)able-gni],
    [Whether to cognile GNI transport conduits [default=no]]
    )],
  [
    enable_gni=$enableval
  ], [
    enable_gni=no
  ]
)

if test "X$enable_gni" = "Xyes"; then
  AM_CONDITIONAL(ENABLE_GNI, true)
  AC_SUBST([rdma_header_file_include], ["<gni/rdma.h>"])
  AC_SUBST([default_transport], ["gni"])
else
  AM_CONDITIONAL(ENABLE_GNI, false)
fi

AC_CONFIG_FILES([gni/Makefile])

])




AC_DEFUN([CHECK_SST], [

AC_ARG_ENABLE(sst,
  [AS_HELP_STRING(
    [--(dis|en)able-sst],
    [Whether to compile SST transport conduits [default=no]]
    )],
  [
    enable_sst=$enableval
  ], [
    enable_sst=no
  ]
)

if test "X$enable_sst" = "Xyes"; then
  AC_SUBST([rdma_header_file_include], ["<sumi/rdma.h>"])
  AC_SUBST([default_transport], ["sst"])
fi

])




AC_DEFUN([CHECK_MPI], [

AC_ARG_ENABLE(mpi,
  [AS_HELP_STRING(
    [--(dis|en)able-mpi],
    [Whether to compile MPI transport conduits [default=no]]
    )],
  [
    enable_mpi=$enableval
  ], [
    enable_mpi=no
  ]
)

if test "X$enable_mpi" = "Xyes"; then
  AM_CONDITIONAL(ENABLE_MPI, true)
  AC_SUBST([rdma_header_file_include], ["<sumi/rdma.h>"])
  AC_SUBST([default_transport], ["mpi"])
else
  AM_CONDITIONAL(ENABLE_MPI, false)
fi

AC_CONFIG_FILES([mpi/Makefile])

])


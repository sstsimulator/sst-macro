

AC_DEFUN([CHECK_MPI_IMPLEMENTATION], [
AC_ARG_ENABLE([sumi-mpi],
  [AS_HELP_STRING([--enable-sumi-mpi],
    [enable alternative MPI implementation based on SUMI])],
  [with_sumi_mpi=$enableval],
  [with_sumi_mpi=no])

if test "X$with_sumi_mpi" = "Xyes"; then
  AM_CONDITIONAL([WITH_SUMI_MPI], true)
  AM_CONDITIONAL([WITH_DEFAULT_MPI], false)
  AC_SUBST([mpi_header_file], ["<sumi-mpi/mpi_wrapper.h>"])
else
  AM_CONDITIONAL([WITH_SUMI_MPI], false)
  AM_CONDITIONAL([WITH_DEFAULT_MPI], true)
  AC_SUBST([mpi_header_file], ["<sstmac/libraries/mpi/mpi_wrapper.h>"])
fi

])

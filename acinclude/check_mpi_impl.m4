

AC_DEFUN([CHECK_MPI_IMPLEMENTATION], [
AC_ARG_ENABLE([dharma-mpi],
  [AS_HELP_STRING([--enable-dharma-mpi],
    [enable alternative MPI implementation based on DHARMA])],
  [with_dharma_mpi=$enableval],
  [with_dharma_mpi=no])

if test "X$with_dharma_mpi" = "Xyes"; then
  AM_CONDITIONAL([WITH_DHARMA_MPI], true)
  AM_CONDITIONAL([WITH_DEFAULT_MPI], false)
  AC_SUBST([mpi_header_file], ["<dharma-mpi/mpi_wrapper.h>"])
else
  AM_CONDITIONAL([WITH_DHARMA_MPI], false)
  AM_CONDITIONAL([WITH_DEFAULT_MPI], true)
  AC_SUBST([mpi_header_file], ["<sstmac/libraries/mpi/mpi_wrapper.h>"])
fi

])

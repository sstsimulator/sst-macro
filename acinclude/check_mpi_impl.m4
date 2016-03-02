

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

AC_ARG_ENABLE([large-mpi-payloads],
  [AS_HELP_STRING([--enable-large-mpi-payloads],
    [allow MPI to send large real payloads - otherwise all MPI messages must be null or fake buffers])],
  [allow_large_mpi_payloads=$enableval],
  [allow_large_mpi_payloads=yes])

AH_TEMPLATE([ALLOW_LARGE_PAYLOADS],
            [Define to 1 to allow MPI to send large real payloads])
if test "X$allow_large_mpi_payloads" = "Xyes"; then
AC_DEFINE_UNQUOTED([ALLOW_LARGE_PAYLOADS], 1, [Allow MPI to simulate large real payloads])
else
AC_DEFINE_UNQUOTED([ALLOW_LARGE_PAYLOADS], 0, [Do NOT allow MPI to simulate large real payloads])
fi

])


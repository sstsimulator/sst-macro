

AC_DEFUN([CHECK_MPI_IMPLEMENTATION], [

AC_ARG_ENABLE([large-mpi-payloads],
  [AS_HELP_STRING([--(dis|en)able-large-mpi-payloads],
    [allow MPI to send large real payloads - otherwise all MPI messages must be null or fake buffers [default=yes]])],
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

AC_DEFUN([CHECK_SUMI_MPI], [

AC_ARG_ENABLE([sumi-mpi],
  [AS_HELP_STRING([--(dis|en)able-sumi-mpi],
    [install support for the SUMI implementation of MPI])],
  [install_sumi_mpi=$enableval],
  [install_sumi_mpi=yes])

AH_TEMPLATE([NO_SUMI_MPI],
            [Define to 1 to not build default SUMI MPI support])
if test "X$install_sumi_mpi" = "Xyes"; then
AC_DEFINE_UNQUOTED([NO_SUMI_MPI], 0, [Build SUMI MPI support])
AM_CONDITIONAL([BUILD_SUMI_MPI], true)
else
AC_DEFINE_UNQUOTED([NO_SUMI_MPI], 1, [Do NOT build SUMI MPI support])
AM_CONDITIONAL([BUILD_SUMI_MPI], false)
fi

])


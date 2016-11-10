

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


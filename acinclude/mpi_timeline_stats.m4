
AC_DEFUN([CHECK_MPI_TIMELINE_STATS], [

AC_ARG_ENABLE(mpi-timeline-stats,
  [AS_HELP_STRING(
    [--(dis|en)able-mpi-timeline-stats],
    [Whether to log times of the send/recv of all mpi messages sent and print the log at the end of the run [default=no]],
    )],
  [
    enable_mpi_timeline=$enableval
  ], [
    enable_mpi_timeline=no
  ]
)

AH_TEMPLATE([ENABLE_MPI_TIMELINE], [Define to compile all sanity checks])

if test "X$enable_mpi_timeline" = "Xno"; then
    AC_DEFINE_UNQUOTED([ENABLE_MPI_TIMELINE], 0, [Whether to enable tracking/printing of the complete MPI timeline])
else
    AC_DEFINE_UNQUOTED([ENABLE_MPI_TIMELINE], 1, [Whether to enable tracking/printing of the complete MPI timeline])
fi

])

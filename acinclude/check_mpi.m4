

AC_DEFUN([CHECK_MPI_PARALLEL], [


if test "X$have_mpi_header" = "Xyes"; then
  AC_CHECK_FUNCS([MPI_Init],
    AC_MSG_RESULT([yes])
    found_mpi=yes,
    found_mpi=no)
fi


AC_MSG_CHECKING([Checking for MPI])
if test "X$found_mpi" = "Xyes"; then
  AC_MSG_RESULT([yes])
  AC_DEFINE_UNQUOTED([HAVE_VALID_MPI], 1, "working MPI found")
  AC_DEFINE_UNQUOTED([DISTRIBUTED_MEMORY], 1, "Shared-memory optimizations off")
  AC_DEFINE_UNQUOTED([DEFAULT_ENV_STRING], "mpi", "Default to mpi environment")
  AC_DEFINE_UNQUOTED([DEFAULT_RUNTIME_STRING], "mpi", "Default to mpi runtime")
  AC_DEFINE_UNQUOTED([DEFAULT_PARTITION_STRING], "block", "Default to basic block partition")
  AC_DEFINE_UNQUOTED([DEFAULT_EVENT_MANAGER_STRING], "clock_cycle_parallel", "Default clock cycle parallelism")
  AM_CONDITIONAL([USE_MPIPARALLEL], true)
  with_mpiparallel=true
else
  AC_DEFINE_UNQUOTED([HAVE_VALID_MPI], 0, "working MPI not found")
  AC_MSG_RESULT([no])
  AC_DEFINE_UNQUOTED([DEFAULT_ENV_STRING], "serial", "Default to mpi environment")
  AC_DEFINE_UNQUOTED([DEFAULT_RUNTIME_STRING], "serial", "Default to mpi runtime")
  AC_DEFINE_UNQUOTED([DEFAULT_PARTITION_STRING], "serial", "Default to basic block partition")
  AC_DEFINE_UNQUOTED([DEFAULT_EVENT_MANAGER_STRING], "map", "Default clock cycle parallelism")
  AM_CONDITIONAL([USE_MPIPARALLEL], false)
  with_mpiparallel=false
fi

])


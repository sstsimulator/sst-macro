

AC_DEFUN([CHECK_MPI_PARALLEL], [
AC_ARG_ENABLE([mpiparallel],
  [AS_HELP_STRING([--enable-mpiparallel],
    [enable MPI-parallel PDES simulation])],
  [with_mpiparallel=$enableval],
  [with_mpiparallel=no])
AM_CONDITIONAL([USE_MPIPARALLEL], [test "X$with_mpiparallel" = "Xyes"])
AM_CONDITIONAL([USE_MPISWEEP], [test "X$with_mpiparallel" = "Xyes"])
if test "X$with_mpiparallel" = "Xyes"; then
  AC_CHECK_FUNCS([MPI_Init],[],[AC_MSG_ERROR([Problem with MPI. Are you using an MPI compiler?])])
  AC_DEFINE_UNQUOTED([DISTRIBUTED_MEMORY], 1, "Shared-memory optimizations off")
  AC_DEFINE_UNQUOTED([DEFAULT_ENV_STRING], "mpi", "Default to mpi environment")
  AC_DEFINE_UNQUOTED([DEFAULT_RUNTIME_STRING], "mpi", "Default to mpi runtime")
  AC_DEFINE_UNQUOTED([DEFAULT_PARTITION_STRING], "block", "Default to basic block partition")
  AC_DEFINE_UNQUOTED([DEFAULT_EVENT_MANAGER_STRING], "clock_cycle_parallel", "Default clock cycle parallelism")
else
  AC_DEFINE_UNQUOTED([DEFAULT_ENV_STRING], "serial", "Default to mpi environment")
  AC_DEFINE_UNQUOTED([DEFAULT_RUNTIME_STRING], "serial", "Default to mpi runtime")
  AC_DEFINE_UNQUOTED([DEFAULT_PARTITION_STRING], "serial", "Default to basic block partition")
  AC_DEFINE_UNQUOTED([DEFAULT_EVENT_MANAGER_STRING], "map", "Default clock cycle parallelism")
fi

AC_ARG_WITH([mpi-launcher],
  [AS_HELP_STRING([--with-mpi-launcher],
    [inform system of MPI launcher for testing purposes])],
  [mpilauncher=$withval],
  [mpilauncher=no])
if test "X$mpilauncher" = "Xno"; then
 AC_SUBST([launcher_test_args],[""])
else
 AC_SUBST([launcher_test_args],["$mpilauncher -n 2"])
fi
])

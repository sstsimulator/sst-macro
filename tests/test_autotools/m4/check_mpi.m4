AC_DEFUN([CHECK_MPI], [

AC_RUN_IFELSE(
[AC_LANG_PROGRAM(
[
  #include <mpi.h>
],[
  int hackc=0;
  char** hackv;
  MPI_Init(&hackc, &hackv);
  MPI_Finalize()
])],
[
AC_MSG_CHECKING([whether mpi is automatically usable])
AC_MSG_RESULT([yes])
enable_mpi="yes"
],[
AC_MSG_CHECKING([whether mpi is automatically usable])
AC_MSG_RESULT([no])
enable_mpi="no"
])

if test "$enable_mpi" = no; then
LIBSAVE="$LIBS"
LIBS="$LIBS -lmpi"
AC_RUN_IFELSE(
[AC_LANG_PROGRAM(
  [
    #include <mpi.h>
  ],[
    int hackc=0;
    char** hackv;
    MPI_Init(&hackc, &hackv);
    MPI_Finalize()
  ])],
[ 
  AC_MSG_CHECKING([whether mpi is usable])
  AC_MSG_RESULT([yes])
],[
  AC_MSG_CHECKING([whether mpi is usable])
  AC_MSG_RESULT([no])
  LIBS="$LIBSAVE"
  AC_MSG_ERROR([mpi tests failed])
])
fi

])

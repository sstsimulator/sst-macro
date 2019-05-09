AC_DEFUN([CHECK_THREADING], [

AC_LINK_IFELSE(
[AC_LANG_PROGRAM(
[
  #include <pthread.h>
],[
  pthread_create(0,0,0,0);
  pthread_join(0,0);
])],
[
AC_MSG_CHECKING([whether pthreads is automatically usable])
AC_MSG_RESULT([yes])
enable_pthread="yes"
],[
AC_MSG_CHECKING([whether pthreads is automatically usable])
AC_MSG_RESULT([no])
enable_pthread="no"
])

if test "$enable_pthread" = no; then
LIBSAVE="$LIBS"
LIBS="$LIBS -lpthread"
AC_LINK_IFELSE(
[AC_LANG_PROGRAM(
  [
    #include <pthread.h>
  ],[
    pthread_create(0,0,0,0);
    pthread_join(0,0);
  ])],
[ 
  AC_MSG_RESULT([yes])
],[
  AC_MSG_RESULT([no])
  LIBS="$LIBSAVE"
  AC_MSG_ERROR([pthreads tests failed])
])
fi

])

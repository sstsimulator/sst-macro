AC_DEFUN([CHECK_THREADING], [

AC_RUN_IFELSE(
[AC_LANG_PROGRAM(
[
  #include <pthread.h>
  void *func1(void* arg) { return 0; }
],[
  pthread_t thread1;
  pthread_create(&thread1,NULL,func1,NULL);
  pthread_join(thread1,NULL);
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
AC_RUN_IFELSE(
[AC_LANG_PROGRAM(
  [
    #include <pthread.h>
    void *func1(void* arg) { return 0; }
  ],[
  pthread_t thread1;
  pthread_create(&thread1,NULL,func1,NULL);
  pthread_join(thread1,NULL);
  ])],
[ 
  AC_MSG_CHECKING([whether pthreads is usable])
  AC_MSG_RESULT([yes])
],[
  AC_MSG_CHECKING([whether pthreads is usable])
  AC_MSG_RESULT([no])
  LIBS="$LIBSAVE"
  AC_MSG_ERROR([pthreads tests failed])
])
fi

])

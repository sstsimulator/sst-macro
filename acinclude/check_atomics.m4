
AC_DEFUN([CHECK_ATOMICS], [
AC_MSG_CHECKING([checking for atomic builtins])
AC_LINK_IFELSE(
  [AC_LANG_PROGRAM(
    [
    ],[
      int* nptr(0); __sync_fetch_and_add(nptr,0)
    ])],
  [
    AC_MSG_RESULT([yes])
    AM_CONDITIONAL([HAVE_ATOMIC_BUILTIN], true)
  ],[
    AC_MSG_RESULT([no])
    AM_CONDITIONAL([HAVE_ATOMIC_BUILTIN],false)
  ]
)
])


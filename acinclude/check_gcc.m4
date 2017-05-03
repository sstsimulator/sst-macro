

AC_DEFUN([CHECK_GCC], [

AC_MSG_CHECKING("Checking for GCC flags")
have_clang=`$srcdir/bin/config_tools/get_gcc $CXX`
AC_MSG_RESULT([$have_gcc])


if test "X$have_gcc" = "Xyes"; then
  AM_CONDITIONAL([HAVE_GCC], true)
else
  AM_CONDITIONAL([HAVE_GCC], false)
fi
])


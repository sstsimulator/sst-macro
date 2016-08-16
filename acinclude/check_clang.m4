

AC_DEFUN([CHECK_CLANG], [

AC_MSG_CHECKING("Checking for clang .so linker flags")
have_clang=`$srcdir/bin/get_clang $CXX`
AC_MSG_RESULT([$have_clang])


if test "X$have_clang" = "Xyes"; then
  AC_MSG_RESULT([clang sub])
  AC_SUBST([LD_SO_FLAGS], ["-shared -undefined dynamic_lookup"])
else
  AC_MSG_RESULT([gcc sub])
  AC_SUBST([LD_SO_FLAGS], ["-shared"])
fi

])


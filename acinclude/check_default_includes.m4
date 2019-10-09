
AC_DEFUN([CHECK_DEFAULT_INCLUDES], [

#no matter what, we need the sys type header
AC_MSG_CHECKING([default include paths for $CXX])
result=`$srcdir/bin/config_tools/get_default_includes $CXX $CXXFLAGS`
AC_SUBST([DEFAULT_INCLUDE_PATHS], [$result])
AC_MSG_RESULT([$result])

])


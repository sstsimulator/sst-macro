

AC_DEFUN([CHECK_CPP11], [

AH_TEMPLATE([HAVE_CPP11],
            [Define to use C++11 language features])

#make sure if needed C++11 flags are automatically configured
AX_CXX_COMPILE_STDCXX_11(noext, optional)

])

AC_DEFUN([CHECK_CPP_STANDARD], [
have_cpp14=`$srcdir/bin/config_tools/get_cpp_version $CXX $CXXFLAGS`
if test "Xhave_cpp14" = "Xyes"; then
AM_CONDITIONAL([HAVE_CPP14], true)
else
AM_CONDITIONAL([HAVE_CPP14], false)
fi
])


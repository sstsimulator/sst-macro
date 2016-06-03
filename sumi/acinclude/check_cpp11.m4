

AC_DEFUN([CHECK_CPP11], [

AC_ARG_ENABLE(cpp11,
  [AS_HELP_STRING(
    [--(dis|en)able-cpp11],
    [Control whether features requiring C++11 language features are compiled. Default is yes if available, otherwise no.]
    )],
  [
    enable_cpp11=$enableval
  ], [
    enable_cpp11=maybe
  ]
)

if test "X$enable_cpp11" = "Xyes"; then
  AX_CXX_COMPILE_STDCXX_11(noext, mandatory)
  cpp11_flag=requested
elif test "X$enable_cpp11" = "Xmaybe"; then 
  AX_CXX_COMPILE_STDCXX_11(noext, optional)
  if test "X$HAVE_CXX11" = "X1"; then
    enable_cpp11=yes
  else 
    enable_cpp11=no
  fi
  cpp11_flag=default
else
  enable_cpp11=no
  cpp11_flag=none
fi

if test "X$enable_cpp11" = "Xyes"; then
AC_CHECK_HEADERS([unordered_map],
  AC_MSG_CHECKING([C++11 headers])
  AC_MSG_RESULT([yes])
  have_cpp11_headers=yes,
  AC_MSG_CHECKING([C++11 headers])
  AC_MSG_RESULT([no])
  have_cpp11_headers=no
)
fi

if test "X$have_cpp11_headers" = "Xno"; then
if test "X$cpp11_flag" = "Xrequested"; then
  AC_MSG_ERROR([C++11 enabled, but could not locate header files
Requested C++11 compiler is not fully implemented
This can happen with older versions of Clang and GCC
Set --disable-cpp11 and reconfigure])
else
AC_MSG_RESULT([WARNING: could not locate all C++11 headers - not activating C++11 by default])
enable_cpp11=no
fi
fi


])


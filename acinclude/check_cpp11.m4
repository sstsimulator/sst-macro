

AC_DEFUN([CHECK_CPP11], [

AH_TEMPLATE([HAVE_CPP11],
            [Define to use C++11 language features])
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

#make sure if needed C++11 flags are automatically configured
if test "X$enable_cpp11" = "Xyes" || test "X$enable_cpp11" = "Xmaybe"; then
  AX_CXX_COMPILE_STDCXX_11(noext, optional)
fi

])


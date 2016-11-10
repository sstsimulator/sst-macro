

AC_DEFUN([CHECK_CPP11], [

AH_TEMPLATE([HAVE_CPP11],
            [Define to use C++11 language features])

AX_CXX_COMPILE_STDCXX_11(noext, mandatory)

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
  AC_MSG_ERROR([C++11 enabled, but could not locate header files
                Requested C++11 compiler is not fully implemented
                This can happen with older versions of Clang and GCC])
fi

AC_DEFINE_UNQUOTED([HAVE_CPP11], 1,
     [Whether to use C++11 language features])
AM_CONDITIONAL([HAVE_CPP11], true)

])


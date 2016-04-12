
AC_DEFUN([CHECK_PARENT_FLAGS], [
  AC_MSG_CHECKING([checking whether sumi should use CXXFLAGS from parent project])
  AC_ARG_WITH([cxxflags],
      AS_HELP_STRING([--with-cxxflags=CXXFLAGS],
          [Pass in a set of custom CXXFLAGS from parent project]
      ), [
        AC_MSG_RESULT([yes - $withval])
        with_parent_cxxflags="$withval"
      ], [
        AC_MSG_RESULT([no])
        with_parent_cxxflags="no"
      ]
  )
])


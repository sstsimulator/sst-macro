

AH_TEMPLATE([WARNING_CFLAGS], [Contains -Werror flags])
AH_TEMPLATE([WARNING_CXXFLAGS], [Contains -Werror flags])
AC_DEFUN([CHECK_WARNINGS], [

  AC_MSG_CHECKING([whether to use -Werror build])
  AC_ARG_WITH(warnings,
    [AS_HELP_STRING(
        [--with-warnings],
        [Whether to build with certain warning flags]
      )
    ],
    [
      add_warnings=$withval
    ], 
    [
      add_warnings=no
    ]
  )
  AC_MSG_RESULT([$add_warnings])

  if test "$add_warnings" != "no"; then
    if test "$add_warnings" = "yes"; then
      WARNING_CFLAGS="-Wall"
      WARNING_CXXFLAGS="-Wall"
    else
      WARNING_CFLAGS="$withval"
      WARNING_CXXFLAGS="$withval"
    fi
  else
    WARNING_CFLAGS=""
    WARNING_CXXFLAGS=""
  fi

  AC_SUBST(WARNING_CFLAGS)
  AC_SUBST(WARNING_CXXFLAGS)
])

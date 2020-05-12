

AH_TEMPLATE([ERROR_CFLAGS], [Contains -Werror flags])
AH_TEMPLATE([ERROR_CXXFLAGS], [Contains -Werror flags])
AC_DEFUN([CHECK_WERROR], [

  AC_MSG_CHECKING([whether to use -Werror build])
  AC_ARG_WITH(werror,
    [AS_HELP_STRING(
        [--with-werror],
        [Whether to build with Werror]
      )
    ],
    [
      add_werror=$withval
    ], 
    [
      add_werror=no
    ]
  )
  AC_MSG_RESULT([$add_werror])

  if test "$add_werror" != "no"; then
    ERROR_CFLAGS="-Werror"
    ERROR_CXXFLAGS="-Werror"
  else
    ERROR_CFLAGS=""
    ERROR_CXXFLAGS=""
  fi

  AC_SUBST(ERROR_CFLAGS)
  AC_SUBST(ERROR_CXXFLAGS)
])

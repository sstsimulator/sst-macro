
AC_DEFUN([CHECK_SDK], [

SAVE_CPPFLAGS="$CPPFLAGS" 

AC_ARG_WITH(sdk,
  [AS_HELP_STRING(
    [--with-sdk],
    [Mac SDK include location],
    )],
  [ enable_sdk=$withval ], 
  [ enable_sdk=no ]
)

if test "X$enable_sdk" = "Xyes"; then
	AC_MSG_ERROR([Must provide a path for the Mac SDK])
else
  if test "X$enable_sdk" != "Xno"; then
    MACSDK_CFLAGS="-isysroot $enable_sdk"
    MACSDK_CXXFLAGS="-isysroot $enable_sdk"
  fi
fi

CPPFLAGS="$MACSDK_CFLAGS"
#check header 
AC_CHECK_HEADER([map],
  [HAVE_MACSDK=yes],
  [HAVE_MACSDK=no]
)
CPPFLAGS="$SAVE_CPPFLAGS"

AM_CONDITIONAL([HAVE_MACSDK], [test "x$HAVE_MACSDK" = "xyes" -a "X$enable_sdk" != "X$no"])
AC_SUBST([MACSDK_CFLAGS])
AC_SUBST([MACSDK_CXXFLAGS])

])


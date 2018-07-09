
AC_DEFUN([CHECK_OTF2], [

SAVE_CPPFLAGS="$CPPFLAGS" 
SAVE_LDFLAGS="$LDFLAGS"

AC_ARG_ENABLE(otf2,
  [AS_HELP_STRING(
    [--(dis|en)able-otf2],
    [Enable otf2 supported trace replay],
    )],
  [ enable_otf2=$enableval ], 
  [ enable_otf2=maybe ]
)
SHOULD_HAVE_OTF2=no

#either $enableval is yes,no if yes, then system install of otf2
#if custom, folder specified then add to cppflags and ldflags 
if test "X$enable_otf2" = "Xyes"; then
  SHOULD_HAVE_OTF2=yes
  LDFLAGS="-lotf2"
else
  if test "X$enable_otf2" != "Xmaybe" -a "X$enable_otf2" != "Xno"; then
    SHOULD_HAVE_OTF2=yes
    LDFLAGS="-L$enable_otf2/lib -lotf2"
    CPPFLAGS="-I$enable_otf2/include"
  fi
fi

#check header 
AC_CHECK_HEADER([otf2/otf2.h],
  [HAVE_OTF2=yes],
  [HAVE_OTF2=no]
)

if test "X$SHOULD_HAVE_OTF2" = "Xyes" -a "X$HAVE_OTF2" != "Xyes"; then
  AC_MSG_ERROR([OTF2 libraries required by --enable-otf2 not found])
fi

if test "X$enable_otf2" = "Xmaybe"; then
  if test "X$HAVE_OTF2" = "Xyes"; then
    LDFLAGS="-lotf2"
    echo otf2 enabled.
  else
    echo otf2 disabled.
  fi
fi

AM_CONDITIONAL([HAVE_OTF2], [test "x$HAVE_OTF2" = "xyes" -a "X$enable_otf2" != "X$no"])

if test "x$HAVE_OTF2" = "xyes" -a "X$enable_otf2" != "X$no"; then
build_otf2=yes
AC_DEFINE([OTF2_ENABLED],,[Define OTF2 support as enabled])
else
build_otf2=no
fi

if test "X$enable_otf2" = "Xno"; then
  echo otf2 disabled by --disable-otf2
fi

#check lib - try this later
OTF2_CPPFLAGS=$CPPFLAGS
OTF2_LDFLAGS=$LDFLAGS

AC_SUBST([OTF2_CPPFLAGS])
AC_SUBST([OTF2_LDFLAGS])

CPPFLAGS="$SAVE_CPPFLAGS"
LDFLAGS="$SAVE_LDFLAGS" 

])

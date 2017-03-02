
AC_DEFUN([CHECK_OTF2], [

SAVE_CPPFLAGS="$CPPFLAGS" 
SAVE_LDFLAGS="$LDFLAGS"

AC_ARG_ENABLE(otf2,
  [AS_HELP_STRING(
    [--(dis|en)able-otf2],
    [Enable otf2 supported trace replay],
    )],
  [ enable_otf2=$enableval ], 
  [ enable_otf2=no ]
)

#either $enableval is yes,no if yes, then system install of otf2
#if custom, folder specified then add to cppflags and ldflags 
if test "X$enable_otf2" != "Xyes" -a "X$enable_otf2" != "Xno" ; then
  LDFLAGS="-L$enable_otf2/lib -lotf2"
  CPPFLAGS="-I$enable_otf2/include"
fi 

#check header 
AC_CHECK_HEADER([otf2/otf2.h],
  [HAVE_OTF2=yes
  AC_MSG_RESULT([otf2/otf2.h found. OTF2 replay enabled.])],
  [HAVE_OTF2=no
  AC_MSG_RESULT([No otf2/otf2.h found. OTF2 replay disabled.])]
)

AM_CONDITIONAL([HAVE_OTF2], [test "x$HAVE_OTF2" = "xyes" ])

#check lib - try this later
OTF2_CPPFLAGS=$CPPFLAGS
OTF2_LDFLAGS=$LDFLAGS

AC_SUBST([OTF2_CPPFLAGS])
AC_SUBST([OTF2_LDFLAGS])

CPPFLAGS="$SAVE_CPPFLAGS"
LDFLAGS="$SAVE_LDFLAGS" 

])


AC_DEFUN([CHECK_FORTRAN], [
# Fortran
AH_TEMPLATE(HAVE_FORTRAN,[Defined if Fortran support is configured])
AC_ARG_ENABLE([fortran],
  [AS_HELP_STRING([--enable-fortran],
    [enable fortran (experimental)])],
  [with_fortran=$enableval],
  [with_fortran=no])
if test "X$with_fortran" = "Xyes"; then
  AC_PROG_FC
  AC_FC_LIBRARY_LDFLAGS
  AC_DEFINE(HAVE_FORTRAN)
else
  FC=
fi
AM_CONDITIONAL(FC_ENABLE, test "X$FC" != "X")
])

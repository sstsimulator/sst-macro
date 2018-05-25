
AC_DEFUN([CHECK_VTK], [

SAVE_CPPFLAGS="$CPPFLAGS" 
SAVE_LDFLAGS="$LDFLAGS"

AC_ARG_ENABLE(vtk,
  [AS_HELP_STRING(
    [--(dis|en)able-vtk],
    [Enable VTK visualization],
    )],
  [ enable_vtk=$enableval ], 
  [ enable_vtk=no ]
)
AH_TEMPLATE([VTK_ENABLED], [Whether VTK is available and VTK stats should be built])
#either $enableval is yes,no if yes, then system install of otf2
#if custom, folder specified then add to cppflags and ldflags 
if test "X$enable_vtk" = "Xyes"; then
AC_MSG_ERROR([VTK can only be enabled by giving the install prefix --enable-vtk=PREFIX])
AM_CONDITIONAL([HAVE_VTK], [false])
else
if test "X$enable_vtk" = "Xno"; then
AM_CONDITIONAL([HAVE_VTK], [false])
else
AM_CONDITIONAL([HAVE_VTK], [true])
AC_DEFINE_UNQUOTED([VTK_ENABLED], 1, [VTK is enabled and installed])
VTK_CPPFLAGS=-I$enable_vtk/include
VTK_LIBS=-lvtk
VTK_LDFLAGS=-L$enable_vtk/lib
fi

fi

])


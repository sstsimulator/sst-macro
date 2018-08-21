
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
#AC_MSG_ERROR([VTK build with ${enable_vtk}/include/vtk-8.1])
VTK_CPPFLAGS="-I${enable_vtk}/include/vtk-8.1"
VTK_LIBS="-lvtkCommonCore-8.1.1 -lvtkCommonDataModel-8.1.1 -lvtkIOXML-8.1.1 -lvtkIOExodus-8.1.1"
VTK_LDFLAGS="-L${enable_vtk}/lib/ -lvtkCommonCore-8.1.1 -lvtkCommonDataModel-8.1.1 -lvtkIOXML-8.1.1 -lvtkIOExodus-8.1.1"
VTK_RPATH=${enable_vtk}/lib

LDFLAGS="$LDFLAGS $VTK_LDFLAGS"
AC_SUBST([VTK_CPPFLAGS])
AC_SUBST([VTK_RPATH])
#AC_SUBST([VTK_LIBS])
#AC_LIB_LINKFLAGS([VTK_LIBS])
#AC_SUBST([VTK_LDFLAGS])

fi

fi

])


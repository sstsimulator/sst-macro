
AC_DEFUN([CHECK_VTK], [
vtk_path="/usr/local"
AC_ARG_WITH([vtk-path], 
  [AS_HELP_STRING([--with-vtk-path], [Give the install root for VTK, default is /usr/local])],
  [
  vtk_path=$withval
  ]
)

# Check for VTK
AH_TEMPLATE(HAVE_VTK,[Defined if VTK library is installed])
AH_TEMPLATE(SSTMAC_VTK_VERSION, [Gives the version number of VTK])
AC_ARG_WITH([vtk], [AS_HELP_STRING([--with-vtk], [Compile for VTK. Version number must be given, e.g. --with-vtk=6.0])],
  [
   AM_CONDITIONAL([VTK_ENABLE], true)
   if test "X$withval" = "Xyes"; then
     AC_MSG_ERROR([--with-vtk flag requires version number to specified, e.g. --with-vtk=6.0])
   fi
   vtkversion=$withval 
   AS_VERSION_COMPARE([$vtkversion],[6.0],
    [AC_MSG_ERROR(Need VTK version >= 6.0. Only have $vtkversion)],
    [AC_DEFINE_UNQUOTED(SSTMAC_VTK_VERSION,60)],
    [AC_DEFINE_UNQUOTED(SSTMAC_VTK_VERSION,61)]
    )
   vtklibs="m4_foreach([thelib], [
     [-lvtkRenderingCore-$vtkversion],
     [-lvtkIOXML-$vtkversion],
     [-lvtkRenderingFreeType-$vtkversion],
     [-lvtkRenderingFreeTypeOpenGL-$vtkversion],
     [-lvtkInteractionStyle-$vtkversion],
     [-lvtkRenderingVolume-$vtkversion],
     [-lvtkRenderingOpenGL-$vtkversion],
     [-lvtkRenderingVolumeOpenGL-$vtkversion],
     [-lvtkCommonCore-$vtkversion],
     [-lvtkChartsCore-$vtkversion], 
     [-lvtkViewsContext2D-$vtkversion]], 
     [thelib ])"
   vtklibpath="${vtk_path}/lib"
   vtkincludepath="${vtk_path}/include/vtk-$vtkversion"
   AC_SUBST(VTK_LDFLAGS, "-L$vtklibpath $vtklibs")
   AC_SUBST(VTK_IFLAGS, "-I${vtk_path}/include/vtk-$vtkversion/")
   AC_SUBST(VTK_LIBS, "$vtklibs")

   CPPFLAGSAVE="$CPPFLAGS"
   CPPFLAGS="$CPPFLAGS -I$vtkincludepath"
   AC_MSG_CHECKING("with $CPPFLAGS")
   AC_CHECK_HEADERS(vtkRenderer.h, [], 
      AC_MSG_ERROR([Problem finding VTK headers in $vtkincludepath. Are you sure version $vtkversion is correct?]))
       
  AC_MSG_CHECKING([VTK libraries])
  LDSAVE="$LDFLAGS"
  LIBSAVE="$LIBS"
  CFLAGSAVE="$CFLAGS"
  CFLAGS="$CFLAGS -I$vtkincludepath"
  LIBS="$LIBS -lvtkRenderingCore-$vtkversion -lvtkCommonCore-$vtkversion -lvtkChartsCore-$vtkversion -lvtkViewsContext2D-$vtkversion"
  LDFLAGS="$LDFLAGS -L$vtklibpath"
  AC_LINK_IFELSE(
  [
    AC_LANG_PROGRAM(
        [ #include <vtkRenderer.h>],
        [ vtkRenderer::New() ]
      ) 
    ],
  [ AC_MSG_RESULT([yes]) ],
  [ 
    AC_MSG_RESULT([no])
      AC_MSG_ERROR([Problem finding VTK libraries in $vtklibpath. Are you sure version $vtkversion is correct?]) 
    ]
  )
  AC_DEFINE(HAVE_VTK)
  ], 
  [
   AM_CONDITIONAL([VTK_ENABLE], false)
   vtk_path=no
  ]
)
])

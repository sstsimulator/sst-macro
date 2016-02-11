
AC_DEFUN([CHECK_QT], [
AC_ARG_WITH([qt],
  [AS_HELP_STRING([--with-qt], [Enable compilation of Qt-dependent GUI utilities])],
  [
    AM_CONDITIONAL([HAVE_QT], true)
    if test "X$withval" = "Xyes"; then
      AC_SUBST(qmake_executable,[qmake])
    else
      AC_SUBST(qmake_executable,$withval)
    fi
    AC_SUBST(gui_config_file,"$MY_ABS_SRCPATH/qt-gui/config/allfiles.gui")
    AC_SUBST(gui_src_include_path,"$MY_ABS_SRCPATH/configurations")
    AC_SUBST(gui_install_include_path,"$MY_PREFIX/include/sstmac/configurations")
    with_qt=yes
  ],
  [
    AM_CONDITIONAL([HAVE_QT], false)
    AC_SUBST(qmake_executable,[])
    with_qt=no
    AC_SUBST(gui_config_file,[])
    AC_SUBST(gui_src_include_path,[])
    AC_SUBST(gui_install_include_path,[])
  ]
)
])

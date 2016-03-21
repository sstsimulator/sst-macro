
AC_DEFUN([CHECK_SST_CORE], [

have_integrated_core="no"
AC_ARG_WITH([integrated-core],
    AS_HELP_STRING([--with-integrated-core],
        [Build shared library compatible with integrated SST core]
    ), [
      AC_CONFIG_FILES([skeletons/sst/env.sh skeletons/sst/config.py])
      AC_CONFIG_FILES([skeletons/sst/run], [chmod +x skeletons/sst/run])
      AC_CONFIG_FILES([bin/sstmac], [chmod +x bin/sstmac])
      AC_CONFIG_FILES([bin/sstmac-check], [chmod +x bin/sstmac-check])
      AC_CONFIG_FILES([tests/api/mpi/testexec], [chmod +x tests/api/mpi/testexec])
      AC_CONFIG_FILES([tests/api/globals/testexec], [chmod +x tests/api/globals/testexec])
      AC_DEFINE_UNQUOTED([INTEGRATED_SST_CORE], 1, [Run on integrated SST core])
      AC_SUBST([sst_prefix], "$withval")
      AM_CONDITIONAL([INTEGRATED_SST_CORE], true)
      SST="$withval"
      SST_INCLUDES="-I$SST/include -I$SST/include/sst"  
      CPPFLAGS="$CPPFLAGS -DSSTMAC_INTEGRATED_SST_CORE=1 $SST_INCLUDES"
      PY_INCLUDES="`python-config --includes`"
      CPPFLAGS="$CPPFLAGS $PY_INCLUDES"
      AC_CHECK_HEADERS([Python.h], [], 
          [AC_MSG_ERROR([Could not locate Python installation needed by SST core])])
      AC_CHECK_HEADERS([sst/core/element.h], [],
          [AC_MSG_ERROR([Could not locate SST core header files at $SST])])
      have_integrated_core="yes"
      SUMI_CPPFLAGS="$SST_INCLUDES"
    ], [
      AC_DEFINE_UNQUOTED([INTEGRATED_SST_CORE], 0, [Do not run on integrated SST core])
      AM_CONDITIONAL([INTEGRATED_SST_CORE], false)
      SUMI_CPPFLAGS=""
    ]
)

have_core_boost="no"
AC_ARG_WITH([core-boost],
  AS_HELP_STRING([--with-core-boost],
      [Build shared library compatible with integrated SST core]
  ), [
    CPPFLAGS="$CPPFLAGS -I$withval/include"
    have_core_boost="yes"
    AC_CHECK_HEADERS([boost/version.hpp], [],
        [AC_MSG_ERROR([Could not locate Boost header files for SST core at $withval])])
    SUMI_CPPFLAGS="$SUMI_CPPFLAGS -I$withval/include"
  ], [
    have_core_boost="no"
  ]
)


if test "X$have_integrated_core" = "Xyes"; then
  if test "X$have_core_boost" = "Xno"; then
    AC_MSG_ERROR([Compiling for integrated core, but do not have Boost path. Please specify Boost used by SST core using --with-core-boost])
  fi
fi

])

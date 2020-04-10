
AC_DEFUN([CHECK_SST_CORE], [

have_integrated_core="no"
AC_ARG_WITH([sst-core],
    AS_HELP_STRING([--with-sst-core@<:@=DIR@:>@],
        [Build shared library compatible with integrated SST core (optional).]
    ), [
      SST="$withval"
      have_integrated_core="yes"
    ], [
      AC_DEFINE_UNQUOTED([INTEGRATED_SST_CORE], 0, [Do not run on integrated SST core])
      AM_CONDITIONAL([INTEGRATED_SST_CORE], false)
      have_integrated_core="no"
    ]
)

if test "X$have_integrated_core" = "Xyes"; then
  AC_CONFIG_FILES([bin/pysstmac], [chmod +x bin/pysstmac])
  AC_CONFIG_FILES([bin/sstmac-check], [chmod +x bin/sstmac-check])
  AC_CONFIG_FILES([tests/api/mpi/testexec], [chmod +x tests/api/mpi/testexec])
  AC_CONFIG_FILES([tests/api/globals/testexec], [chmod +x tests/api/globals/testexec])
  AC_DEFINE_UNQUOTED([INTEGRATED_SST_CORE], 1, [Run on integrated SST core])
  AC_SUBST([sst_prefix], "$SST")
  AM_CONDITIONAL([INTEGRATED_SST_CORE], true)
  SST_INCLUDES="-I$SST/include -I$SST/include/sst -I$SST/include/sst/core"
  SST_CPPFLAGS="-DSSTMAC_INTEGRATED_SST_CORE=1 $SST_INCLUDES -D__STDC_FORMAT_MACROS"
  SAVE_CPPFLAGS="$CPPFLAGS"
  PY_INCLUDES="`$pycfg --includes`"
  PY_LDFLAGS=`$pyexe $srcdir/bin/config_tools/get_py_ldflags`
  SST_CPPFLAGS="$SST_CPPFLAGS $PY_INCLUDES"
  CPPFLAGS="$CPPFLAGS $SST_CPPFLAGS"

  SST_LDFLAGS="$PY_LDFLAGS"

  # We have to use CXXFLAGS from sst-config script
  SAVE_CXXFLAGS="$CXXFLAGS"
  SST_CXXFLAGS="`$SST/bin/sst-config --CXXFLAGS`"
  CXXFLAGS="$CXXFLAGS $SST_CXXFLAGS"

  # Check for compiler consistency
  SST_CORE_CXX=`$SST/bin/sst-config --CXX`
  #if test "X$CXX" != "X$SST_CORE_CXX"; then
  #  AC_MSG_ERROR([C++ Compiler $CXX doesn't match SST core compiler $SST_CORE_CXX])
  #fi
  SST_CORE_CC=`$SST/bin/sst-config --CC`
  #if test "X$CC" != "X$SST_CORE_CC"; then
  #  AC_MSG_ERROR([C Compiler $CC doesn't match SST core compiler $SST_CORE_CC])
  #fi

  AC_CHECK_HEADERS([Python.h], [],
      [AC_MSG_ERROR([Could not locate Python installation needed by SST core])])
  AC_CHECK_HEADERS([sst/core/component.h], [],
      [AC_MSG_ERROR([Could not locate SST core header files at $SST])])

  SUMI_CPPFLAGS="$SST_INCLUDES"
  AC_SUBST(SST_CPPFLAGS)
  CPPFLAGS="$SAVE_CPPFLAGS"
  SST_CXXFLAGS=`echo "$SST_CXXFLAGS" | sed s/-std=c++11//g | sed s/-std=c++14//g | sed s/-std=c++1y//g | sed s/-std=c++1z//g`
  AC_SUBST(SST_CXXFLAGS)
  AC_SUBST(SST_LDFLAGS)
  CXXFLAGS="$SAVE_CXXFLAGS"

  # Already failed if user tried to specify --with-boost.  We insist on using whatever sst-core
  # was configured with.

  AM_CONDITIONAL([USE_MPIPARALLEL], false)
else
  SST_CPPFLAGS=""
  SST_CXXFLAGS=""
  AC_SUBST(SST_CPPFLAGS)
  AC_SUBST(SST_CXXFLAGS)
fi

])


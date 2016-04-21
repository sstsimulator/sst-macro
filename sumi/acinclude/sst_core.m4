
AC_DEFUN([CHECK_SST_CORE], [

AC_ARG_WITH([sst-core],
    AS_HELP_STRING([--with-sst-core@<:@=DIR@:>@],
        [Build shared library compatible with integrated SST core (optional).]
    ), [
      SST="$withval"
      have_integrated_core="yes"
      AM_CONDITIONAL([INTEGRATED_SST_CORE], true)
    ], [
      AM_CONDITIONAL([INTEGRATED_SST_CORE], false)
      AC_DEFINE_UNQUOTED([INTEGRATED_SST_CORE], 0, [Do not run on integrated SST core])
      have_integrated_core="no"
    ]
)

if test "X$have_integrated_core" = "Xyes"; then
    SST_INCLUDES="-I$SST/include -I$SST/include/sst -I$SST/include/sst/core"
    SST_CPPFLAGS="-DSSTMAC_INTEGRATED_SST_CORE=1 $SST_INCLUDES -D__STDC_FORMAT_MACROS"
    AC_SUBST(SST_CPPFLAGS)

    AC_DEFINE_UNQUOTED([INTEGRATED_SST_CORE], 1, [Run on integrated SST core])

    SST_CXXFLAGS="`$SST/bin/sst-config --CXXFLAGS`"
    AC_SUBST(SST_CXXFLAGS)

    BOOST_CPPFLAGS="`$SST/bin/sst-config --BOOST_CPPFLAGS`"
    BOOST_LDFLAGS="`$SST/bin/sst-config --BOOST_LDFLAGS`"
    AC_SUBST(BOOST_CPPFLAGS)
    AC_SUBST(BOOST_LDFLAGS)
fi

])



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
    SST_CXXFLAGS="`$SST/bin/sst-config --CXXFLAGS`"
    AC_SUBST(SST_CXXFLAGS)
fi

])


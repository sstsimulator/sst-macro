

AC_DEFUN([CHECK_CLANG], [

AC_MSG_CHECKING("Checking for Clang flags")
have_clang=`$srcdir/bin/config_tools/get_clang $CXX`
AC_MSG_RESULT([$have_clang])


if test "X$have_clang" = "Xyes"; then
  AC_SUBST([LD_SO_FLAGS], ["-shared -undefined dynamic_lookup"])
  AM_CONDITIONAL([HAVE_CLANG], true)
else
  AC_SUBST([LD_SO_FLAGS], ["-shared"])
  AM_CONDITIONAL([HAVE_CLANG], false)
fi
])

AC_DEFUN([CHECK_CLANG_LIBTOOLING], [

AC_ARG_WITH(clang,
  [AS_HELP_STRING(
    [--with-clang],
    [Whether Clang libTooling is available for static analysis],
    )],
  [
    clang=$withval
  ], [
    clang=no
  ]
)

if test "$clang" != "no"; then
  SAVE_LDFLAGS=$LDFLAGS
  SAVE_CPPFLAGS=$CPPFLAGS
  SAVE_CXXFLAGS=$CXXFLAGS
  CLANG_LDFLAGS=
  CLANG_CPPFLAGS=
  if test "$clang" != "yes"; then
    CLANG_LDFLAGS=-L$clang/lib 
    CLANG_CPPFLAGS=-I$clang/include
    LDFLAGS="$LDFLAGS $CLANG_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $CLANG_CPPFLAGS"
  fi
  CXXFLAGS="$CXXFLAGS $SST_CXXFLAGS"
  AC_CHECK_LIB(clang,
    clang_createIndex,
    [:],
    AC_MSG_ERROR([Unable to find valid Clang libTooling at specified location])
  )
  AC_CHECK_HEADER([clang/AST/AST.h],
    found_clang=yes
    AC_SUBST(CLANG_LDFLAGS)
    AC_SUBST(CLANG_CPPFLAGS)
    ,
    found_clang=no
    AC_MSG_ERROR([Unable to find valid Clang libTooling at specified location])
  )
  CPPFLAGS="$SAVE_CPPFLAGS"
  CXXFLAGS="$SAVE_CXXFLAGS"
  LDFLAGS="$SAVE_LDFLAGS"
else
found_clang=no
fi

if test "X$found_clang" = "Xno"; then
AM_CONDITIONAL(HAVE_CLANG, false)
else
AM_CONDITIONAL(HAVE_CLANG, true)
#need to figure out clang absolute include paths
#because clang libtooling is an abominiation hard-wired to relative paths
AC_SUBST([CLANG_LIBTOOLING_CXX_FLAGS], "`$srcdir/bin/config_tools/get_clang_includes $clang -E -v -std=c++1y -stdlib=libc++ -x c++ < /dev/null`")
AC_SUBST([CLANG_LIBTOOLING_C_FLAGS], "`$srcdir/bin/config_tools/get_clang_includes $clang -E -v -- < /dev/null`")
fi

])

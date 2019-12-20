

AC_DEFUN([CHECK_CLANG], [

  AC_MSG_CHECKING([Clang flags])
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

AC_DEFUN([CHECK_CLANG_LLVM], [
  AC_ARG_WITH(clang,
    [
      AS_HELP_STRING(
        [--with-clang],
        [Whether Clang libTooling is available for static analysis]
      )
    ],
    [
      clang=$withval
    ], 
    [
      clang=no
    ]
  )

  CLANG_INSTALL_DIR=$clang

  if test "$clang" != "no"; then
    if test "$clang" = "yes"; then
      AC_MSG_ERROR([--with-clang option requires an explicit path to Clang installation root: --with-clang=<CLANG_ROOT>])
    fi
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

    CXXFLAGS="$CXXFLAGS $SST_CXXFLAGS $STD_CXXFLAGS"

    AC_CHECK_HEADER([clang/AST/AST.h],
      found_clang=yes
      AC_SUBST(CLANG_LDFLAGS)
      AC_SUBST(CLANG_CPPFLAGS)
      AC_SUBST(CLANG_INSTALL_DIR)
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
    AM_CONDITIONAL(CLANG_NEED_LIBCPP,false)
  else
    AM_CONDITIONAL(HAVE_CLANG, true)
    offset=`$srcdir/bin/config_tools/get_offsetof_macro $CXX`
    AC_MSG_CHECKING([offsetof macro definition])
    AC_MSG_RESULT([$offset])
    AC_DEFINE_UNQUOTED([OFFSET_OF_MACRO], [$offset], "the definition of the offsetof macro")

    # need to figure out clang absolute include paths
    # because clang libtooling is an abominiation hard-wired to relative paths
    
    CLANG_LIBTOOLING_SYSTEM_LIBS=`$clang/bin/llvm-config --system-libs`
    CLANG_LIBTOOLING_LIBS=`$clang/bin/llvm-config --libs`
    LLVM_LIBS="$CLANG_LIBTOOLING_LIBS"
    LLVM_SYSTEM_LIBS="$CLANG_LIBTOOLING_SYSTEM_LIBS"
    LLVM_CPPFLAGS="$CLANG_CPPFLAGS"
    LLVM_LDFLAGS="$CLANG_LDFLAGS"

    clang_version=`$clang/bin/clang --version | head -n 1 | cut -d ' ' -f 3`
    clang_major_version=`echo $clang_version | cut -d '.' -f 1`
    if test "$clang_major_version" = "9"; then
      AM_CONDITIONAL(CLANG_NEED_LIBCPP,true)
    else
      AM_CONDITIONAL(CLANG_NEED_LIBCPP,false)
    fi

    clang_compatibility=`$srcdir/bin/config_tools/check_clang_compatibility $CXX $clang $srcdir/bin/config_tools/clang_version_test.cc $CXXFLAGS $SST_CXXFLAGS $STD_CXXFLAGS`

    if test "X$clang_compatibility" != "X"; then
      AC_MSG_ERROR([$clang_compatibility])
    fi

    AC_SUBST([CLANG_LIBTOOLING_LIBS])
    AC_SUBST([CLANG_LIBTOOLING_SYSTEM_LIBS])
    AC_SUBST([CLANG_LIBTOOLING_CXX_FLAGS], "`$srcdir/bin/config_tools/get_clang_includes $clang -E -v -std=c++1y -stdlib=libc++ -x c++`")
    AC_SUBST([CLANG_LIBTOOLING_C_FLAGS], "`$srcdir/bin/config_tools/get_clang_includes $clang -E -v`")
  fi

  clang_has_float128=`$srcdir/bin/config_tools/get_float_128 $clang/bin/clang++`
  if test "X$clang_has_float128" = "Xyes"; then
    AC_SUBST([have_float_128], [True])
  else
    AC_SUBST([have_float_128], [False])
  fi

  AC_SUBST([LLVM_LIBS])
  AC_SUBST([LLVM_SYSTEM_LIBS])
  AC_SUBST([LLVM_CPPFLAGS])
  AC_SUBST([LLVM_LDFLAGS])
])




AC_DEFUN([CHECK_CXX_STD], [

AH_TEMPLATE([HAVE_CXX14],
            [Define to use C++14 language features])

AH_TEMPLATE([HAVE_CXX17],
            [Define to use C++17 language features])

AC_ARG_WITH(std,
  [
    AS_HELP_STRING(
      [--with-std],
      [The C++ standard to enable],
    )
  ],
  [
    cxxstd=$withval
  ],
  [
    cxxstd=11
  ]
)

# Use decltypes to check 11
m4_define([CXX11_testbody], [[
int x = 5;
decltype(x) y = 10;
]])

# Use auto returns to check 14
m4_define([CXX14_testbody], [[
auto myFxn(){
  return 0;
}
]])

# Use structured bindings to check 17
m4_define([CXX17_testbody], [[
struct S {
  int a;
  int b;
};
S getThing();
void myFxn()
{
  auto [a,b] = getThing();
}
]])

if test "x$cxxstd" = "x11"; then
  SAVE_FLAGS="$CXXFLAGS"
  CXXFLAGS="$SAVE_FLAGS -std=c++11"
  AC_COMPILE_IFELSE(
    [AC_LANG_SOURCE([CXX11_testbody])],
    [
      have_cxx11=yes
    ],
    [
      have_cxx11=no
    ]
  )
  if test "x$have_cxx11" = "xno"; then
    AC_MSG_ERROR([Minimally require C++11 support. Compiler check with -std=c++11 failed])
  fi
  STD_CXXFLAGS="-std=c++11"
  CXXFLAGS="$SAVE_FLAGS"

  AM_CONDITIONAL([HAVE_CXX14], [false])
  AM_CONDITIONAL([HAVE_CXX17], [false])
fi

if test "x$cxxstd" = "x14"; then
  SAVE_FLAGS="$CXXFLAGS"
  CXXFLAGS="$SAVE_FLAGS -std=c++14"
  AC_COMPILE_IFELSE(
    [AC_LANG_SOURCE([CXX14_testbody])],
    [
      have_cxx14=yes
    ],
    [
      have_cxx14=no
    ]
  )
  CXXFLAGS="$SAVE_FLAGS"
  if test "x$have_cxx14" = "xyes"; then
    STD_CXXFLAGS="-std=c++14"
  else
    CXXFLAGS="$SAVE_FLAGS -std=c++1y"
    AC_COMPILE_IFELSE(
      [AC_LANG_SOURCE([CXX14_testbody])],
      [
        have_cxx1y=yes
      ],
      [
        have_cxx1y=no
      ]
    )
    if test "x$have_cxx1y" = "xyes"; then
      STD_CXXFLAGS="-std=c++1y"
    else
      AC_MSG_ERROR([Requested C++14 support. Neither -std=c++14 or -std=c++1y passed compiler checks])
    fi
  fi
  CXXFLAGS="$SAVE_FLAGS"

  AM_CONDITIONAL([HAVE_CXX14], [true])
  AM_CONDITIONAL([HAVE_CXX17], [false])
fi

if test "x$cxxstd" = "x17"; then
  SAVE_FLAGS="$CXXFLAGS"
  CXXFLAGS="$SAVE_FLAGS -std=c++17"
  AC_COMPILE_IFELSE(
    [AC_LANG_SOURCE([CXX17_testbody])],
    [
      have_cxx17=yes
    ],
    [
      have_cxx17=no
    ]
  )
  CXXFLAGS="$SAVE_FLAGS"
  if test "x$have_cxx17" = "xyes"; then
    STD_CXXFLAGS="-std=c++17"
  else
    CXXFLAGS="$SAVE_FLAGS -std=c++1z"
    AC_COMPILE_IFELSE(
      [AC_LANG_SOURCE([CXX17_testbody])],
      [
        have_cxx1z=yes
      ],
      [
        have_cxx1z=no
      ]
    )
    if test "x$have_cxx1z" = "xyes"; then
      STD_CXXFLAGS="-std=c++1z"
    else
      AC_MSG_ERROR([Requested C++17 support. Neither -std=c++17 or -std=c++1z passed compiler checks])
    fi
  fi
  CXXFLAGS="$SAVE_FLAGS"
  AM_CONDITIONAL([HAVE_CXX14], [true])
  AM_CONDITIONAL([HAVE_CXX17], [true])
fi

if test "x$cxxstd" != "x11" -a test "x$cxxstd" != "x14" -a test "x$cxxstd" != "x17"; then
  AC_MSG_ERROR([Got invalid C++ standard request $cxxstd. Must be 11, 14, or 17])
fi

AC_SUBST([STD_CXXFLAGS])

])

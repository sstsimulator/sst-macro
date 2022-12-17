

AC_DEFUN([CHECK_THREADING], [

AH_TEMPLATE([HAVE_UCONTEXT], [Define to make ucontext available for threading])
AC_ARG_WITH(ucontext,
  [AS_HELP_STRING(
    [--with-ucontext],
    [Control whether or not ucontext is available. Default is yes if not on Darwin.]
    )],
  [
    user_with_ucontext=yes
    enable_ucontext=$enableval
  ], [
    user_with_ucontext=no
    if test "$darwin" = false; then
      enable_ucontext=yes
    else
      enable_ucontext=no
    fi
  ]
)

AH_TEMPLATE([HAVE_GNU_PTH], [Define to make pth available for threading])
AC_ARG_WITH(pth,
  [AS_HELP_STRING(
    [--with-pth@<:@=DIR@:>@],
    [Control whether or not pth is available. Optionally specify installation location
    DIR. Default is yes.]
    )],
  [
    user_with_pth=yes
    enable_pth=$withval
  ], [
    user_with_pth=no
    enable_pth=yes
  ]
)

if test "$enable_ucontext" != no; then
  AC_LINK_IFELSE(
    [AC_LANG_PROGRAM(
      [
        #include <ucontext.h>
        void func(void) {return;}
      ], [
        ucontext_t mcp, ucp;
        getcontext(&ucp);
        makecontext(&mcp, func, 0);
        swapcontext(&ucp, &mcp);
      ])],
    [
      AC_MSG_CHECKING([whether ucontext.h is present and usable])
      AC_MSG_RESULT([yes])
      enable_ucontext="yes"
      AC_DEFINE(HAVE_UCONTEXT)
      AM_CONDITIONAL(HAVE_UCONTEXT, true)
      AC_SUBST(UCONTEXT_CPPFLAGS)
      AC_SUBST(UCONTEXT_LDFLAGS)
    ], [
      AC_MSG_CHECKING([whether ucontext.h is present and usable])
      AC_MSG_RESULT([no])
      enable_ucontext="no"
      AM_CONDITIONAL(HAVE_UCONTEXT, false)
      if test "$user_with_ucontext" = yes; then
        AC_MSG_ERROR([ucontext tests failed])
      fi
    ]
  )
else
  AM_CONDITIONAL(HAVE_UCONTEXT, false)
fi


if test "$enable_pth" != "no"; then
  SAVE_LDFLAGS=$LDFLAGS
  SAVE_CPPFLAGS=$CPPFLAGS
  PTH_LDFLAGS=
  PTH_CPPFLAGS=

  if test "$enable_pth" != "yes"; then
    PTH_LDFLAGS=-L$enable_pth/lib
    PTH_CPPFLAGS=-I$enable_pth/include
    LDFLAGS="$LDFLAGS $PTH_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $PTH_CPPFLAGS"
  fi

  AC_CHECK_HEADERS([pth.h], 
    [ 
     found_pth_header="yes"
    ], 
    [
     found_pth_header="no"
    ]
  )

  AC_CHECK_LIB(
    [pth],
    [pth_uctx_switch],
    [
      found_pth_lib="yes"
    ],
    [
      found_pth_lib="no"
    ]
  )
  if test "$found_pth_lib" = "yes" -a "$found_pth_header" = "yes"; then
     AC_DEFINE(HAVE_GNU_PTH)
     AM_CONDITIONAL(HAVE_PTH, true)
     PTH_LDFLAGS="$PTH_LDFLAGS -lpth"
     AC_SUBST(PTH_CPPFLAGS)
     AC_SUBST(PTH_LDFLAGS)
     enable_pth="yes"
  else
    AM_CONDITIONAL(HAVE_PTH, false)
    enable_pth="no"
    if test "$user_with_pth" = yes; then
      AC_MSG_ERROR([pth tests failed])
    fi
  fi
  LDFLAGS=$SAVE_LDFLAGS
  CPPFLAGS=$SAVE_CPPFLAGS
else
AM_CONDITIONAL(HAVE_PTH, false)
fi

AC_LINK_IFELSE(
[AC_LANG_PROGRAM(
[
  #include <pthread.h>
],[
  pthread_create(0,0,0,0);
  pthread_join(0,0);
])],
[
AC_MSG_CHECKING([whether pthreads is automatically usable])
AC_MSG_RESULT([yes])
enable_pthread="yes"
],[
AC_MSG_CHECKING([whether pthreads is automatically usable])
AC_MSG_RESULT([no])
enable_pthread="no"
])

if test "$enable_pthread" = no; then
LIBSAVE="$LIBS"
LIBS="$LIBS -lpthread"
AC_LINK_IFELSE(
[AC_LANG_PROGRAM(
  [
    #include <pthread.h>
  ],[
    pthread_create(0,0,0,0);
    pthread_join(0,0);
  ])],
[
  AC_MSG_RESULT([yes])
  enable_pthread="yes"
],[
  AC_MSG_RESULT([no])
  enable_pthread="no"
  LIBS="$LIBSAVE"
  if test "$user_with_pthread" = yes; then
    AC_MSG_ERROR([pthreads tests failed])
  fi
])
fi

AH_TEMPLATE([HAVE_PTHREAD], [Define to make pthreads available for threading])
if test "$enable_pthread" = yes; then
AC_DEFINE(HAVE_PTHREAD)
fi

macCheck=`echo $host_os | awk '{print substr(\$ 1,0,6)}'`

AM_CONDITIONAL(FCONTEXT_X86, [test "X$host_cpu" = "Xx86_64"])
AM_CONDITIONAL(FCONTEXT_MAC, [test "X$macCheck" = "Xdarwin"])
AM_CONDITIONAL(FCONTEXT_A64, [test "X$host_cpu" = "Xarm64" -o "X$host_cpu" = "Xaarch64" -o "X$host_cpu" = "Xarm"])

#previously we would have checked for at least one threading interface
#but now we guarantee that fcontext is available to use
#thus check is no longer necessary

])



AC_DEFUN([CHECK_THREADING], [
# Find out if we want to use pthreads instead of a user-space threads.
AH_TEMPLATE([HAVE_PTHREAD], [Define to allow use of pthreads])
AC_ARG_ENABLE(pthread,
  [AS_HELP_STRING(
    [--(dis|en)able-pthread],
    [Control whether or not pthreads is available. Default is yes.]
    )],
  [
    enable_pthread=$enableval
  ], [
    enable_pthread=yes
  ]
)

AH_TEMPLATE([HAVE_PTH], [Define to make pth available for threading])
AC_ARG_ENABLE(pth,
  [AS_HELP_STRING(
    [--enable-pth[=PATH]],
    [Use pth installed at the given PATH. Can also give builtin (which is the default) to use pth shipped with SST/macro.]
    )],
  [
    enable_pth="$enableval"
  ], [
    enable_pth=builtin
  ]
)

AH_TEMPLATE([HAVE_UCONTEXT], [Define to make ucontext available for threading])
enable_ucontext=maybe
AC_ARG_ENABLE(ucontext,
  [AS_HELP_STRING(
    [--(dis|en)able-ucontext],
    [Control whether or not ucontext is available. Default is yes if not on Darwin.]
    )],
  [
    enable_ucontext=$enableval
  ], [
    if test "$darwin" = false; then
      enable_ucontext=yes
    else
      enable_ucontext=no
    fi
  ]
)

if test "$enable_pthread" != no; then
  AC_MSG_CHECKING([whether pthreads is automatically usable])
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
    ])

  if test "$enable_pthread" = no; then
    AC_MSG_CHECKING([whether libpthread is present and usable])
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
      ])
  fi

  if test "$enable_pthread" = yes; then
    AC_DEFINE(HAVE_PTHREAD)
  fi

fi

if test "$enable_ucontext" != no; then
  AC_MSG_CHECKING([whether ucontext.h is present and usable])
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
      AC_MSG_RESULT([yes])
      enable_ucontext="yes"
      AC_DEFINE(HAVE_UCONTEXT)
      AM_CONDITIONAL(HAVE_UCONTEXT, true)
    ], [
      AC_MSG_RESULT([no])
      AM_CONDITIONAL(HAVE_UCONTEXT, false)
      if test "$enable_ucontext" = maybe; then
        enable_ucontext="no"
      else
        AC_MSG_ERROR([ucontext enabled but not usable])
      fi
    ]
  )
else
  AM_CONDITIONAL(HAVE_UCONTEXT, false)
fi

# If pth is enabled, but we're not using the builtin version, then
# test whether or not it works.
PTH_PATH=none
if test "$enable_pth" != "no" -a "$enable_pth" != builtin; then
  if test "$enable_pth" != "yes"; then
    CXXFLAGS="$CXXFLAGS -I$enable_pth/include"
    LDFLAGS="$LDFLAGS -L$enable_pth/lib"
    PTH_PATH="$enable_pth"
  else
    PTH_PATH="default"
  fi
  dnl First check for a custom pth.
  AC_CHECK_LIB(
    [pth-sstmac],
    [pth_uctx_switch_ignore_sigmask],
    [ HAVE_PTH_CUSTOM=yes
      LIBS="-lpth-sstmac $LIBS"
    ],
    [ dnl Couldn't find a custom pth -- find a regular one.
      AC_CHECK_LIB(
        [pth],
        [pth_uctx_switch],
        [LIBS="-lpth $LIBS"],
        [AC_MSG_ERROR([GNU pth enabled but not found])]
      )
    ]
  )
  AC_DEFINE(HAVE_PTH)
fi

if test "$enable_pth" = builtin; then
  AC_DEFINE(HAVE_PTH)
  HAVE_PTH_CUSTOM="yes"
  PTH_PATH=$abs_top_srcdir/pth
  AC_DEFINE([HAVE_PTH_CUSTOM],[],[Defined if using custom (builtin) pth.])
fi

AC_ARG_ENABLE(pth-fast-context-switch,
  [AS_HELP_STRING(
    [--(dis|en)able-pth-fast-context-switch],
    [Control whether or not fast context switching is used with builtin pth (if available). Default is yes.]
    )],
  [
    enable_pth_fast_context_switch=$enableval
  ], [
    enable_pth_fast_context_switch=yes
  ]
)
if test "$HAVE_PTH_CUSTOM" = yes && test "$enable_pth_fast_context_switch" = yes; then
  AC_DEFINE([HAVE_PTH_UCTX_SWITCH_IGNORE_SIGMASK],[],[Defined if pth_uctx_switch_ignore_sigmask is available.])
fi

AC_ARG_WITH(default-threading,
  [AS_HELP_STRING(
    [--with-default-threading=(pth|ucontext|pthread)],
    [Select the default threading method (default pth).]
    )],
  [
    default_threading=$with_default_threading
  ],
)
#unless explicitly given, do not choose a default threading
#let that be chosen by logic within operating_system.cc

AH_TEMPLATE([USE_PTH], [Use Pth for threading by default.])
AH_TEMPLATE([USE_UCONTEXT], [Use ucontext for threading by default.])
AH_TEMPLATE([USE_PTHREAD], [Use pthread for threading by default.])
if test "$default_threading" = pth -a "$enable_pth" != no; then
  AC_DEFINE(USE_PTH)
elif test "$default_threading" = ucontext -a "$enable_ucontext" != no; then
  AC_DEFINE(USE_UCONTEXT)
elif test "$default_threading" = pthread -a "$enable_pthread" != no; then
  AC_DEFINE(USE_PTHREAD)
fi
])


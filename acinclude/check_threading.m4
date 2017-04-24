

AC_DEFUN([CHECK_THREADING], [

# Find out if we want pthreads as an option
AH_TEMPLATE([HAVE_PTHREAD], [Define to make pthreads available for threading])
AC_ARG_WITH(pthread,
  [AS_HELP_STRING(
    [--with-pthread],
    [Control whether or not pthreads is available. Default is yes.]
    )],
  [
    user_with_pthread=yes
    enable_pthread=$enableval
  ], [
    user_with_pthread=no
    enable_pthread=yes
  ]
)

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

if test "X$have_integrated_core" = "Xyes"; then
  AC_MSG_RESULT([pthread virtual thread interface not compatible with unified core -- disabling])
  enable_pthread=no
else
  if test "$enable_pthread" != no; then
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
        if test "$user_with_pthread" = yes; then
          AC_MSG_ERROR([pthreads tests failed])
        fi
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

    if test "$enable_pthread" = yes; then
      AC_DEFINE(HAVE_PTHREAD)
    fi
  fi
fi

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
  AC_CHECK_LIB(
    [pth],
    [pth_uctx_switch],
    [
      AC_DEFINE(HAVE_GNU_PTH)
      AM_CONDITIONAL(HAVE_PTH, true)
      PTH_LDFLAGS="$PTH_LDFLAGS -lpth"
      AC_SUBST(PTH_CPPFLAGS)
      AC_SUBST(PTH_LDFLAGS)
      enable_pth="yes"
    ],
    [
      AM_CONDITIONAL(HAVE_PTH, false)
      enable_pth="no"
      if test "$user_with_pth" = yes; then
        AC_MSG_ERROR([pth tests failed])
      fi
    ]
  )
  LDFLAGS=$SAVE_LDFLAGS
  CPPFLAGS=$SAVE_CPPFLAGS
fi

AC_ARG_WITH(default-threading,
  [AS_HELP_STRING(
    [--with-default-threading=(pth|ucontext|pthread)],
    [Select the default threading method.]
    )],
  [
    default_threading=$with_default_threading
  ],
)

#fail if default threading isn't available
if test "$default_threading" = pth -a "$enable_pth" = no; then
  AC_MSG_ERROR([Default threading method GNU pth is unavailable])
elif test "$default_threading" = ucontext -a "$enable_ucontext" = no; then
  AC_MSG_ERROR([Default threading method uncontext is unavailable])
elif test "$default_threading" = pthread -a "$enable_pthread" = no; then
  AC_MSG_ERROR([Default threading method pthreads is unavailable])
fi

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

#make sure we have an acceptable threading capabilites
#pthreads only counts if it has been explicitly given as the default
at_least_one_threading=no
if test "$default_threading" = pthread -a "$enable_pthread" != no; then
  at_least_one_threading=yes
elif test "$enable_ucontext" != no -o "$enable_pth" != no; then 
  at_least_one_threading=yes
fi

if test "X$at_least_one_threading" = "Xno"; then
AC_MSG_ERROR([Insufficient virtual threading interfaces available
must have pth or ucontext for best performance
use --with-default-threading=pthread to allow pthread only (good for debugging but low performance)
ucontext is not available on Mac OS X
pthread is not compatible with integrated SST core
pth must be downloaded from https://www.gnu.org/software/pth
])
fi
])


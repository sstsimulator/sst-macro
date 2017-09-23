

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

AH_TEMPLATE([HAVE_FCONTEXT], [Define to make pth available for threading])
AC_ARG_WITH(fcontext,
  [AS_HELP_STRING(
    [--with-pth@<:@=DIR@:>@],
    [Control whether or not Boost fcontext is available. Optionally specify installation location
    DIR. Default is yes.]
    )],
  [
    user_with_fcontext=yes
    enable_fcontext=$withval
  ], [
    user_with_fcontext=no
    enable_fcontext=no
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
else
AM_CONDITIONAL(HAVE_PTH, false)
fi

if test "$enable_fcontext" != "no"; then
  SAVE_LDFLAGS=$LDFLAGS
  SAVE_LIBS=$LIBS
  SAVE_CPPFLAGS=$CPPFLAGS
  FCONTEXT_LIBS=
  FCONTEXT_LDFLAGS=
  FCONTEXT_CPPFLAGS=
  FCONTEXT_LIBDIR=
  if test "$enable_fcontext" != "yes"; then
    FCONTEXT_LDFLAGS=-L$enable_fcontext/lib
    FCONTEXT_CPPFLAGS=-I$enable_fcontext/include
    AM_CONDITIONAL(HAVE_FCONTEXT_LIBDIR, true)
    LDFLAGS="$LDFLAGS $FCONTEXT_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $FCONTEXT_CPPFLAGS"
    FCONTEXT_LIBDIR=$enable_fcontext/lib
  else
    AM_CONDITIONAL(HAVE_FCONTEXT_LIBDIR, false)
  fi
  AC_SUBST(FCONTEXT_LIBDIR)
  AC_CHECK_LIB(
    [boost_context],
    [jump_fcontext],
    [
      AC_DEFINE(HAVE_FCONTEXT)
      AM_CONDITIONAL(HAVE_FCONTEXT, true)
      FCONTEXT_LIBS=-lboost_context
      AC_SUBST(FCONTEXT_CPPFLAGS)
      AC_SUBST(FCONTEXT_LDFLAGS)
      AC_SUBST(FCONTEXT_LIBS)
      enable_fcontext="yes"
    ],
    [
      AM_CONDITIONAL(HAVE_FCONTEXT, false)
      enable_fcontext="yes"
      if test "$user_with_pth" = yes; then
        AC_MSG_ERROR([fcontext tests failed])
      else 
        FCONTEXT_CPPFLAGS=""
        FCONTEXT_LDFLAGS=""
        FCONTEXT_LIBS=""
        AC_SUBST(FCONTEXT_CPPFLAGS)
        AC_SUBST(FCONTEXT_LDFLAGS)
        AC_SUBST(FCONTEXT_LIBS)
      fi
    ]
  )
  LIBS="$LIBS $FCONTEXT_LIBS"
  AC_LINK_IFELSE(
    [AC_LANG_PROGRAM(
      [
      #include <boost/context/all.hpp>
      namespace ctx=boost::context;
      ], [
      int a;
      ctx::continuation source=ctx::callcc(
        [[&a]](ctx::continuation && sink){
          a=0;
          int b=1;
          for(;;){
            sink=sink.resume();
            int next=a+b;
            a=b;
            b=next;
          }
          return std::move(sink);
        });
        for (int j=0;j<10;++j) {
          source=source.resume();
        }
      ])],
    [
      AC_MSG_CHECKING([whether fcontext headers are present and usable])
      AC_MSG_RESULT([yes])
    ], [
      AC_MSG_CHECKING([whether fcontext headers are present and usable])
      AC_MSG_RESULT([no])
      enable_fcontext="no"
      if test "$user_with_fcontext" = yes; then
        AC_MSG_ERROR([fcontext tests failed. Ensure Boost >= 1.65 and C++14 support])
      fi
    ]
  )
  LIBS=$SAVE_LIBS
  LDFLAGS=$SAVE_LDFLAGS
  CPPFLAGS=$SAVE_CPPFLAGS
else
AM_CONDITIONAL(HAVE_FCONTEXT, false)
AM_CONDITIONAL(HAVE_FCONTEXT_LIBDIR, false)
FCONTEXT_CPPFLAGS=""
FCONTEXT_LDFLAGS=""
FCONTEXT_LIBS=""
AC_SUBST(FCONTEXT_CPPFLAGS)
AC_SUBST(FCONTEXT_LDFLAGS)
AC_SUBST(FCONTEXT_LIBS)
fi


if test "$enable_ucontext" != no -o "$enable_pth" != no -o "$enable_fcontext" != no; then 
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


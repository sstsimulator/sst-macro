AC_DEFUN([CHECK_PYTHON], [

AC_ARG_WITH([python-config],
  [AS_HELP_STRING(
    [--with-python-config=<path>],
    [Give location to Python package's python-config],
    )
  ],
  [
    pycfg="$withval"
    if test -z "$pycfg"; then
      AC_MSG_ERROR([Must give valid python-config to --with-python-config option])
    fi
  ],
  [
    pycfg=""
  ]
)

 AC_ARG_WITH([python],
   [AS_HELP_STRING([--with-python=<path>],
     [Give location to Python installation prefix])
   ],
   [
     pyprefix="$withval"
   ],
   [
   ]
 )

 if test "$have_integrated_core" = "yes"; then
   core_pycfg=`$SST/bin/sst-config --PYTHON_CONFIG`
   if test -n "$pycfg"; then
    #if given python config, make sure consistent with sst-core
    if test "$pycfg" != "$core_pycfg"; then
      AC_MSG_ERROR([--with-python-config given $pycfg, which is different from sst-core $core_pycfg. Omit --with-python-config or ensure they match.])
    fi
   fi
   pycfg=$core_pycfg
 fi

 # We now prioritize python3
 if test -z "$pycfg"; then
   if test -z "$pyprefix"; then
     AC_PATH_PROGS([pycfg], ["python3-config" "python-config" "python2-config"], ["notfound"])
   else
     AC_PATH_PROGS([pycfg], ["python3-config" "python-config" "python2-config"], ["notfound"], [$pyprefix/bin])
   fi
 fi

 if test "$pycfg" = "notfound"; then
   AC_MSG_ERROR([not given a valid Python prefix or python-config path])
 fi
 pyprefix="`$pycfg --prefix`"

 AC_MSG_CHECKING([for valid Python])
 AC_MSG_RESULT([$pyprefix])

 if [[[ $pycfg == *python3* ]]]; then
   py_tryfirst="python3"
   py_trysecond="python"
 else
   py_tryfirst="python"
   py_trysecond="python3"
 fi

 AC_PATH_PROGS([pyexe], ["$py_tryfirst" "$py_trysecond" "python2"], ["notfound"], [$pyprefix/bin])
 if test "$pyexe" = "notfound"; then
   AC_MSG_ERROR([No python/python2/python3 exe found in $pyprefix/bin])
 fi

 AC_SUBST([pyexe])

 if test "$have_integrated_core" = "yes"; then
  PY_INCLUDES="`$pycfg --includes`"
  PY_LDFLAGS=`$pyexe $srcdir/bin/config_tools/get_py_ldflags`
  PY_CPPFLAGS="$PY_INCLUDES"
  SAVE_CPPFLAGS="$CPPFLAGS"
  CPPFLAGS="$CPPFLAGS $PY_CPPFLAGS"
  AC_CHECK_HEADERS([Python.h], [],
       [AC_MSG_ERROR([Could not locate Python installation needed by SST core])])
  CPPFLAGS="$SAVE_CPPFLAGS"
 fi
 AC_SUBST(PY_CPPFLAGS)
 AC_SUBST(PY_LDFLAGS)
])

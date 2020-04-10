AC_DEFUN([CHECK_PYTHON], [
 AC_ARG_WITH([python-config],
   [
    AS_HELP_STRING([--with-python-config=<path>],
     [Give location to Python package's python-config])
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
   [
    AS_HELP_STRING([--with-python=<path>],
     [Give location to Python installation prefix])
   ],
   [
     pyprefix="$withval"
   ],
   [
   ]
 )

 if test -z "$pycfg"; then
   if test -z "$pyprefix"; then
     AC_PATH_PROGS([pycfg], ["python-config" "python3-config" "python2-config"], ["notfound"])
   else
     AC_PATH_PROGS([pycfg], ["python-config" "python3-config" "python2-config"], ["notfound"], [$pyprefix/bin])
   fi
 fi

 if test "$pycfg" = "notfound"; then
   AC_MSG_ERROR([not given a valid Python prefix or python-config path])
 fi
 pyprefix="`$pycfg --prefix`"

 AC_MSG_CHECKING([for valid Python])
 AC_MSG_RESULT([$pyprefix])

 AC_PATH_PROGS([pyexe], ["python" "python3" "python2"], ["notfound"], [$pyprefix/bin])
 if test "$pyexe" = "notfound"; then
   AC_MSG_ERROR([No python/python2/python3 exe found in $pyprefix/bin])
 fi

 AC_SUBST([pyexe])
])

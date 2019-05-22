
AC_DEFUN([CHECK_DOT], [
  # dot (from graphviz) is optional but can really help with doxygen documentation
  AC_PATH_PROG([DOT], [dot])
  if test -z "$DOT"; then
    HAVE_DOT=NO
  else
    HAVE_DOT=YES
  fi
  AC_SUBST([HAVE_DOT])
])


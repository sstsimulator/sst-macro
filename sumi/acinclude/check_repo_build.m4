
AC_DEFUN([CHECK_REPO_BUILD], [
  AC_MSG_CHECKING([checking whether building from repo])
  isRepo=`cd $srcdir ; bin/check_repo_build`
if test "X$isRepo" = "Xyes"; then
  AC_MSG_RESULT([yes])
  AM_CONDITIONAL([REPO_BUILD], true)
  runScript=`$srcdir/bin/make_repo_header $srcdir $1`
  AC_DEFINE_UNQUOTED([REPO_BUILD], 1, "Build from a repo checkout")
else
  AC_MSG_RESULT([no])
  AM_CONDITIONAL([REPO_BUILD], false)
  AC_DEFINE_UNQUOTED([REPO_BUILD], 0, "Build from a tarball")
fi

])


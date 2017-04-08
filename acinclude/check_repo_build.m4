
AC_DEFUN([CHECK_REPO_BUILD], [
  AC_MSG_CHECKING([checking whether building from repo])
  gitBranch=`cd $srcdir ; bin/config_tools/check_repo_build`
if test "X$gitBranch" = "X"; then
  AC_MSG_RESULT([no])
  AM_CONDITIONAL([REPO_BUILD], false)
  AC_DEFINE_UNQUOTED([REPO_BUILD], 0, "Build from a tarball")
  AC_SUBST(git_branch, [""])
else
  AC_MSG_RESULT(["$gitBranch"])
  AM_CONDITIONAL([REPO_BUILD], true)
  runScript=`$srcdir/bin/config_tools/make_repo_header $srcdir $1`
  AC_DEFINE_UNQUOTED([REPO_BUILD], 1, "Build from a repo checkout")
  AC_SUBST(git_branch, ["$gitBranch"])
fi

])


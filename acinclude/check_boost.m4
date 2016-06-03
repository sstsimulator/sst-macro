
AC_DEFUN([CHECK_BOOST], [
# TODO fail if C++11 or external boost isn't given
AX_BOOST_BASE(1.48)
with_external_boost=$want_boost
AC_MSG_RESULT([set external boost to $with_external_boost])
AM_CONDITIONAL([EXTERNAL_BOOST], [test "X$with_external_boost" = "Xyes"])
AC_SUBST(BOOST_CPPFLAGS)
AC_SUBST(BOOST_LDFLAGS)
])

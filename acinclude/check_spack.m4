# Spack's build system will point CC and CXX to the compiler wrapper. The
# wrapper is not valid outside of Spack's build envionment, but the location of
# the underlying compiler is stored in SPACK_CC and SPACK_CXX.

AC_DEFUN([CHECK_SPACK],
AC_SUBST(SPACK_CC, $SPACK_CC)
AC_SUBST(SPACK_CXX, $SPACK_CXX)
)

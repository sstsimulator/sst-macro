

AC_DEFUN([CHECK_TLS], [
# Check for TLS support
AX_TLS([with_tls=yes], [with_tls=no])
AH_TEMPLATE([HAVE_TLS], [Define to allow use of thread-local storage with __thread])
if test "X$with_tls" = "Xyes"; then
  AC_DEFINE(HAVE_TLS)
fi
])


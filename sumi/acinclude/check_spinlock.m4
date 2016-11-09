

AC_DEFUN([CHECK_SPINLOCK], [

AC_ARG_ENABLE(spinlock,
  [AS_HELP_STRING(
    [--(dis|en)able-spinlock],
    [Whether to compile spinlock support [default=no]]
    )],
  [
    enable_spinlock=$enableval
  ], [
    enable_spinlock=no
  ]
)

if test "X$enable_spinlock" = "Xyes"; then
  AC_SUBST([sumi_use_spinlock], [1])
else
  AC_SUBST([sumi_use_spinlock], [0])
fi

])


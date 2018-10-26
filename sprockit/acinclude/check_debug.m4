
AC_DEFUN([CHECK_DEBUG], [

AH_TEMPLATE([VALGRIND_MODE], [Define to compile in a valgrind-friendly mode])
AC_ARG_ENABLE(valgrind,
  [AS_HELP_STRING(
    [--(dis|en)able-valgrind],
    [Turn off certain optimizations to compile in a valgrind-friendly manner [default=no]],
   )],
  [
    enable_valgrind=$enableval
  ], [
    enable_valgrind=no
  ]
)
if test "X$enable_valgrind" = "Xyes"; then
    AC_DEFINE_UNQUOTED([VALGRIND_MODE], 1, [Whether to compile in a valgrind-friendly mode])
else
    AC_DEFINE_UNQUOTED([VALGRIND_MODE], 0, [Whether to compile in a valgrind-friendly mode])
fi

])


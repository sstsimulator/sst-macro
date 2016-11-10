
AC_DEFUN([CHECK_CUSTOM_NEW], [
AC_ARG_ENABLE(custom-new,
  [AS_HELP_STRING(
    [--(dis|en)able-custom-new],
    [Control whether or not the global operator new is overwritten for all allocations [default=no]]
    )],
  [
    enable_custom_new=$enableval
  ], [
    enable_custom_new=no
  ]
)
if test "X$enable_custom_new" = "Xyes"; then
    AM_CONDITIONAL([USE_CUSTOM_NEW], true)
    CPPFLAGS="$CPPFLAGS -DUSE_CUSTOM_NEW"
else
    AM_CONDITIONAL([USE_CUSTOM_NEW], false)
fi
])


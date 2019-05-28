AC_DEFUN([CHECK_CUSTOM_NEW], [

AC_ARG_ENABLE([custom-new],
  [AS_HELP_STRING([--(dis|en)able-custom-new],
    [enable custom new on certain classes for efficient, thread-safe mem pools [default=enable]])],
  [with_custom_new=$enableval],
  [with_custom_new=yes]
)

if test "X$with_custom_new" = "Xyes"; then
  AC_DEFINE_UNQUOTED([CUSTOM_NEW], 1, [Track communcation synchronization stats])
fi

])


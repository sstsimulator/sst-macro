
AC_DEFUN([CHECK_DEBUG], [

AC_ARG_ENABLE(sstmac-debug,
  [AS_HELP_STRING(
    [--(dis|en)able-sstmac-debug],
    [Whether to enable debug printing in code. Disabling optimizes code slightly by removing many if statements [default=yes]]
    )],
  [
    enable_sstmac_debug=$enableval
  ], [
    enable_sstmac_debug=yes
  ]
)
if test "X$enable_sstmac_debug" = "Xno"; then
  CPPFLAGS += -DSPROCKIT_DISABLE_DEBUG
  AM_CONDITIONAL([ENABLE_DEBUG], false)
else
  AM_CONDITIONAL([ENABLE_DEBUG], true)
fi

AH_TEMPLATE([SANITY_CHECK], [Define to compile all sanity checks])
AC_ARG_ENABLE(sanity-check,
  [AS_HELP_STRING(
    [--(dis|en)able-sanity-check],
    [Controls whether safe mode is run with sanity checks. If enabled, code may run slower [default=no]],
   )],
  [
    enable_sanity_check=$enableval
  ], [
    enable_sanity_check=no
  ]
)
if test "X$enable_sanity_check" = "Xyes"; then
    AC_DEFINE_UNQUOTED([SANITY_CHECK], 1, [Whether safe mode should be run with sanity checks])
else
    AC_DEFINE_UNQUOTED([SANITY_CHECK], 0, [Whether safe mode should be run with sanity checks])
fi

AC_ARG_ENABLE(debug-swap,
  [AS_HELP_STRING(
    [--(dis|en)able-debug-swap],
    [Whether or not to provide hooks for manual context switching in GDB [default=no]]
    )],
  [
    enable_debug_swap=$enableval
  ], [
    enable_debug_swap=no
  ]
)
if test "X$enable_debug_swap" = "Xyes"; then
    AC_DEFINE_UNQUOTED([ENABLE_DEBUG_SWAP], 1, "Track/allow context switches for debugging purposes")
fi

])

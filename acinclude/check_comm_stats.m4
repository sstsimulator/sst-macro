AC_DEFUN([CHECK_COMM_SYNC_STATS], [

AC_ARG_ENABLE([comm-sync-stats],
  [AS_HELP_STRING([--(dis|en)able-comm-sync-stats],
    [enable extra statistics for looking at synchronization delays due to communication [default=disable]])],
  [with_comm_sync_stats=$enableval],
  [with_comm_sync_stats=no]
)

if test "X$with_comm_sync_stats" = "Xyes"; then
  AC_DEFINE_UNQUOTED([COMM_SYNC_STATS], 1, [Track communcation synchronization stats])
  AM_CONDITIONAL(COMM_SYNC_STATS, true)
  force_delay_stats=yes
else
  AM_CONDITIONAL(COMM_SYNC_STATS, false)
  force_delay_stats=no
fi

])

AC_DEFUN([CHECK_COMM_DELAY_STATS], [

AC_ARG_ENABLE([comm-delay-stats],
  [AS_HELP_STRING([--(dis|en)able-comm-delay-stats],
    [enable extra statistics for analyzing messages delays [default=disable]])],
  [with_delay_stats=$enableval],
  [with_delay_stats=no]
)

if test "X$force_delay_stats"; then
 with_delay_stats=yes
 AC_MSG_RESULT([--enable-comm-sync-stats set - forcing --enable-comm-delay-stats])
fi

if test "X$with_delay_stats" = "Xyes"; then
  AC_DEFINE_UNQUOTED([COMM_DELAY_STATS], 1, [Track communcation synchronization stats])
  AM_CONDITIONAL(COMM_DELAY_STATS, true)
else
  AM_CONDITIONAL(COMM_DELAY_STATS, false)
fi

])


AC_DEFUN([CHECK_COMM_SYNC_STATS], [

AC_ARG_ENABLE([comm-sync-stats],
  [AS_HELP_STRING([--(dis|en)able-comm-sync-stats],
    [enable extra statistics for looking at synchronization delays due to communication [default=no]])],
  [with_comm_sync_stats=$enableval],
  [with_comm_sync_stats=no]
)

if test "X$with_comm_sync_stats" = "Xyes"; then
  AC_DEFINE_UNQUOTED([COMM_SYNC_STATS], 1, [Track communcation synchronization stats])
fi

])


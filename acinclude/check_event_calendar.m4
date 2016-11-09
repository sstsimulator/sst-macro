

AC_DEFUN([CHECK_EVENT_CALENDAR], [
# Find out if we want to use pthreads instead of a user-space threads.
AH_TEMPLATE([HAVE_EVENT_CALENDAR], [Define to compile event calendar])
AC_ARG_ENABLE(event-calendar,
  [AS_HELP_STRING(
    [--(dis|en)able-event-calendar],
    [Control whether or not event calendars can be used or O(1) event scheduling [default=no]],
    )],
  [
    enable_event_calendar=$enableval
  ], [
    enable_event_calendar=no
  ]
)
if test "X$enable_event_calendar" = "Xyes"; then
    AC_DEFINE_UNQUOTED([HAVE_EVENT_CALENDAR], 1, [Whether to compile compatibility with event calendars])
    AM_CONDITIONAL([HAVE_EVENT_CALENDAR], true)
else
    AC_DEFINE_UNQUOTED([HAVE_EVENT_CALENDAR], 0, [Whether to compile compatibility with event calendars])
    AM_CONDITIONAL([HAVE_EVENT_CALENDAR], false)
fi
])


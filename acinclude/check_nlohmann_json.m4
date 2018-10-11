

AC_DEFUN([CHECK_NLOHMANN_JSON], [

SAVE_CPPFLAGS="$CPPFLAGS" 
SAVE_LDFLAGS="$LDFLAGS"

AC_ARG_WITH(json,
  [AS_HELP_STRING(
    [--with-json],
    [Give path to nlohmann-json installation for json serialization support],
    )],
  [ json_path=$withval ], 
  [ json_path="no" ]
)


AH_TEMPLATE([JSON_ENABLED], [Whether nlohmann-json is available])

if ! test "X$json_path" = "Xno"; then
if ! test -f "$json_path/include/nlohmann/json.hpp"; then
AC_MSG_ERROR([json path does not exist, please specify with --with-json=PREFIX])
fi
fi

if test "X$json_path" = "Xno"; then
AM_CONDITIONAL([HAVE_JSON], [false])
json_enabled=no
else
json_enabled=yes
AM_CONDITIONAL([HAVE_JSON], [true])
AC_DEFINE_UNQUOTED([JSON_ENABLED], 1, [nlohmann-json is enabled and installed])
JSON_CPPFLAGS="-I${json_path}/include"
AC_SUBST([JSON_CPPFLAGS])
fi

])


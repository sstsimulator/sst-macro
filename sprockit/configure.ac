dnl
dnl   This file is part of SST/macroscale:
dnl                The macroscale architecture simulator from the SST suite.
dnl   Copyright (c) 2009 Sandia Corporation.
dnl   This software is distributed under the BSD License.
dnl   Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
dnl   the U.S. Government retains certain rights in this software.
dnl   For more information, see the LICENSE file in the top
dnl   SST/macroscale directory.
dnl

dnl Version info
m4_define([SPROCKIT_VERSION_TAG], 7)
m4_define([SPROCKIT_SUBVERSION_TAG], 0)
m4_define([SPROCKIT_SUBSUBVERSION_TAG], 0)
# Enable this for releases
dnl m4_define([SPROCKIT_SNAPSHOT_TAG])
# Enable this for development snapshots (should generally be enabled)
m4_define([SPROCKIT_ACVERSION_TAG],
          [SPROCKIT_VERSION_TAG.SPROCKIT_SUBVERSION_TAG.SPROCKIT_SUBSUBVERSION_TAG])

dnl Libtool library versioning is
dnl a useful idea implemented in a confusing way.
dnl -version-info A:B:C means:
dnl     Major version A
dnl     subversion B (backward compatible for all A:X where X <= B)
dnl     and A is backward compatible to major version A-C
dnl which means that C <= A,
dnl and C == 0 means no major-version backward compatibility.
m4_define([SPROCKIT_MAJOR_LIBVERS], 7)
m4_define([SPROCKIT_MINOR_LIBVERS], 0)
m4_define([SPROCKIT_BACKW_LIBVERS], 0)
m4_define([SPROCKIT_LIBVERSION_TAG],
          [SPROCKIT_MAJOR_LIBVERS:SPROCKIT_MINOR_LIBVERS:SPROCKIT_BACKW_LIBVERS])

AC_INIT([sprockit], [SPROCKIT_ACVERSION_TAG], [jjwilke@sandia.gov])
AC_CONFIG_MACRO_DIR([acinclude])
AC_CONFIG_AUX_DIR(bin)
AC_CONFIG_HEADER(sprockit/sprockitconfig-generated.h)
AX_PREFIX_CONFIG_H(sprockit/spkt_config.h, SPKT)

AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE([-Wall -Werror subdir-objects foreign])
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

AC_PROG_CXX
AC_PROG_LIBTOOL
AC_LANG_CPLUSPLUS
LT_INIT([disable-shared])

# Version info.
AH_TEMPLATE([SPROCKIT_VERSION], [Major version number])
AH_TEMPLATE([SPROCKIT_SUBVERSION], [Major version number])
AH_TEMPLATE([SPROCKIT_SUBSUBVERSION], [Major version number])
AC_DEFINE_UNQUOTED(SPROCKIT_VERSION, [SPROCKIT_VERSION_TAG])
AC_DEFINE_UNQUOTED(SPROCKIT_SUBVERSION, [SPROCKIT_SUBVERSION_TAG])
AC_DEFINE_UNQUOTED(SPROCKIT_SUBSUBVERSION, [SPROCKIT_SUBSUBVERSION_TAG])
AC_SUBST(SPROCKIT_LIBVERSION, [SPROCKIT_LIBVERSION_TAG])

# TODO fail if C++11 or external boost isn't given
AX_BOOST_BASE(1.48)
with_external_boost=$want_boost
AM_CONDITIONAL([EXTERNAL_BOOST], [test "X$with_external_boost" = "Xyes"])
AC_SUBST(BOOST_CPPFLAGS)
AC_SUBST(BOOST_LDFLAGS)

AC_ARG_ENABLE(sanity-check,
  [AS_HELP_STRING(
    [--(dis|en)able-sanity-check],
    [Controls whether safe mode is run with sanity checks. If enabled, code may run much slower [default=no]],
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

AC_ARG_ENABLE(sst-subrepo,
  [AS_HELP_STRING([--(dis|en)able-sst-subrepo],
    [Whether building as a subrepo of SST])],
  [ enable_sst_subrepo=$enableval ], 
  [ enable_sst_subrepo=no ])

have_integrated_core="no"
if test "X$enable_sst_subrepo" = "Xyes"; then
  AM_CONDITIONAL(ENABLE_SST_SUBREPO, true)
  CHECK_SST_CORE()
else
  AM_CONDITIONAL(ENABLE_SST_SUBREPO, false)
  AM_CONDITIONAL([INTEGRATED_SST_CORE], false)
fi

if test "X$have_integrated_core" = "Xno"; then
CHECK_CPP11()
else
SAVE_CXXFLAGS="$CXXFLAGS"
CXXFLAGS="$SST_CXXFLAGS $CXXFLAGS"
AC_MSG_CHECKING([C++11 headers])
AC_CHECK_HEADERS([unordered_map],
  AC_MSG_RESULT([yes])
  AM_CONDITIONAL([HAVE_CPP11], true)
  AC_DEFINE_UNQUOTED([HAVE_CPP11], 1)
  ,
  AC_MSG_RESULT([no])
  AC_DEFINE_UNQUOTED([HAVE_CPP11], 0)
  AM_CONDITIONAL([HAVE_CPP11], false)
)
CXXFLAGS="$SAVE_CXXFLAGS"
fi

CHECK_REGEX()

AH_TEMPLATE([ATOMIC_REFCOUNT], ["Whether refcounts need to be atomic operations - only x86 compatible"])
AC_ARG_ENABLE(atomic-refcount,
  [AS_HELP_STRING(
    [--(dis|en)able-atomic-refcount],
    [Enables atomic increment/decrement of refcounted pointers [default=no]]
   )],
  [
  enable_atomic_refcount=$enableval
  ], [
  enable_atomic_refcount=no
  ]
)

if test "X$enable_atomic_refcount" = "Xyes"; then
    AC_DEFINE_UNQUOTED([ATOMIC_REFCOUNT], 1, [Use atomic refcount])
fi

AC_DEFINE_UNQUOTED([ENABLE_ORDERED_MAP], 0, [Do not use C++98 ordered containers instead of unordered containers])


AC_CONFIG_FILES([
 Makefile
 test/Makefile
 sprockit/Makefile
])
AC_OUTPUT

echo "SProCKit Configuration Summary:"
echo "---------------------------------------------------------------"
echo "Install prefix     $prefix"
echo "C compiler         $CC"
echo "C++ compiler       $CXX"

if test "X$with_external_boost" = "Xno"; then
echo "Boost              None"
if test "X$enable_regexp" = "Xyes"; then
echo "Regex              C++11"
fi           
else
echo "Boost              $BOOST_PATH"
if test "X$enable_regexp" = "Xyes"; then
echo "Regex              Boost"
fi           
fi

if test "X$enable_regexp" = "Xno"; then
echo "Regex              None"
fi

echo "CFLAGS             $CFLAGS"
echo "CXXFLAGS           $CXXFLAGS"
echo "CPPFLAGS           $CPPFLAGS"
echo "LDFLAGS            $LDFLAGS"

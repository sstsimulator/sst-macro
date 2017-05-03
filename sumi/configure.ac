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
m4_define([SUMI_VERSION_TAG], 7)
m4_define([SUMI_SUBVERSION_TAG], 0)
m4_define([SUMI_SUBSUBVERSION_TAG], 0)
# Enable this for releases
dnl m4_define([SUMI_SNAPSHOT_TAG])
# Enable this for development snapshots (should generally be enabled)
m4_define([SUMI_ACVERSION_TAG],
          [SUMI_VERSION_TAG.SUMI_SUBVERSION_TAG.SUMI_SUBSUBVERSION_TAG])

dnl Libtool library versioning is
dnl a useful idea implemented in a confusing way.
dnl -version-info A:B:C means:
dnl     Major version A
dnl     subversion B (backward compatible for all A:X where X <= B)
dnl     and A is backward compatible to major version A-C
dnl which means that C <= A,
dnl and C == 0 means no major-version backward compatibility.
m4_define([SUMI_MAJOR_LIBVERS], 7)
m4_define([SUMI_MINOR_LIBVERS], 0)
m4_define([SUMI_BACKW_LIBVERS], 0)
m4_define([SUMI_LIBVERSION_TAG],
          [SUMI_MAJOR_LIBVERS:SUMI_MINOR_LIBVERS:SUMI_BACKW_LIBVERS])

AC_INIT([sumi], [SUMI_ACVERSION_TAG], [jjwilke@sandia.gov])
AC_CONFIG_MACRO_DIR([acinclude])
AC_CONFIG_AUX_DIR(bin)
AC_CONFIG_HEADER(sumi/sumiconfig-generated.h)
AX_PREFIX_CONFIG_H(sumi/sumi_config.h, SUMI)

AC_CANONICAL_SYSTEM
AM_INIT_AUTOMAKE([-Wall -Werror -Wno-portability subdir-objects foreign])
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

AC_PROG_CXX
AC_PROG_LIBTOOL
AC_LANG_CPLUSPLUS
LT_INIT([disable-shared])


# Version info.
AH_TEMPLATE([SUMI_VERSION], [Major version number])
AH_TEMPLATE([SUMI_SUBVERSION], [Major version number])
AH_TEMPLATE([SUMI_SUBSUBVERSION], [Major version number])
AC_DEFINE_UNQUOTED(SUMI_VERSION, [SUMI_VERSION_TAG])
AC_DEFINE_UNQUOTED(SUMI_SUBVERSION, [SUMI_SUBVERSION_TAG])
AC_DEFINE_UNQUOTED(SUMI_SUBSUBVERSION, [SUMI_SUBSUBVERSION_TAG])
AC_SUBST(SUMI_LIBVERSION, [SUMI_LIBVERSION_TAG])


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

CHECK_MPI()

CHECK_GNI()

CHECK_SST()

CHECK_SPINLOCK()

CHECK_COMM_SYNC_STATS()

AC_CONFIG_FILES([
 Makefile
 sumi/Makefile
 sumi/config.h:config.h.in
])
AC_OUTPUT

echo "SUMI Configuration Summary:"
echo "---------------------------------------------------------------"
echo "Install prefix     $prefix"
echo "C compiler         $CC"
echo "C++ compiler       $CXX"
#echo "Preset CXXFLAGS    $with_parent_cxxflags"
echo "CFLAGS             $CFLAGS"
echo "CXXFLAGS           $CXXFLAGS"
echo "CPPFLAGS           $CPPFLAGS"
echo "LDFLAGS            $LDFLAGS"
echo "SST Subrepo        $enable_sst_subrepo"
echo "MPI Conduit        $enable_mpi"
echo "GNI Conduit        $enable_gni"

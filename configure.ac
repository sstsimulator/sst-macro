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

#-------------------------------------------------
  launch/launch_info.h  \
# Basic setup
#-------------------------------------------------

# Version info, used both in library versioning and inside sstmacro.
m4_define([SSTMAC_VERSION_TAG], 7)
m4_define([SSTMAC_SUBVERSION_TAG], 1)
m4_define([SSTMAC_SUBSUBVERSION_TAG], 0)

dnl Enable this for releases
dnl m4_define([SSTMAC_SNAPSHOT_TAG])
dnl Enable this for beta releases
dnl m4_define([SSTMAC_SNAPSHOT_TAG],-beta1)
# Enable this for development snapshots (should generally be enabled)
#m4_define([SSTMAC_SNAPSHOT_TAG],-snapshot)

# Construct the sstmacro version
m4_define([SSTMAC_ACVERSION_TAG],
          [SSTMAC_VERSION_TAG.SSTMAC_SUBVERSION_TAG.SSTMAC_SUBSUBVERSION_TAG])

dnl AC_PREREQ([2.68]) avoid this if possible
AC_INIT([sstmacro], [SSTMAC_ACVERSION_TAG], [sstmacro-devel@googlegroups.com])
AC_CONFIG_MACRO_DIR([acinclude])
AC_CONFIG_AUX_DIR(bin)
AC_CONFIG_HEADER(sstmac/common/config.h)
AX_PREFIX_CONFIG_H(sstmac/common/sstmac_config.h, SSTMAC)
AC_CANONICAL_TARGET

if test "X$prefix" == "XNONE"; then
  MY_PREFIX=$ac_default_prefix
else
  MY_PREFIX=`cd $prefix ; pwd`
fi
MY_ABS_SRCPATH=`cd $srcdir ; pwd`

AC_DEFINE_UNQUOTED([CONFIG_INSTALL_INCLUDE_PATH], "$MY_PREFIX/include/sstmac/configurations",
                    [The include path for .ini configurations])

AC_DEFINE_UNQUOTED([CONFIG_SRC_INCLUDE_PATH], "$MY_ABS_SRCPATH/configurations",
                    [The include path for .ini configurations])

# Construct the libtool version
dnl Libtool library versioning is used to determine compatible libraries. A libtool version
dnl consists of three numbers CURRENT:REVISION:AGE. These have the following meanings:
dnl CURRENT:  The most recent interface number that this library implements.
dnl REVISION: The implementation number of the current interface.
dnl AGE:      The difference between the newest and oldest interfaces
dnl           that this library implements. In other words, the
dnl           library implements all the interface numbers in the
dnl           range from number current - age to current.
dnl For new releases the following procedure is used to determine the new version:
dnl If incompatible changes are made: CURRENT++, REVISION=0, AGE=0
dnl else if interfaces are added: CURRENT++, REVISION=0, AGE++
dnl else (no interface changes): REVISION++
m4_define([SSTMAC_CURRENT_LIBVERS],  7)
m4_define([SSTMAC_REVISION_LIBVERS], 1)
m4_define([SSTMAC_AGE_LIBVERS],      0)
m4_define([SSTMAC_LIBVERSION_TAG],
          [SSTMAC_CURRENT_LIBVERS:SSTMAC_REVISION_LIBVERS:SSTMAC_AGE_LIBVERS])

# More version info.
AH_TEMPLATE([VERSION], [Major version number])
AH_TEMPLATE([SUBVERSION], [Major version number])
AH_TEMPLATE([SUBSUBVERSION], [Major version number])
AC_DEFINE_UNQUOTED(VERSION, [SSTMAC_VERSION_TAG])
AC_DEFINE_UNQUOTED(SUBVERSION, [SSTMAC_SUBVERSION_TAG])
AC_DEFINE_UNQUOTED(SUBSUBVERSION, [SSTMAC_SUBSUBVERSION_TAG])
AC_SUBST(SSTMAC_LIBVERSION, [SSTMAC_LIBVERSION_TAG])

# Init automake
AM_INIT_AUTOMAKE([tar-pax -Wall -Werror foreign -Wno-portability subdir-objects])
m4_ifdef([AM_PROG_AR], [AM_PROG_AR])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

#-------------------------------------------------
# Environment 
#-------------------------------------------------

# Find out if this is running Mac OS X.
darwin=false
case $target_os in
  darwin*)  darwin=true ;;
esac
AM_CONDITIONAL([DARWIN],[test "$darwin" = true])

# Before detecting compilers, we must see if a non-native DES core
# will be used. If so, we try to use its compiler and flags.
# Currently only supporting native DES.
nat_des=yes # native DES
sst_des=no  # SST/core DES



# Check compilers and environment
dnl AC_PROG_RANLIB
AC_PROG_CC
AC_PROG_CXX
AC_PROG_FC
AC_LANG_CPLUSPLUS
AC_PROG_LIBTOOL
LT_INIT([disable-shared])
#LT_INIT()
LT_PREREQ([2.2.6])
AC_PROG_LN_S

os=`$srcdir/bin/config_tools/get_os`

if test "X$os" = "Xubuntu"; then
LDFLAGS="$LDFLAGS -Wl,--no-as-needed"
fi

CHECK_CLANG()

CHECK_GCC()

CHECK_SST_CORE()

AC_CHECK_HEADERS([mpi.h],
  have_mpi_header=yes,
  have_mpi_header=no
)

if test "X$have_integrated_core" = "Xyes"; then
ac_configure_args="$ac_configure_args --with-cxxflags='$CXXFLAGS'"
else
CHECK_CPP11()
CHECK_BOOST()
CHECK_MPI_PARALLEL()
fi

CHECK_PYTHON()

AM_CONDITIONAL(HAVE_REGEXP, false)

# Check thread local storage
#CHECK_TLS()

# Check what's available and what's requested for
# thread context switching of applications
CHECK_THREADING()

# Check for whether to support event calendar optimizations
CHECK_EVENT_CALENDAR()

# MiniMD uses atomic builtins that may not be there.  We fake it if needed.  
CHECK_ATOMICS()

# Check what debugging features to enable
# and what debugging features to disable for performance
CHECK_DEBUG()

# Define the default environment (serial,thread parallel,mpi parallel,etc)
AH_TEMPLATE([DISTRIBUTED_MEMORY], [Define to indicate distributed memory])
AH_TEMPLATE([DEFAULT_ENV_STRING], [Define to indicate default environment type (mpi/serial)])
AH_TEMPLATE([DEFAULT_RUNTIME_STRING], [Define to indicate default runtime type (mpi/serial)])
AH_TEMPLATE([DEFAULT_EVENT_MANAGER_STRING], [Define to indicate default event manager (event map/clock cycler)])
AH_TEMPLATE([DEFAULT_PARTITION_STRING], [Define to indicate default partitioning strategy])

CHECK_REPO_BUILD([sstmac])

CHECK_MPI_IMPLEMENTATION()

CHECK_OTF2()

CHECK_MPI_TIMELINE_STATS()

CHECK_THREAD_PARALLEL()

#CHECK_FORTRAN()

CHECK_BACKTRACE_VIZ()

CHECK_INTEGER_TYPES()

CHECK_STL_REPLACEMENT_HEADERS()

CHECK_COMM_SYNC_STATS()

CHECK_CLANG_LIBTOOLING()

#-------------------------------------------------
# Configure subdirs
#-------------------------------------------------

ac_configure_args="$ac_configure_args --enable-sst --enable-sst-subrepo"

# Configure sprockit
AC_CONFIG_SUBDIRS([sprockit])

# Configure dumpi.
AC_CONFIG_SUBDIRS([sst-dumpi])

AC_CONFIG_SUBDIRS([sumi])

#-------------------------------------------------
# Custom new.  Make sure this is last otherwise 
# configure tests will fail.
#-------------------------------------------------
CHECK_CUSTOM_NEW()

#-------------------------------------------------
# Finalize 
#-------------------------------------------------

dnl Don't reorder options.
dnl AC_PRESERVE_HELP_ORDER

AC_CONFIG_FILES([
 Makefile
 sstmac/Makefile
 sstmac/install/Makefile
 sstmac/dumpi_util/Makefile
 sstmac/libraries/blas/Makefile
 sstmac/libraries/sumi/Makefile
 sstmac/libraries/pthread/Makefile
 sstmac/libraries/machines/Makefile
 sstmac/libraries/uq/Makefile
 sstmac/libraries/Makefile
 sstmac/skeletons/Makefile
 sstmac/software/Makefile
 sstmac/hardware/Makefile
 sstmac/sst_core/Makefile
 sstmac/backends/Makefile
 sstmac/backends/common/Makefile
 sstmac/backends/native/Makefile
 sstmac/backends/mpi/Makefile
 sstmac/main/Makefile
 sstmac/common/Makefile
 sstmac/replacements/Makefile
 sstmac/clang_replacements/Makefile
 bin/Makefile
 python/Makefile
 configurations/Makefile
 tests/Makefile
 tests/external/Makefile
 tests/unit_tests/Makefile
 tests/unit_tests/util/Makefile
 tests/sumi/Makefile
 tests/api/mpi/Makefile
 tests/api/globals/Makefile
 docs/doxygen.cfg
 sumi-mpi/Makefile
])

AC_CONFIG_FILES([bin/sstmacro-config], [chmod +x bin/sstmacro-config])
AC_CONFIG_FILES([bin/sstccvars.py])

AC_OUTPUT

if test "X$with_mpiparallel" = "Xtrue"; then
  yesno_mpiparallel="yes"
else
  yesno_mpiparallel="no"
fi

# --------- Print out a configuration summary. ---------
echo
echo "SST Macroscale Configuration Summary:"
echo "---------------------------------------------------------------"
echo "Install prefix     $prefix"
echo "OS                 $os"
echo "C compiler         $CC"
echo "C++ compiler       $CXX"
echo "CFLAGS             $CFLAGS"
echo "CXXFLAGS           $CXXFLAGS"
echo "CPPFLAGS           $CPPFLAGS"
echo "LDFLAGS            $LDFLAGS"
echo

echo "User Space Threading:"
echo "  GNU Pth          $enable_pth"
echo "  Pthread          $enable_pthread"
echo "  Ucontext         $enable_ucontext"
echo

echo "Parallel Discrete Event Simulation:"
echo "  MPI PDES         $yesno_mpiparallel"
echo "  Multithreaded    $with_multithread"
echo "  Spinlock         $with_spinlock"
echo "  Thread Affinity  $with_cpu_affinity"
echo

echo "SST Core           $have_integrated_core"
echo "Clang Autoskeleton $found_clang"
echo "MPI Timeline Stats $enable_mpi_timeline"
echo "MPI Sync Stats     $with_comm_sync_stats"
echo "Graphviz Trace     $enable_graphviz"
echo "Sanity Checking    $enable_sanity_check"

echo "---------------------------------------------------------------"


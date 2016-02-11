#
#   This file is part of SST/macroscale: 
#                The macroscale architecture simulator from the SST suite.
#   Copyright (c) 2011 Sandia Corporation.
#   This software is distributed under the BSD License.
#   Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
#   the U.S. Government retains certain rights in this software.
#   For more information, see the LICENSE file in the top 
#   SST/macroscale directory.
#

if INTEGRATED_SST_CORE
noinst_LTLIBRARIES = libsstmac.la
else
lib_LTLIBRARIES = libsstmac.la
libsstmac_la_LDFLAGS = -version-info @SSTMAC_LIBVERSION@
endif

libsstmac_la_SOURCES =
nodist_EXTRA_libsstmac_la_SOURCES = dummy.cxx
libsstmac_la_LIBADD = \
  ../../sumi/sumi/libsumi.la \
  ../backends/common/libsstmac_backends.la \
  ../backends/native/libsstmac_native.la \
  ../common/libsstmac_common.la \
  ../dumpi_util/libsstmac_dumpi.la \
  ../hardware/libsstmac_hw.la \
  ../libraries/machines/libsstmac_machines.la \
  ../libraries/pthread/libsstmac_pthread.la \
  ../libraries/sockets/libsstmac_sockets.la \
  ../libraries/sumi/libsstmac_sumi.la \
  ../main/libsstmac_driver.la \
  ../skeletons/libsstmac_skeletons.la \
  ../software/libsstmac_sw.la  

if WITH_DEFAULT_MPI
libsstmac_la_LIBADD += ../libraries/mpi/libsstmac_mpi.la 
else
libsstmac_la_LIBADD += ../../sumi-mpi/libsstmac_mpi.la
endif

if USE_MPIPARALLEL
libsstmac_la_LIBADD += ../backends/mpi/libsstmac_mpi_runtime.la 
endif


SUBDIRS = $(STD_SUBDIRS) 
DIST_SUBDIRS = $(STD_SUBDIRS) 



if INTEGRATED_SST_CORE
lib_LTLIBRARIES = libmacro.la
libsstmac_la_LIBADD += ../sst_core/libsstmac_sst_core.la
libmacro_la_SOURCES =
nodist_EXTRA_libmacro_la_SOURCES = dummy.cxx

libmacro_la_LIBADD = \
  ../../sprockit/sprockit/libsprockit.la \
  libsstmac.la

libmacro_la_LDFLAGS = -module -shared -version-info @SSTMAC_LIBVERSION@

if EXTERNAL_BOOST
  libmacro_la_LDFLAGS += $(BOOST_LDFLAGS)
  libmacro_la_LDFLAGS += $(BOOST_REGEX_LIB)
endif

endif


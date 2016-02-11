#
#   This file is part of SST/macroscale: 
#                The macroscale architecture simulator from the SST suite.
#   Copyright (c) 2009 Sandia Corporation.
#   This software is distributed under the BSD License.
#   Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
#   the U.S. Government retains certain rights in this software.
#   For more information, see the LICENSE file in the top 
#   SST/macroscale directory.
#

include $(top_srcdir)/Makefile.common

library_includedir=$(includedir)/sstmac/backends/mpi
noinst_LTLIBRARIES = libsstmac_mpi_runtime.la
libsstmac_mpi_runtime_la_SOURCES =
nobase_library_include_HEADERS =

if USE_MPIPARALLEL
nobase_library_include_HEADERS += \
    mpi_runtime.h

libsstmac_mpi_runtime_la_SOURCES += \
    mpi_runtime.cc
endif


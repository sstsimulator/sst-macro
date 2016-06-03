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

library_includedir=$(includedir)/sstmac/sst_core

# Depend on all libraries for now
SUBDIRS = $(STD_SUBDIRS) 
DIST_SUBDIRS = $(STD_SUBDIRS) 


if INTEGRATED_SST_CORE
noinst_LTLIBRARIES = libsstmac_sst_core.la

libsstmac_sst_core_la_LDFLAGS = 

if EXTERNAL_BOOST
  libsstmac_sst_core_la_LDFLAGS += $(BOOST_LDFLAGS)
endif

nobase_library_include_HEADERS = \
  connectable_wrapper.h \
  integrated_component.h \
  integrated_core.h

# micro pieces, baseclasses, etc needed for linking in the rest of sstmac
libsstmac_sst_core_la_SOURCES =  \
  connectable_wrapper.cc \
	python_topology.cc \
  integrated_component.cc \
  integrated_core.cc
endif


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

EXTRA_DIST = sstmacro.py

nobase_library_include_HEADERS = \
  connectable_wrapper.h \
  integrated_component.h \
  integrated_core.h

if INTEGRATED_SST_CORE
noinst_LTLIBRARIES = libsstmac_sst_core.la

BUILT_SOURCES = sstmacro.inc

# This sed script converts 'od' output to a comma-separated list of byte-
# values, suitable for #include'ing into an array definition.
# This can be done much more simply with xxd or hexdump, but those tools
# are not installed by default on all supported platforms.
#
# od:	-v:		Print all data
#		-t x1:	Print as byte-values, in hex
# sed:	Script 1:  Remove base-address column from od output
# 		Script 2:  Remove trailing blank line resulting from script 1
# 		Script 3:  Add '0x' prefix, and ',' suffix to each value
%.inc: %.py
	od -v -t x1 < $< | sed -e 's/^[^ ]*[ ]*//g' -e '/^\s*$$/d' -e 's/\([0-9a-f]*\)[ $$]*/0x\1,/g' > $@

libsstmac_sst_core_la_LDFLAGS = 

if EXTERNAL_BOOST
  libsstmac_sst_core_la_LDFLAGS += $(BOOST_LDFLAGS)
endif


# micro pieces, baseclasses, etc needed for linking in the rest of sstmac
libsstmac_sst_core_la_SOURCES =  \
  partitioner.cc \
	python_topology.cc \
  integrated_component.cc \
  integrated_core.cc
endif


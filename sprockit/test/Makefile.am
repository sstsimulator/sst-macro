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


if !INTEGRATED_SST_CORE
check_PROGRAMS = test_refcount test_serialize

test_refcount_SOURCES = test_refcount.cc 
test_refcount_LDADD = \
  ../sprockit/libsprockit.la

test_serialize_SOURCES = test_serialize.cc 
test_serialize_LDADD = \
  ../sprockit/libsprockit.la


if EXTERNAL_BOOST
AM_LDFLAGS = $(BOOST_LDFLAGS)
AM_LDFLAGS += $(BOOST_REGEX_LIB)
endif

check-local: 
	$(top_srcdir)/bin/runtest 2 test_serialize.out ./test_serialize
	$(top_srcdir)/bin/runtest 2 test_refcount.out ./test_refcount
endif



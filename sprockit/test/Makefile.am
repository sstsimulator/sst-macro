#
#   This file is part of SST/macroscale:
#                The macroscale architecture simulator from the SST suite.
#   Copyright (c) 2009-2020 NTESS.
#   This software is distributed under the BSD License.
#   Under the terms of Contract DE-NA0003525 with NTESS,
#   the U.S. Government retains certain rights in this software.
#   For more information, see the LICENSE file in the top
#   SST/macroscale directory.
#

include $(top_srcdir)/Makefile.common


if !INTEGRATED_SST_CORE
check_PROGRAMS = test_serialize

test_serialize_SOURCES = test_serialize.cc 
test_serialize_LDADD = \
  ../sprockit/libsprockit.la

check-local: 
	./test_serialize notime
endif



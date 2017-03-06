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

library_includedir=$(includedir)/sstmac/main

library_include_HEADERS = \
    sstmac.h \
    driver.h

noinst_LTLIBRARIES = libsstmac_driver.la 
lib_LTLIBRARIES = libsstmac_main.la


if REPO_BUILD
../../sstmac_repo.h: $(abs_top_srcdir)/.git/refs/heads/@git_branch@
	cd $(abs_top_srcdir) && $(abs_top_srcdir)/bin/config_tools/make_repo_header $(abs_top_srcdir) sstmac

sstmac.lo: ../../sstmac_repo.h
endif

libsstmac_main_la_SOURCES = main.cc

libsstmac_driver_la_SOURCES = \
    driver.cc \
    sstmac.cc \
    util.cc \
    help.cc \
    valid_keywords.cc \
    params.cc \
    parseopts.cc


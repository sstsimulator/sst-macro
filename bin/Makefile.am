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

dist_bin_SCRIPTS = aprun \
dumpibreakdown \
fix_style \
readsymbols \
sstmain \
sst++ \
dsst++ \
sstcc \
dsstcc \
sstcclib.py \
check_repo_build \
get_os \
get_include_path \
get_pthread_mutex_vars \
get_pthread_cond_vars

if INTEGRATED_SST_CORE
dist_bin_SCRIPTS += pysstmac
endif

bin_SCRIPTS = sstmacro-config sstccvars.py

EXTRA_DIST = sstmacro-config.in sstcclib.py sstccvars.py.in runtest checktest noop

AM_CPPFLAGS += -I$(top_builddir)/pth -I$(top_srcdir)/pth -I$(top_builddir)/bin \
  -I$(top_srcdir)/bin -I$(top_builddir)/dumpi -I$(top_srcdir)/dumpi

if !INTEGRATED_SST_CORE
bin_PROGRAMS = sstmac sstmac_top_info

sstmac_SOURCES = sstmac_dummy_main.cc
sstmac_top_info_SOURCES = top_info.cc

exe_LDADD = \
    ../sprockit/sprockit/libsprockit.la \
    ../sstmac/main/libsstmac_main.la \
    ../sstmac/install/libsstmac.la

if EXTERNAL_BOOST
  exe_LDADD += $(BOOST_LDFLAGS)
endif

sstmac_LDADD = $(exe_LDADD)
sstmac_top_info_LDADD = $(exe_LDADD)

python_includedir=$(includedir)/sstmac
nobase_python_include_HEADERS = \
    pysst/__init__.py \
    pysst/append.py \
    pysst/collect.py \
    pysst/compile.py \
    pysst/refactor.py \
    pysst/fakepath.py \
    pysst/fakeparse.py \
    pysst/var.py \
    pysst/fake.py \
    pysst/gui.py \
    pysst/header.py \
    pysst/keyword.py \
    pysst/util.py \
    pysst/parse.py \
    pysst/path.py \
    pysst/scope.py \
    pysst/sstglobals.py \
    pysst/standardize.py 

uninstall-local:
	test -f $(includedir)/sstmac/pysst/__init__.pyc ; if [[ $$? -eq 0 ]] ; then rm $(includedir)/sstmac/pysst/*.pyc; fi

endif




#
#   This file is part of SST/macroscale: 
#                The macroscale architecture simulator from the SST suite.
#   Copyright (c) 2009-2018 NTESS.
#   This software is distributed under the BSD License.
#   Under the terms of Contract DE-NA0003525 with NTESS,
#   the U.S. Government retains certain rights in this software.
#   For more information, see the LICENSE file in the top 
#   SST/macroscale directory.
#

include $(top_srcdir)/Makefile.common

bin_PROGRAMS = 

dist_bin_SCRIPTS = \
findHeaders \
sst++ \
libsst++ \
dsst++ \
sstcc \
libsstcc \
dsstcc \
sstcclib.py \
configlib.py

if INTEGRATED_SST_CORE
dist_bin_SCRIPTS += pysstmac
endif

bin_SCRIPTS = sstmacro-config sstccvars.py

EXTRA_DIST = sstmacro-config.in sstccvars.py.in tools config_tools manifest uq plots

AM_LDFLAGS += -ldl

AM_CPPFLAGS += \
 -I$(top_builddir)/bin \
 -I$(top_srcdir)/bin \
 -I$(top_builddir)/dumpi \
 -I$(top_srcdir)/dumpi

if HAVE_OTF2
AM_LDFLAGS += $(OTF2_LDFLAGS)
endif

if !INTEGRATED_SST_CORE
bin_PROGRAMS += sstmac sstmac_top_info

sstmac_SOURCES = src/sstmac_dummy_main.cc
sstmac_top_info_SOURCES = src/top_info.cc

exe_LDADD = \
    ../sprockit/sprockit/libsprockit.la \
    ../sstmac/main/libsstmac_main.la \
    ../sstmac/install/libsstmac.la

if EXTERNAL_BOOST
  exe_LDADD += $(BOOST_LDFLAGS)
endif


sstmac_LDADD = $(exe_LDADD) -ldl 
sstmac_top_info_LDADD = $(exe_LDADD)
endif

EXTRA_DIST += clang

if HAVE_CLANG
bin_PROGRAMS += sstmac_clang
sstmac_clang_SOURCES = \
  clang/util.cc \
  clang/main.cc \
  clang/pragmas.cc \
  clang/computePragma.cc \
  clang/computeVisitor.cc \
  clang/replacePragma.cc \
  clang/astConsumers.cc \
  clang/frontendActions.cc \
  clang/astVisitor.cc 

sstmac_clang_CXXFLAGS = -fno-rtti $(AM_CXXFLAGS)
sstmac_clang_CPPFLAGS = $(CLANG_CPPFLAGS) $(AM_CPPFLAGS)
sstmac_clang_LDFLAGS = $(CLANG_LDFLAGS) \
 -Wl,-rpath,@CLANG_INSTALL_DIR@/lib \
 -lclangFrontend \
 -lclangFrontendTool \
 -lclangTooling \
 -lclangBasic \
 -lclangASTMatchers \
 -lclangFormat \
 -lclangFrontend \
 -lclangDriver \
 -lclangParse \
 -lclangSerialization \
 -lclangSema \
 -lclangEdit \
 -lclangAnalysis \
 -lclangToolingCore \
 -lclangAST \
 -lclangRewrite \
 -lclangLex \
 -lclangBasic \
 @CLANG_LIBTOOLING_LIBS@ \
 @CLANG_LIBTOOLING_SYSTEM_LIBS@ 
endif






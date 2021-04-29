#
#   This file is part of SST/macroscale: 
#                The macroscale architecture simulator from the SST suite.
#   Copyright (c) 2009-2021, NTESS.
#   This software is distributed under the BSD License.
#   Under the terms of Contract DE-NA0003525 with NTESS,
#   the U.S. Government retains certain rights in this software.
#   For more information, see the LICENSE file in the top 
#   SST/macroscale directory.
#

include $(top_srcdir)/Makefile.common

bin_PROGRAMS = 

dist_bin_SCRIPTS = \
  sst++ \
  sstcc \
  sstcclib.py \
  sstccutils.py \
  sstcompile.py \
  sstlink.py \
  configlib.py

if INTEGRATED_SST_CORE
dist_bin_SCRIPTS += pysstmac
endif

bin_SCRIPTS = sstmacro-config sstccvars.py

EXTRA_DIST = sst++.in sstcc.in sstmacro-config.in sstccvars.py.in tools config_tools manifest uq plots

AM_LDFLAGS += -ldl

AM_CPPFLAGS += \
 -I$(top_builddir)/bin \
 -I$(top_srcdir)/bin \
 -I$(top_builddir)/dumpi \
 -I$(top_srcdir)/dumpi


if !INTEGRATED_SST_CORE
bin_PROGRAMS += sstmac sstmac_top_info

sstmac_SOURCES = src/sstmac_dummy_main.cc
sstmac_top_info_SOURCES = src/top_info.cc

exe_LDADD =

#if HAVE_OTF2
#  exe_LDADD += ../sst-dumpi/dumpi/libotf2dump/libotf2dump.la 
#  exe_LDADD += $(OTF2_LDFLAGS) $(OTF2_LIBS)
#endif

exe_LDADD += \
    ../sprockit/sprockit/libsprockit.la \
    ../sstmac/main/libsstmac_main.la \
    ../sstmac/install/libsstmac.la \
    ../sprockit/sprockit/libsprockit.la

if HAVE_OTF2
  exe_LDADD += ../sst-dumpi/dumpi/libotf2dump/libotf2dump.la
endif

if HAVE_OTF2
  exe_LDADD += ../sst-dumpi/dumpi/libotf2dump/libotf2dump.la 
#  exe_LDADD += $(OTF2_LDFLAGS) $(OTF2_LIBS)
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
  clang/astVisitorGlobalVars.cc \
  clang/pragmas.cc \
  clang/computePragma.cc \
  clang/computeVisitor.cc \
  clang/replacePragma.cc \
  clang/astConsumers.cc \
  clang/frontendActions.cc \
  clang/astVisitor.cc 


sstmac_clang_CXXFLAGS = -fno-rtti $(AM_CXXFLAGS)
sstmac_clang_CPPFLAGS = $(CLANG_CPPFLAGS) $(AM_CPPFLAGS)

if HAVE_CXX17
sstmac_clang_SOURCES += \
	clang/annotatePragma.cc \
	clang/memoizePragma.cc \
	clang/memoizeVariableCaptureAnalyzer.cc \
	clang/ompPuppetizePragma.cc \
	clang/memoizeVariable.cc

old_standards=-std=c++11 -std=c++0x -std=c++1y -std=c++14 -std=c++1z
libclang17_a_CXXFLAGS = $(filter-out $(old_standards), $(sstmac_clang_CXXFLAGS))

library_includedir=$(includedir)/memoization
library_include_HEADERS = clang/memoization/capture.h
endif

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

# TODO Figure out this was included
# if CLANG_NEED_LIBCPP
# sstmac_clang_LDFLAGS += -lclang-cpp
# endif

endif




if HAVE_SST_ELEMENTS
bin_PROGRAMS += ariel_test
ariel_test_SOURCES = ariel_test.cc
ariel_test_LDADD = -lrt
ariel_test_CPPFLAGS = $(SST_ELEMENTS_CPPFLAGS) $(SST_CPPFLAGS)
endif




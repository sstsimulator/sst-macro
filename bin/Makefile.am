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

dist_bin_SCRIPTS = \
sst++ \
dsst++ \
sstcc \
dsstcc \
sstcclib.py 

if INTEGRATED_SST_CORE
dist_bin_SCRIPTS += pysstmac
endif

bin_SCRIPTS = sstmacro-config sstccvars.py

EXTRA_DIST = sstmacro-config.in sstcclib.py sstccvars.py.in tools config_tools

AM_CPPFLAGS += \
 -I$(top_builddir)/bin \
 -I$(top_srcdir)/bin \
 -I$(top_builddir)/dumpi \
 -I$(top_srcdir)/dumpi

if !INTEGRATED_SST_CORE
bin_PROGRAMS = sstmac sstmac_top_info

sstmac_SOURCES = src/sstmac_dummy_main.cc
sstmac_top_info_SOURCES = src/top_info.cc

exe_LDADD = \
    ../sprockit/sprockit/libsprockit.la \
    ../sstmac/main/libsstmac_main.la \
    ../sstmac/install/libsstmac.la

if EXTERNAL_BOOST
  exe_LDADD += $(BOOST_LDFLAGS)
endif

sstmac_LDADD = $(exe_LDADD)
sstmac_top_info_LDADD = $(exe_LDADD)

EXTRA_DIST += \
 clang/util.h \
 clang/pragmas.h \
 clang/astConsumers.h \
 clang/clangheaders.h \
 clang/firstAstVisitor.h \
 clang/findAstVisitor.h \
 clang/foundMacro.h \
 clang/frontendActions.h \
 clang/globalVarNamespace.h \
 clang/replAstVisitor.h

if HAVE_CLANG
bin_PROGRAMS += sstmac_clang_deglobal
sstmac_clang_deglobal_SOURCES = \
  clang/util.cc \
  clang/globals.cc \
  clang/pragmas.cc \
  clang/astConsumers.cc \
  clang/firstVisitor.cc \
  clang/findAstVisitor.cc \
  clang/frontendActions.cc \
  clang/replAstVisitor.cc 

sstmac_clang_deglobal_CXXFLAGS = -fno-rtti
sstmac_clang_deglobal_CPPFLAGS = $(CLANG_CPPFLAGS)
sstmac_clang_deglobal_LDFLAGS = $(CLANG_LDFLAGS) \
 -Wl,-rpath,$(HOME)/Programs/install/clang-llvm/lib \
 -lclang \
 -lz \
 -lm \
 -lcurses \
 -lclangFrontend \
 -lclangFrontendTool \
 -lLLVMSupport \
 -lclangTooling \
 -lclangBasic \
 -lclangASTMatchers \
 -lclangFormat \
 -lclangFrontend \
 -lclangDriver \
 -lclangParse \
 -lLLVMMCParser \
 -lclangSerialization \
 -lclangSema \
 -lclangEdit \
 -lclangAnalysis \
 -lLLVMBitReader \
 -lLLVMProfileData \
 -lclangToolingCore \
 -lclangAST \
 -lclangRewrite \
 -lclangLex \
 -lclangBasic \
 -lLLVMCore \
 -lLLVMMC \
 -lLLVMOption \
 -lLLVMSupport \
 -lLLVMDemangle
endif

uninstall-local:
	test -f $(includedir)/sstmac/pysst/__init__.pyc ; if [[ $$? -eq 0 ]] ; then rm $(includedir)/sstmac/pysst/*.pyc; fi

endif




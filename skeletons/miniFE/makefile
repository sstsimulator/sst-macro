#-----------------------------------------------------------------------

MINIFE_TYPES =  \
        -DMINIFE_SCALAR=double   \
        -DMINIFE_LOCAL_ORDINAL=int      \
        -DMINIFE_GLOBAL_ORDINAL=int

MINIFE_MATRIX_TYPE = -DMINIFE_CSR_MATRIX
# MINIFE_MATRIX_TYPE = -DMINIFE_ELL_MATRIX

#-----------------------------------------------------------------------

CFLAGS = -O3
CXXFLAGS = -O3

# For debugging, the macro MINIFE_DEBUG will cause miniFE to dump a log file
# from each proc containing various information.
# This macro will also enable a somewhat expensive range-check on indices in
# the exchange_externals function.

# CPPFLAGS = -I. -I../utils -I../fem $(MINIFE_TYPES) -DMINIFE_DEBUG -DHAVE_MPI -DMPICH_IGNORE_CXX_SEEK
CPPFLAGS =   -I. -I../utils -I../fem $(MINIFE_TYPES) $(MINIFE_MATRIX_TYPE) -DHAVE_MPI -DMPICH_IGNORE_CXX_SEE

# The MPICH_IGNORE_CXX_SEEK macro is required for some mpich versions,
# such as the one on my cygwin machine.

CXX=mpicxx
CC=mpicc

ifneq ($(NOSSTMAC),y)
CXX := sst++    
CC := sstcc     
CXXFLAGS :=
CPPFLAGS +=-DUSING_SSTMAC 
LIBDIR :=  
PREFIX :=
LDFLAGS := -Wl,-rpath,$(PREFIX)/lib 
SSTGEN = 
SSTOBJ = $(SSTGEN:%.cc=%.o)

ifeq ($(LOOP),y)
CPPFLAGS += -D_USE_LOOP_MODEL
endif
endif


# set profiling option. default is off. Settable from command line as
# make clean; make LWPERF=some_value
LWPERF =EIGER
LWPERF =FAKEEIGER
LWPERF =CSV
LWPERF =

# set eiger root directory
EIGERINCLUDE = /usr/local/include/eiger
EIGERLIBDIR = /usr/local/lib
LWPDIR = $(HOME)/eiger/lwperf/src
# compute perf files and rules
ifneq ($(LWPERF),)
LWTOOLINC = -I$(LWPDIR) -I.
LWTOOLINC += -DLWP_DBNAME='"minife"' -I$(EIGERINCLUDE)
LWPGEN = InitFuncs.h LocationElements.h InitSwitchElements.h cperf._log.h cperf._save.h cperf._stop.h
LWPSRC = perf.cpp
ifeq ($(LWPERF),CSV)
LWPSRC += csvbackend.cpp 
LWTOOLINC += -D_USE_CSV 
endif
ifeq ($(LWPERF),EIGER)
LWPSRC += eigerbackend.cpp 
EIGERLIB = -L$(EIGERLIBDIR) -leigerInterface 
# add eiger build flags
LWTOOLINC += -D_USE_EIGER 
endif
ifeq ($(LWPERF),FAKEEIGER)
LWPSRC += eigerbackend.cpp 
EIGERLIB = -L$(EIGERLIBDIR) -lmpifakeeigerInterface 
LWTOOLINC += -D_USE_FAKEEIGER 
endif
ifeq ($(LWPERF),MODEL)
LWTOOLINC += -D_USE_EIGER_MODEL
endif
LWPOBJ = $(LWPSRC:.cpp=.o)
endif
CXXFLAGS += $(LWTOOLINC)
LIBS += $(EIGERLIB)
VPATH += $(LWPDIR)

include make_targets
include ./optional/make_targets

# helper rules
instrument:
	$(MAKE) NOSSTMAC=y LWPERF=FAKEEIGER

cleaninstrument:
	$(MAKE) NOSSTMAC=y LWPERF=FAKEEIGER clean

nosstmac:
	$(MAKE) NOSSTMAC=y

cleannosstmac:
	$(MAKE) NOSSTMAC=y clean

loop:
	$(MAKE) LOOP=y

cleanloop:
	$(MAKE) LOOP=y clean

eiger:
	$(MAKE) LWPERF=MODEL

cleaneiger:
	$(MAKE) LWPERF=MODEL clean

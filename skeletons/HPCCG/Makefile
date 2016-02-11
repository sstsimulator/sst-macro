TARGET = runHPCCG
SRC := $(shell ls *.cpp) 

CXX=mpicxx.openmpi
CPPFLAGS = -g

ifneq ($(NOSSTMAC),y)
SRC := $(shell ls *.cpp) 
CXX :=  sst++
CC :=   sstcc
CXXFLAGS := 
CPPFLAGS := 
LIBDIR :=   
PREFIX := 
LDFLAGS :=   -Wl,-rpath,$(PREFIX)/lib
SSTGEN = 

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
LWTOOLINC += -DLWP_DBNAME='"hpccg"' -I$(EIGERINCLUDE)
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
SRC += $(LWPSRC)
OBJ := $(SRC:.cc=.o) 
OBJ := $(OBJ:.cpp=.o)
OBJ := $(OBJ:.cxx=.o)
OBJ := $(OBJ:.c=.o)

VPATH += $(LWPDIR)

.PHONY: clean install fake cleanfake sstmac cleansstmac

all: $(TARGET)

$(TARGET): $(OBJ) 
	$(CXX) -o $@ $+ $(LDFLAGS) $(LIBS) $(CXXFLAGS)

%.o: %.cpp $(LWPGEN)
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

%.o: %.cxx
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

%.o: %.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

clean: 
	$(RM) -f $(TARGET) $(OBJ) $(LWPOBJ) $(LWPGEN) *_mpi_*.log $(SSTGEN)

install: $(TARGET)
	cp $< $(PREFIX)/bin

$(LWPGEN): $(SRC)
	$(LWPDIR)/updateLoggersCXX.sh $(SRC)

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

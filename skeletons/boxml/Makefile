HAVE_RUNTIME := false
HAVE_METIS := false

TARGET := runboxml
LIBTARGET := libboxml.a
SSTMAC=$(HOME)/install/sst-macro-boxml
BOOST=$(HOME)/src/boost_1_59_0

SRC := boxml-data.cc boxml-run.cc boxml-xml.cc lock_array.cc boxml.cc tinyxml2.cc

CXX = $(SSTMAC)/bin/sst++
CC  = $(SSTMAC)/bin/sstcc
#CXX = mpicxx
#CC = mpicc

# Optionally add -DBOXML_STD_UNORDERED (requires C++11 compiler)
# # Optionally add -DBOXML_HAVE_METIS
 CPPFLAGS += -I. -DSSTMAC_HAVE_BOOST=1 -I$(BOOST)
# #CPPFLAGS += -I. -DSSTMAC_HAVE_BOOST=1 -I$(BOOST) -I$(HOME)/install/sst-macro-boxml/include/sumi
# #CPPFLAGS += -I. -DSSTMAC_HAVE_BOOST=1 -I$(BOOST) -I$(HOME)/install/parmetis-4.0.3/include -I$(HOME)/install/sst-macro-boxml/include/sumi -I/Users/jpkenny/src/rank-placer
# #CPPFLAGS += -I. -DSSTMAC_HAVE_BOOST=1 -DBOXML_HAVE_TEST -Dno_miclib -Dno_affinity -I$(BOOST) -I$(HOME)/src/hpc-task-pareto-frontiers/runtime -I$(HOME)/install/sst-macro-boxml/include/sstmac -I$(HOME)/install/sst-macro-boxml/include -I$(HOME)/install/sst-macro-boxml/include/sumi
#
 CXXFLAGS += -g -O2 -std=c++11
# #LDFLAGS := -Wl,-rpath,$(SSTMAC)/lib -L$(HOME)/install/parmetis-4.0.3/lib -lmetis /Users/jpkenny/src/rank-placer/librankplacer.a
# #LDFLAGS := -Wl,-rpath,$(SSTMAC)/lib

ifeq ($(HAVE_RUNTIME),true)
  SRC += boxml-taskdag.cc
endif
ifeq ($(HAVE_METIS),true)
  SRC += boxml-partition.cc
endif

OBJ := $(SRC:.cc=.o)

.PHONY: clean install

all: $(TARGET)

$(TARGET): $(OBJ)
	$(CXX) -o $@ $+ $(LDFLAGS) $(LIBS)

$(LIBTARGET): $(OBJ)
	ar rvs $(LIBTARGET) $(OBJ)

%.o: %.cc containers.h boxml.h
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

clean: 
	rm -f $(TARGET) $(OBJ) 

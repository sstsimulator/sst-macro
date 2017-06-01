HAVE_RUNTIME := false
HAVE_METIS := false

TARGET := runboxml
LIBTARGET := libboxml.a

SRC := boxml-data.cc boxml-run.cc boxml-xml.cc lock_array.cc boxml.cc tinyxml2.cc

CXX = sst++
CC  = sstcc

# Optionally add -DBOXML_STD_UNORDERED (requires C++11 compiler)
# # Optionally add -DBOXML_HAVE_METIS
CPPFLAGS += -I. -fPIC
# #CPPFLAGS += -I. -DBOXML_HAVE_TEST -Dno_miclib -Dno_affinity -I$(HOME)/src/hpc-task-pareto-frontiers/runtime -I$(HOME)/install/sst-macro-boxml/include/sstmac -I$(HOME)/install/sst-macro-boxml/include -I$(HOME)/install/sst-macro-boxml/include/sumi
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
	SSTMAC_SRC2SRC=0 $(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

clean: 
	rm -f $(TARGET) $(OBJ) 

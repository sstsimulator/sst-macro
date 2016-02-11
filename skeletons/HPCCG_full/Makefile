TARGET = runHPCCG
SRC := $(shell ls *.cpp) 
CXX :=  sst++
CC :=   sstcc
CXXFLAGS := 
CPPFLAGS := -DUSING_MPI
LIBDIR :=   
PREFIX := 
LDFLAGS :=   -Wl,-rpath,$(PREFIX)/lib
SSTGEN = 

CPPFLAGS += -D_USE_LOOP_MODEL

OBJ := $(SRC:.cc=.o) 
OBJ := $(OBJ:.cpp=.o)
OBJ := $(OBJ:.cxx=.o)
OBJ := $(OBJ:.c=.o)

VPATH += $(LWPDIR)

.PHONY: clean install 

all: $(TARGET)

$(TARGET): $(OBJ) 
	$(CXX) -o $@ $+ $(LDFLAGS) $(LIBS) $(CXXFLAGS)

%.o: %.cpp 
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



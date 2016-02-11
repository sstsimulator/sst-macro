TARGET := runsstmac
SRC := $(shell ls *.f90) 

# uncomment and fill this in to build the real mpi program
#FC := mpif90

# the sstmacro-config script must be found in PATH
# comment these out to build the real app
CXX      := sst++
CC      := sstcc
CXXFLAGS := 
CPPFLAGS := -m32 -m64 $(shell sstmacro-config --cppflags)
FC       := $(shell sstmacro-config --fc )
FCFLAGS  := $(shell sstmacro-config --fcflags )
FCLIBS   := $(shell sstmacro-config --fclibs )
PREFIX :=   $(shell sstmacro-config --prefix )
LDFLAGS :=  -Wl,-rpath,$(PREFIX)/lib

OBJ := $(SRC:.f90=.o) c_link.o

$(TARGET): $(OBJ) 
	$(CXX) $^ $(LDFLAGS) $(CXXFLAGS) $(LIBS) $(FCLIBS) -o $@

c_link.o: c_link.cc
	$(CXX) $(CPPFLAGS) $(CXXFAGS) -c $< -o $@

%.o: %.f90
	$(FC) $(FCFLAGS) $(CPPFLAGS) -c $< -o $@

.PHONY: clean install

clean: 
	rm -f $(TARGET) *.o

install: $(TARGET)
	cp $< $(PREFIX)/bin


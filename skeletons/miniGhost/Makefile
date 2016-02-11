TARGET := runsstmac


# uncomment and fill this in to build the real mpi program
#FC := mpif90

# the sstmacro-config script must be found in PATH
# comment these out to build the real app
CXX      := sst++
CC := sstcc
CXXFLAGS := 
CPPFLAGS := -m64
FC       := $(shell sstmacro-config --fc )
FCFLAGS  := $(shell sstmacro-config --fcflags ) -x f95-cpp-input -D_INT4 -D_REAL8 -D_OSX_MPICH -D_TIMER_MPI -D_MPI 
FCLIBS   := $(shell sstmacro-config --fclibs )
LDFLAGS :=  -Wl,-rpath,$(PREFIX)/lib -fPIC 

OBJS_F = MG_CONSTANTS.o MG_OPTIONS.o MG_UTILS.o MG_PROFILING.o \
	MG_CHECKPOINT.o \
	MG_GET_FACE.o \
	MG_BUFINIT.o MG_PACK.o MG_IRECV.o MG_UNPACK.o \
	MG_BSEND.o MG_ISEND.o \
	MG_SEND.o \
        MG_PACK_SVAF.o \
	MG_BSEND_SVAF.o MG_ISEND_SVAF.o \
	MG_SEND_SVAF.o \
	MG_UNPACK_AGG.o \
	MG_BSPMA.o MG_SVAF.o \
	MG_STENCIL_COMPS.o MG_STENCIL.o \
        MG_BSPMA_DIAGS.o \
        MG_SVCP_NODIAGS_INIT.o MG_SVCP_DIAGS_INIT.o MG_SVCP_INIT.o \
	MG_SVCP_3D7PT.o MG_SVCP_3D27PT.o \
	MG_SVCP.o \
	MG_ALLREDUCE.o MG_SUM_GRID.o \
	DRIVER_BSPMA.o DRIVER_SVAF.o DRIVER_SVCP.o \
	DRIVER.o

	
OBJS_C = main.o 

$(TARGET): $(OBJS_F) $(OBJS_C)
	$(CXX) $^ $(LDFLAGS) $(CXXFLAGS) $(LIBS) $(FCLIBS) -o $@

%.o: %.f90
	$(FC) $(FCFLAGS) $(CPPFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@
	
%.o: %.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@
	
.PHONY: clean install

clean: 
	rm -f $(TARGET) *.o *.mod

install: $(TARGET)
	cp $< $(PREFIX)/bin


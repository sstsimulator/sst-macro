# Makefile for src-openmp

SHELL = /bin/bash

SKEL = 1

### CoMD can be built in either double or single precision and with or
### without MPI.  Select desired precision and MPI here.

# double precision (ON/OFF)
DOUBLE_PRECISION = ON
# MPI for parallel (ON/OFF)
DO_MPI = ON

CoMD_VARIANT=openmp-mpi

### Set your desired C compiler and any necessary flags.  Note that CoMD
### uses some c99 features.  You can also set flags for optimization and
### specify paths to include files that the compiler can't find on its
### own.  If you need any -L or -l switches to get C standard libraries
### (such as -lm for the math library) put them in C_LIB.

#CC = mpicc
CC = sstcc
OPTFLAGS = -O3

CFLAGS = -std=c99 -fPIC
INCLUDES = 
C_LIB = -lm

#CC = gcc
#CFLAGS += -fopenmp
#DO_MPI=OFF


### If you need to specify include paths, library paths, or link flags
### for MPI, put them here.  Put both -L and -l switches into MPI_LIB.
MPI_LIB =
MPI_INCLUDE =

### A place to specify any other include or library switches your
### platform requires.
OTHER_LIB =
OTHER_INCLUDE =




#########################################
### Below here, it is pitch black.  
### You are likely to be eaten by a grue.
##########################################

# clear all suffixes
.SUFFIXES:
# list only those that we use 
.SUFFIXES: .c .o

.PHONY: DEFAULT clean distclean depend

# Check for double precision
ifeq ($(DOUBLE_PRECISION), ON)
CFLAGS += -DDOUBLE
else
CFLAGS += -DSINGLE
endif

# Set executable name and add includes & libraries for MPI if needed.
ifeq ($(DO_MPI), ON)
INCLUDES += ${MPI_INCLUDE}
CFLAGS += -DDO_MPI
LDFLAGS += ${MPI_LIB} 
endif
CoMD_EXE = runCoMD

LDFLAGS += ${C_LIB} ${OTHER_LIB}
CFLAGS  += ${OPTFLAGS} ${INCLUDES} ${OTHER_INCLUDE}


SOURCES=\
 src/CoMD.c \
 src/cmdLineParser.c \
 src/decomposition.c \
 src/eam.c \
 src/haloExchange.c \
 src/initAtoms.c \
 src/linkCells.c \
 src/ljForce.c \
 src/mycommand.c \
 src/parallel.c \
 src/performanceTimers.c \
 src/random.c \
 src/timestep.c \
 src/yamlOutput.c

OBJECTS=$(SOURCES:.c=.o)


DEFAULT: ${CoMD_EXE}

sstmac_headers:
	$(shell find `pwd`/src -name "*.h" > $@)

%.o: %.c sstmac_headers
	SSTMAC_SKELETONIZE=$(SKEL) SSTMAC_HEADERS=`pwd`/sstmac_headers \
   ${CC} ${CFLAGS} -c $< -o $@

${CoMD_EXE}: CoMD_info.h ${OBJECTS} 
	${CC} ${CFLAGS} -o ${CoMD_EXE} ${OBJECTS} ${LDFLAGS}

libcomd.so: CoMD_info.h ${OBJECTS} 
	@echo "Linking SST core .so"
	${CC} ${CFLAGS} -o $@ ${OBJECTS} ${LDFLAGS}

CoMD_info.h: Makefile
	./generate_info_header ${CoMD_VARIANT} "$(CC)" "$(CFLAGS)" "$(LDFLAGS)"

clean:
	rm -f src/*.o CoMD_info.h .depend ${CoMD_EXE} src/.depend.bak .depend.bak src/.depend sstmac_headers libcomd.so

distclean: clean
	rm -f ${CoMD_EXE} .depend.bak
	rm -rf html latex

.depend: $(SOURCES)
	@touch .depend
	@$(MAKE) --no-print-directory depend

depend:
	@echo "Rebuilding dependencies..."
	@$(MAKE) CoMD_info.h
	@makedepend -f .depend -Y. --$(CFLAGS)-- $(SOURCES) 2> /dev/null


-include .depend

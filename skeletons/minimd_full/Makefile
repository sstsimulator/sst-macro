# Multiple-machine Makefile

SHELL = /bin/sh

# Files

SRC =	ljs.cpp input.cpp integrate.cpp atom.cpp force_lj.cpp force_eam.cpp neighbor.cpp \
	thermo.cpp comm.cpp timer.cpp output.cpp setup.cpp
INC =	ljs.h atom.h force.h neighbor.h thermo.h timer.h comm.h integrate.h threadData.h variant.h openmp.h \
	force_lj.h force_eam.h types.h

# Definitions

ROOT =	runminimd_full
#EXE =	$(ROOT)_$@
EXE =	$(ROOT)
OBJ =	$(SRC:.cpp=.o)

all:
	$(MAKE) sstmac

# Help

help:
	@echo 'Type "make target" where target is one of:'
	@echo '      openmpi     (using OpenMPI)'
	@echo '      openmpi-dbg (using OpenMPI + DBG)'
	@echo '      intel64     (using iMPI for x86_64)'
	@echo '      intelMIC    (using iMPI for MIC)'
	@echo '      plain       (vanilla mpi build)'
	@echo '      instrument  (with lwperf instrumentation)'
	@echo '      sstmac      (for running in sstmacro)'
	@echo '      loop        (for running in sstmacro with loop models)'
	@echo '      eiger       (for running in sstmacro with Eiger models)'

# Targets

openmpi:
	@if [ ! -d Obj_$@ ]; then mkdir Obj_$@; fi
	@cp -p $(SRC) $(INC) Obj_$@
	@cp Makefile.$@ Obj_$@/Makefile
	@cd Obj_$@; \
	$(MAKE)  "OBJ = $(OBJ)" "INC = $(INC)" "EXE = ../$(EXE)" ../$(EXE)
#	@if [ -d Obj_$@ ]; then cd Obj_$@; rm $(SRC) $(INC) Makefile*; fi

openmpi-dbg:
	@if [ ! -d Obj_$@ ]; then mkdir Obj_$@; fi
	@cp -p $(SRC) $(INC) Obj_$@
	@cp Makefile.$@ Obj_$@/Makefile
	@cd Obj_$@; \
	$(MAKE)  "OBJ = $(OBJ)" "INC = $(INC)" "EXE = ../$(EXE)" ../$(EXE)
#       @if [ -d Obj_$@ ]; then cd Obj_$@; rm $(SRC) $(INC) Makefile*; fi

intel64:
	@if [ ! -d Obj_$@ ]; then mkdir Obj_$@; fi
	@cp -p $(SRC) $(INC) Obj_$@
	@cp Makefile.$@ Obj_$@/Makefile
	@cd Obj_$@; \
	$(MAKE)  "OBJ = $(OBJ)" "INC = $(INC)" "EXE = ../$(EXE)" ../$(EXE)
#       @if [ -d Obj_$@ ]; then cd Obj_$@; rm $(SRC) $(INC) Makefile*; fi


intelMIC:
	@if [ ! -d Obj_$@ ]; then mkdir Obj_$@; fi
	@cp -p $(SRC) $(INC) Obj_$@
	@cp Makefile.$@ Obj_$@/Makefile
	@cd Obj_$@; \
	$(MAKE)  "OBJ = $(OBJ)" "INC = $(INC)" "EXE = ../$(EXE)" ../$(EXE)
#       @if [ -d Obj_$@ ]; then cd Obj_$@; rm $(SRC) $(INC) Makefile*; fi

plain:
	@if [ ! -d Obj_$@ ]; then mkdir Obj_$@; fi
	@cp -p $(SRC) $(INC) Obj_$@
	@cp Makefile.all Obj_$@/Makefile
	@cd Obj_$@; \
	$(MAKE) "SRC = $(SRC)" "OBJ = $(OBJ)" "INC = $(INC)" "EXE = ../$(EXE)" ../$(EXE)
#       @if [ -d Obj_$@ ]; then cd Obj_$@; rm $(SRC) $(INC) Makefile*; fi

instrument:
	@if [ ! -d Obj_$@ ]; then mkdir Obj_$@; fi
	@cp -p $(SRC) $(INC) Obj_$@
	@cp Makefile.all Obj_$@/Makefile
	@cd Obj_$@; \
	$(MAKE) "SRC = $(SRC)" "OBJ = $(OBJ)" "INC = $(INC)" "EXE = ../$(EXE)" ../$(EXE) LWPERF=FAKEEIGER
#       @if [ -d Obj_$@ ]; then cd Obj_$@; rm $(SRC) $(INC) Makefile*; fi

sstmac:
	@if [ ! -d Obj_$@ ]; then mkdir Obj_$@; fi
	@cp -p $(SRC) $(INC) Obj_$@
	@cp Makefile.all Obj_$@/Makefile
	@cd Obj_$@; \
	$(MAKE) "SRC = $(SRC)" "OBJ = $(OBJ)" "INC = $(INC)" "EXE = ../$(EXE)" ../$(EXE) SSTMAC=y
#       @if [ -d Obj_$@ ]; then cd Obj_$@; rm $(SRC) $(INC) Makefile*; fi

loop:
	@if [ ! -d Obj_$@ ]; then mkdir Obj_$@; fi
	@cp -p $(SRC) $(INC) Obj_$@
	@cp Makefile.all Obj_$@/Makefile
	@cd Obj_$@; \
	$(MAKE) "SRC = $(SRC)" "OBJ = $(OBJ)" "INC = $(INC)" "EXE = ../$(EXE)" ../$(EXE) SSTMAC=y LOOP=y
#       @if [ -d Obj_$@ ]; then cd Obj_$@; rm $(SRC) $(INC) Makefile*; fi

eiger:
	@if [ ! -d Obj_$@ ]; then mkdir Obj_$@; fi
	@cp -p $(SRC) $(INC) Obj_$@
	@cp Makefile.all Obj_$@/Makefile
	@cd Obj_$@; \
	$(MAKE) "SRC = $(SRC)" "OBJ = $(OBJ)" "INC = $(INC)" "EXE = ../$(EXE)" ../$(EXE) SSTMAC=y LWPERF=MODEL
#       @if [ -d Obj_$@ ]; then cd Obj_$@; rm $(SRC) $(INC) Makefile*; fi

# Clean

clean:
	rm -rf Obj_* $(EXE)

clean_openmpi:
	rm -r Obj_openmpi

clean_openmpi-dbg:
	rm -r Obj_openmpi-dbg

clean_intel64:
	rm -r Obj_intel64

clean_intelMIC:
	rm -r Obj_intelMIC

clean_plain:
	rm -r Obj_plain

clean_instrument:
	rm -r Obj_instrument

clean_sstmac:
	rm -r Obj_sstmac

clean_loop:
	rm -r Obj_loop

clean_eiger:
	rm -r Obj_eiger

# Test

test:
	scope=0
	force=lj
	halfneigh=0
	path=""
	bash run_tests ${scope} ${force} ${halfneigh} ${path}   

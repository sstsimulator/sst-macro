# Makefile for mpicxx compiler

SHELL = /bin/sh
#.IGNORE:

# System-specific settings

CC =		mpicxx
CCFLAGS =	-O3 -mavx -DMPICH_IGNORE_CXX_SEEK -fopenmp  -DPRECISION=2 -g
LINK =		mpicxx 
LINKFLAGS =	-O3 -mavx -fopenmp -g
USRLIB = 	-lrt
SYSLIB =	-lpthread
SIZE =		size

# Link rule

$(EXE):	$(OBJ)
	$(LINK) $(LINKFLAGS) $(OBJ) $(USRLIB) $(SYSLIB) -o $(EXE)
	$(SIZE) $(EXE)

# Compilation rules

.cpp.o:
	$(CC) $(CCFLAGS) -c $<

# Individual dependencies

$(OBJ): $(INC)

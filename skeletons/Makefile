
TARGETS= \
bootmachine \
gtc \
HPCCG \
HPCCG_full \
HPCCG_skel \
miniFE \
miniGhost \
location_trace \
lu \
lulesh \
minimd \
minimd_full \
mpi3d \
osu \
sockets \
sweep3d \
systolicmxm

TESTS = $(TARGETS:%=test_%)
CLEANERS = $(TARGETS:%=clean_%)

all: $(TESTS)

.PHONY: clean

clean: $(CLEANERS)
		
test_%: 
	cd $*; $(MAKE)
	touch $@

clean_%: 
	cd $*; $(MAKE) clean
	rm -f test_*


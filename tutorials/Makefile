
TARGETS= \
1d_integrator_c \
sendrecv_c \
sendrecv_cxx \
sendrecv_cxx2 

TESTS = $(TARGETS:%=test_%)

.phony: clean

all: $(TESTS)

CLEANERS = $(TARGETS:%=clean_%)
clean: $(CLEANERS)
clean_%: 
	cd $*; $(MAKE) clean

test_%: 
	cd $*; $(MAKE)
	touch $@


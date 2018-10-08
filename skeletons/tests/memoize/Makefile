TARGET := libmemoize_sleep.so libskeleton_sleep.so

CXX :=    sst++
CC :=     sstcc
CXXFLAGS := -fPIC
CPPFLAGS := -I.
LIBDIR :=   
PREFIX :=   
LDFLAGS :=  -Wl,-rpath,$(PREFIX)/lib

.PHONY: clean install 

all: $(TARGET)

libmemoize_sleep.so: memoize.cc code.h
	SSTMAC_MEMOIZE=1 $(CXX) -shared $(LIBS) $(CPPFLAGS) $(CXXFLAGS) $< -o $@

libskeleton_sleep.so: skeleton.cc code.h
	SSTMAC_SKELETONIZE=1 $(CXX) -shared $(LIBS) $(CPPFLAGS) $(CXXFLAGS) $< -o $@

clean: 
	rm -f $(TARGET) 

install: $(TARGET)
	cp $< $(PREFIX)/bin


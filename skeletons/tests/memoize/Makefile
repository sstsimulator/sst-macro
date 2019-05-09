TARGET := runmemoize runskeletonize

CXX :=    sst++
CC :=     sstcc
CXXFLAGS := -fPIC
CPPFLAGS := -I.
LIBDIR :=   
PREFIX :=   
LDFLAGS :=  -Wl,-rpath,$(PREFIX)/lib

.PHONY: clean install 

all: $(TARGET)

runmemoize: memoize.cc code.h
	$(CXX) --memoize $(LIBS) $(CPPFLAGS) $(CXXFLAGS) $< -o $@

runskeletonize: skeleton.cc code.h
	$(CXX) --skeletonize $(LIBS) $(CPPFLAGS) $(CXXFLAGS) $< -o $@

clean: 
	rm -f $(TARGET) 

install: $(TARGET)
	cp $< $(PREFIX)/bin


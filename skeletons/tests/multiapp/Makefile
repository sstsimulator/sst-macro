TARGET := runallgather runallreduce
SRC := allgather.cc allreduce.cc

CXX :=    sst++
CC :=     sstcc
CXXFLAGS := -fPIC
CPPFLAGS := -I.
LIBDIR :=  
PREFIX := 
LDFLAGS :=  -Wl,-rpath,$(PREFIX)/lib 

OBJ := $(SRC:.cc=.o) 
OBJ := $(OBJ:.cpp=.o)
OBJ := $(OBJ:.c=.o)

.PHONY: clean install 

all: $(TARGET)

run%: %.o
	$(CXX) -o $@ $+ $(LDFLAGS) $(LIBS)  $(CXXFLAGS)

%.o: %.cc 
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

clean: 
	rm -f $(TARGET) *.o

install: $(TARGET)
	cp $< $(PREFIX)/bin


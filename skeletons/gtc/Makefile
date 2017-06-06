TARGET := rungtc
SRC := $(shell ls *.cc) 

COMPUTE := -D_USE_LOOPS

CXX := sst++
CC :=  sstcc
CXXFLAGS := -fPIC
CPPFLAGS := -I. $(COMPUTE)
LIBDIR :=   
PREFIX := install
LDFLAGS := -Wl,-rpath,$(PREFIX)/lib

OBJ := $(SRC:.cc=.o) 
OBJ := $(OBJ:.cpp=.o)
OBJ := $(OBJ:.c=.o)

.PHONY: clean install 

all: $(TARGET)

$(TARGET): $(OBJ) 
	$(CXX) -o $@ $+ $(LDFLAGS) $(LIBS) 

%.o: %.cc 
	SSTMAC_SRC2SRC=0 $(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

%.o: %.c
	SSTMAC_SRC2SRC=0 $(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

clean: 
	rm -f $(TARGET) $(OBJ) 

install: $(TARGET)
	cp $< $(PREFIX)/bin


TARGET := runboxml
SRC := $(shell ls *.cc)
SSTMAC=$(HOME)/install/sstmacro-debug

BOOST=$(HOME)/src/boost_1_59_0

CXX = $(SSTMAC)/bin/sst++
CC  = $(SSTMAC)/bin/sstcc
CPPFLAGS += -I. -DSSTMAC_HAVE_BOOST=1 -I$(BOOST)

# Optionally add -DBOXML_STD_UNORDERED (requires C++11 compiler)
CXXFLAGS += -g
LDFLAGS := -Wl,-rpath,$(SSTMAC)/lib 

OBJ := $(SRC:.cc=.o) 
OBJ := $(OBJ:.cpp=.o)
OBJ := $(OBJ:.c=.o)

.PHONY: clean install 

all: $(TARGET)

$(TARGET): $(OBJ) 
	$(CXX) -o $@ $+ $(LDFLAGS) $(LIBS)

%.o: %.cc
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

%.o: %.c containers.h boxml.h
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

clean: 
	rm -f $(TARGET) $(OBJ) 

install: $(TARGET)
	cp $< $(PREFIX)/bin


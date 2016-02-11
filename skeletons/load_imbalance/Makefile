TARGET := runsst
SRC := $(shell ls *.cc) 

CXX :=  sst++
CC :=   sstcc
CXXFLAGS := 
CPPFLAGS := -I. -DUSE_CUSTOM_NEW -include sprockit/spkt_new.h
LIBDIR :=   
PREFIX :=   $(shell sstmacro-config --prefix )
LDFLAGS :=  -Wl,-rpath,$(PREFIX)/lib -pg

OBJ := $(SRC:.cc=.o) 
OBJ := $(OBJ:.cpp=.o)
OBJ := $(OBJ:.c=.o)

.PHONY: clean install 

all: $(TARGET)

$(TARGET): $(OBJ) 
	$(CXX) -o $@ $+ $(LDFLAGS) $(LIBS) 

%.o: %.cc 
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

clean: 
	rm -f $(TARGET) $(OBJ) 

install: $(TARGET)
	cp $< $(PREFIX)/bin


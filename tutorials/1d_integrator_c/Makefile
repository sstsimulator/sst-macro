TARGET := runsstmac
SRC := $(shell ls *.c) 

CXX :=      sst++
CC :=       sstcc
CXXFLAGS := 
CPPFLAGS := -I. 
LIBDIR :=   
PREFIX := install
LDFLAGS := -Wl,-rpath,$(PREFIX)/lib

OBJ := $(SRC:.cc=.o) 
OBJ := $(OBJ:.cpp=.o)
OBJ := $(OBJ:.c=.o)

.PHONY: clean install 

all: $(TARGET)

$(TARGET): $(OBJ) 
	$(CC) -o $@ $+ $(LDFLAGS) $(LIBS) 

%.o: %.cc 
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

clean: 
	rm -f $(TARGET) $(OBJ) 

install: $(TARGET)
	cp $< $(PREFIX)/bin


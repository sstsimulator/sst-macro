TARGET := libtest.so
SRC := component.cc

SST_CORE=$(HOME)/Programs/install/sst-core/clang

CXX :=    libsst++
CC :=     libsstcc
CXXFLAGS := -fPIC
CPPFLAGS := -I. 

#uncomment if integrated core
#CPPFLAGS += -I$(SST_CORE)/include

LIBDIR :=  
PREFIX := 
LDFLAGS :=  -Wl,-rpath,$(PREFIX)/lib

OBJ := $(SRC:.cc=.o) 
OBJ := $(OBJ:.cpp=.o)
OBJ := $(OBJ:.c=.o)

.PHONY: clean install 

all: $(TARGET)

$(TARGET): $(OBJ) 
	$(CXX) -shared -o $@ $+ $(LDFLAGS) $(LIBS)  $(CXXFLAGS)

%.o: %.cc 
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

clean: 
	rm -f $(TARGET) $(OBJ) 

install: $(TARGET)
	cp $< $(PREFIX)/bin


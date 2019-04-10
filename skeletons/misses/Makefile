TARGET := runmiss
SRC := miss.cc

CXX :=    sst++
CC :=     sstcc
CXXFLAGS := -fPIC -O3
CPPFLAGS := -I.
LIBDIR :=  
PREFIX := 
LDFLAGS :=  -Wl,-rpath,$(PREFIX)/lib 

OBJ := $(SRC:.cc=.o) 
OBJ := $(OBJ:.cpp=.o)
OBJ := $(OBJ:.c=.o)

.PHONY: clean install 

all: $(TARGET)

$(TARGET): $(OBJ) 
	SSTMAC_SKELETONIZE=1 $(CXX) -o $@ $+ $(LDFLAGS) $(LIBS)  $(CXXFLAGS)

run: $(OBJ)
	$(CXX) $(OBJ) $(LDFLAGS) -lm -o $@ 

%.o: %.cc 
	SSTMAC_SKELETONIZE=1 $(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

clean: 
	rm -f $(TARGET) $(OBJ) 

install: $(TARGET)
	cp $< $(PREFIX)/bin


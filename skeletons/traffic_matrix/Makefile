TARGET := runtraffic
SRC := $(shell ls *.cc) 

COMPUTE := -D_USE_LOOPS
DHARMA=$(HOME)/Programs/install/dharma/sst

CXX := sst++
CC :=  sstcc
CXXFLAGS := 
CPPFLAGS := -I. $(COMPUTE) -I$(DHARMA)/include
LIBS := -L$(DHARMA)/lib -ldharma_api -Wl,-rpath,$(DHARMA)/lib 
LDFLAGS := 

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


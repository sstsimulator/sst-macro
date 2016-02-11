TARGET := runsst
SRC := test_scan.cc

CXX :=    dsst++
CC :=     dsstcc
CXXFLAGS := -fPIC
CPPFLAGS := -I.
LIBDIR :=   
PREFIX :=   
LDFLAGS :=  -Wl,-rpath,$(PREFIX)/lib -lmain -L.

OBJ := $(SRC:.cc=.o) 
OBJ := $(OBJ:.cpp=.o)
OBJ := $(OBJ:.c=.o)

.PHONY: clean install 

all: $(TARGET)

libmain.so: main.o
	lsst++ -o main main.o 

main.o: main.cc 
	lsst++ $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

$(TARGET): $(OBJ) libmain.so
	$(CXX) -o $@ $+ $(LDFLAGS) $(LIBS) 

%.o: %.cc 
	$(CXX) $(CPPFLAGS) $(CXXFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

clean: 
	rm -f $(TARGET) $(OBJ) *.so *.o main

install: $(TARGET)
	cp $< $(PREFIX)/bin


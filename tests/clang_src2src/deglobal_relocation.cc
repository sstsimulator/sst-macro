
int x;

struct y {
  int* ptr;
};

struct y yinst = {
  &x
};

struct y yArr[] = {
  { &x },
  { 0 }
};


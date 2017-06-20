
template <class T>
T* allocate();

void fxn1()
{
#pragma sst init 5
  int x = 0;
#pragma sst init nullptr
  int* y = new int[10];
#pragma sst init (int*)nullptr
  auto z = allocate<int>();
}



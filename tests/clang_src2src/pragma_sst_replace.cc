
template <class T>
T* allocate();

void fxn1()
{
#pragma sst start_replace allocate nullptr
  auto x = allocate<int>();
  auto y = allocate<int>();
#pragma sst stop_replace allocate nullptr
  auto z = allocate<int>();
}

#pragma sst replace allocate nullptr
void fxn2()
{
  auto x = allocate<int>();
  auto y = allocate<int>();
  auto z = allocate<int>();
}

int compute(){ return 0; }

void fxn3()
{
  int a[4];
#pragma sst replace a 5
  for (int i=0; i < 3; ++i){
    int x = a[i];
  }
#pragma sst replace compute 10
  for (int i=0; i < 3; ++i){
    int x = compute();
  }

  int b[10][10];
#pragma sst replace b 42
  int x = b[0][0];

#pragma sst replace compute 10
#pragma sst replace b 42
  for (int i=0; i < 3; ++i){
    int x = compute();
    int z = b[3][2];
  }
}


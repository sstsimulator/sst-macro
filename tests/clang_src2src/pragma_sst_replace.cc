
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


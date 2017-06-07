
int fxn()
{
  int i=0;
#pragma sst compute
  for (i=0; i < 5; ++i){
    for (int j=0; j < 10; ++j){
#pragma sst replace count 6
      int count = 5;
    }
  }
  return 0;
}


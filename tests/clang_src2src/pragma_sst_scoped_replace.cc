
int fxn()
{
  int i=0;
#pragma sst compute
  for (i=0; i < 5; ++i){
    int count = i;
#pragma sst replace count 6
    for (int j=0; j < count; ++j){
    }
  }
#pragma sst compute
  for (i=0; i < 5; ++i){
#pragma sst replace count 6
    int count = i;
    for (int j=0; j < count; ++j){
    }
  }
  return 0;
}



void fxn(int x)
{
}

static void (**fxn_list)(int);

void next_fxn()
{
  fxn_list = (void(**)(int)) new char[100*sizeof(*fxn_list)];
  fxn_list[0] = fxn;
}


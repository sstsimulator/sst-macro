
void fxn(int x)
{
}

extern void (*callbacks[])(int input);

void first_fxn()
{
  callbacks[0] = fxn;
}

void (*callbacks[4])(int input);

void second_fxn()
{
  callbacks[0] = fxn;
}

extern void(*more_callbacks[][2])(int input);

void third_fxn()
{
  more_callbacks[0][1] = fxn;
}

void (*more_callbacks[4][2])(int input);

void fourth_fxn()
{
  more_callbacks[0][1] = fxn;
}

typedef void(*func)(int);

func moar_callbacks[4];

void fifth_fxn()
{
  moar_callbacks[0] = fxn;
}



void fxn()
{
  static int x = 10;
  x += 5;

  static int y;
}

void next_fxn()
{
  static int y = 7;
  {
    static int y = 8;
    y -= 1;
  }
  y += 3;
}

int z = 0;

void fxn2()
{
  static struct ayz {
    int* y;
    int z;
  } a = { &z, 2 };
}

void fxn3()
{
  static struct {
    int y;
    int z;
  } a = { .y = 1, .z = 10 };
}


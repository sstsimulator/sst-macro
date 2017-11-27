
void fxn()
{
  static int x = 10;
  x += 5;
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


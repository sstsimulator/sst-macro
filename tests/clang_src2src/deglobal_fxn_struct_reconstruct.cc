
void fxn()
{
  struct boo {
    int a;
    int x[3];
  };
  static boo bird;

  static struct ugh {
    int a;
    boo x;
  } uggs[2];

  static struct again {
    ugh a[1];
    boo d[5];
  } yo;
}


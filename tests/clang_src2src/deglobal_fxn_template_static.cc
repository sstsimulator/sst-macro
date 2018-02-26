

template <class T>
class A {
 public:
  A(int x);
};

template <class T>
void fxn()
{
  int init = 0;
  static int x = 10;
  static A<T> a(0);
  static A<T> b(init);
}

void next_fxn()
{
  fxn<int>();
}


#ifndef ASSERT_H
#define ASSERT_H

class DefaultCTFailure {};

template <bool t, typename msg = DefaultCTFailure>
struct FailCompileTime {
  enum { N = 1 - 2 * int(t) };
  static char A[N];
};

template <class T>
class InvalidContainer {};

template <class T>
void invalidContainer(const T& t)
{
  FailCompileTime<true, InvalidContainer<T> > check;
}




#endif // ASSERT_H


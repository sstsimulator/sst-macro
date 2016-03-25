#ifndef EQUALITY_H
#define EQUALITY_H

#include <vector>
#include <cmath>

template <class T>
inline bool arrays_equal(size_t n, const T* test, const T* right);


template <class T>
class TestEquals
{

 public:
  static bool equals(size_t n, const T* test, const T* right) {
    return arrays_equal<T>(n, test, right);
  }

  static bool equals(const T& test, const T& right) {
    return test == right;
  }

};



template <> class TestEquals<float>
{

 public:
  static float cutoff;

  static bool equals(float test, float right) {
    // do a relative difference - normalize for large numbers
    return (fabs(test - right) / fabs(right)) < cutoff;
  }

  static bool equals(size_t n, const float* test, const float* right) {
    return arrays_equal<float>(n, test, right);
  }

};



template <> class TestEquals<double>
{

 public:
  static double cutoff;

  static bool equals(double test, double right) {
    // do a relative difference - normalize for large numbers
    return (fabs(test - right) / fabs(right)) < cutoff;
  }

};

template <class T> class TestEquals< std::vector<T> >
{

 public:
  static bool equals(const std::vector<T>& test, const std::vector<T>& asserted) {
    if (test.size() != asserted.size()) {
      return false;
    }

    typename std::vector<T>::const_iterator it, end = test.end();
    typename std::vector<T>::const_iterator ia = asserted.begin();
    for (it=test.begin(); it != end; ++ia, ++it) {
      if ( !(TestEquals<T>::equals(*it, *ia)) ) {
        return false;
      }
    }
    return true;
  }
};

template <class T>
inline bool arrays_equal(size_t n, const T* test, const T* right)
{
  for (size_t i=0; i < n; ++i) {
    if ( !(TestEquals<T>::equals(test[i], right[i])) ) {
      return false;
    }
  }
  return true;
}




#endif // EQUALITY_H


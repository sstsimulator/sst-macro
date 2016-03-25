#ifndef OUTPUT_H
#define OUTPUT_H

#include <vector>
#include <map>
#include <ostream>

//Default string output for unit tests
template <class T> class ClassOutput
{

 public:
  static void
  print(const T& t, std::ostream& os) {
    os << t;
  }

};

template <class T>
class ClassOutput< std::vector<T> >
{

 public:
  static void
  print(const std::vector<T>& test, std::ostream& os) {
    os << "{ ";
    typename std::vector<T>::const_iterator
    it, begin = test.begin(), end = test.end();
    for (it=begin; it != end; ++it) {
      if (it != begin) {
        os << ",";
      }
      ClassOutput<T>::print(*it, os);
    }
    os << " }";
  }
};

template <class T, class U>
class ClassOutput< std::map<T, U> >
{

 public:
  static void
  print(const std::map<T, U>& test, std::ostream& os) {
    os << "map {\n";
    typename std::map<T,U>::const_iterator
    it, begin = test.begin(), end = test.end();
    for (it=begin; it != end; ++it) {
      if (it != begin) {
        os << ",";
      }
      ClassOutput<T>::print(it->first);
      os << "->";
      ClassOutput<T>::print(it->second);
    }
    os << "   }";
  }

};

#endif // OUTPUT_H


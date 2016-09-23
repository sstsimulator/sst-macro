#ifndef STL_STRING_H
#define STL_STRING_H

#include <set>
#include <vector>
#include <list>
#include <map>
#include <sstream>

template <class Container>
std::string
one_dim_string(const Container& c, const char* open, const char* close){
  std::stringstream sstr;
  sstr << open;
  for (auto& item : c){
    sstr << " " << item;
  }
  sstr << " " << close;
  return sstr.str();
}

template <class T>
std::string
stl_string(const std::set<T>& t){
  return one_dim_string(t, "{", "}");
}

template <class T>
std::string
stl_string(const std::vector<T>& t){
  return one_dim_string(t, "[", "]");
}

template <class T>
std::string
stl_string(const std::list<T>& t){
  return one_dim_string(t, "<", ">");
}

#endif // STL_STRING_H

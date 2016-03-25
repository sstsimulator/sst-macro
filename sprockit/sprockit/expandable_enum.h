#ifndef EXPANDABLE_ENUM_H
#define EXPANDABLE_ENUM_H

#include <map>
#include <sprockit/serialize.h>

struct expandable_enum {
int value;
};

#define declare_expandable_enum(clsname) \
struct clsname : public expandable_enum { \
explicit clsname(int val) {value = val;} \
explicit clsname() {value = 0;} \
static int enum_count; \
static std::map<int, const char*>* names; \
static clsname register_enum(const char* name){ \
  if (!names){ \
    names = new std::map<int, const char*>; \
  } \
  (*names)[enum_count] = name; \
  return clsname(enum_count++); \
} \
const char* \
tostr() const { \
  return names->find(value)->second; \
} \
}

#define implement_enum_functions(clsname) \
inline bool \
operator==(const clsname& e1, const clsname& e2){ \
  return e1.value == e2.value; \
} \
\
inline bool \
operator<(const clsname& e1, const clsname& e2){ \
  return e1.value < e2.value; \
} \
\
template <class T> \
std::ostream& \
operator<<(std::ostream& os, const clsname& e){ \
  os << clsname::names->find(e.value)->second; \
  return os; \
}


#define ImplementEnum(enum_type) \
  int enum_type::enum_count = 0; \
  std::map<int, const char*>* enum_type::names = 0

#define RegisterEnum(enum_type, name) \
  enum_type name = enum_type::register_enum(#name)


namespace sprockit {

template <>
class serialize<expandable_enum> {
 public:
  void operator()(expandable_enum &t, serializer& ser){
    ser & t.value;
  }
};

}


#endif // EXPANDABLE_ENUM_H

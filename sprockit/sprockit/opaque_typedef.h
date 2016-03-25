#ifndef sprockit_common_OPAQUE_TYPEDEF_H
#define sprockit_common_OPAQUE_TYPEDEF_H

#include <sstream>
#include <sprockit/serialize.h>
#include <sprockit/spkt_config.h>

namespace sprockit {

struct opaque_int {
  static const int uninitialized = -1;
};

/**
    @class opaque_type
    Class used for opaque typedefs of integers
    to ensure that only a specific type is used.
    This is meant to hold unsigned types.
    However, -1 is also a valid value indicating
    uninitialized.
*/
template <class int_type>
class opaque_type
{
 public:
  typedef int_type int_t;
  int_type id_;

  opaque_type(int_type v) : id_(v) {
  }
};

template <class T>
class serialize<opaque_type<T> > {
 public:
  void operator()(opaque_type<T>& o, serializer& ser){
    ser & o.id_;
  }
};

} // end namespace sprockit

#if SPKT_HAVE_CPP11
#include <functional>
namespace std {
template <typename int_type>
struct hash<sprockit::opaque_type<int_type> >
{
  size_t operator()(sprockit::opaque_type<int_type> const& key) const {
    return key.id_;
  }
};
}
#endif

#define typedef_opaque_int(cls_type, int_type) \
struct cls_type : \
    public sprockit::opaque_type<int_type> \
{ \
    explicit cls_type(int_type v = ::sprockit::opaque_int::uninitialized) : ::sprockit::opaque_type<int_type>(v) {} \
    inline cls_type& operator++() { \
      ++id_; \
      return *this; \
    } \
    inline cls_type operator++(int) { \
      cls_type other(*this); \
      ++id_; \
      return other; \
    } \
    operator int_type() const { \
        return id_; \
    } \
    std::string \
    to_string() const { \
        std::stringstream sstr; \
        sstr << #cls_type "(" << id_ << ")"; \
        return sstr.str(); \
    } \
} \

#define implement_opaque_int(cls_type) \
inline bool \
operator==(const cls_type &a, const cls_type &b) { \
    return (a.id_ == b.id_); \
} \
inline bool \
operator!=(const cls_type &a, const cls_type &b) { \
    return (a.id_ != b.id_); \
} \
inline bool \
operator<(const cls_type &a, const cls_type &b) { \
    return (a.id_ < b.id_); \
} \
inline bool \
operator<=(const cls_type &a, const cls_type &b) { \
    return (a.id_ <= b.id_); \
} \
inline bool \
operator>(const cls_type &a, const cls_type &b) { \
    return (a.id_ > b.id_); \
} \
inline bool \
operator>=(const cls_type &a, const cls_type &b) { \
    return (a.id_ >= b.id_); \
} \
inline std::ostream& \
operator<<(std::ostream &os, const cls_type &n) { \
    return (os << n.id_); \
} \
inline cls_type::int_t \
to_printf_type(const cls_type& t){ \
  return t.id_; \
}

 
#if SPKT_HAVE_CPP11
#define implement_opaque_hash(clstype) \
namespace std { \
template <> \
struct hash<clstype> \
  : public std::hash<::sprockit::opaque_type<int>> \
{ }; \
}
#else
#define implement_opaque_hash(clstype) \
namespace boost { \
inline std::size_t \
hash_value(const clstype &t) \
{ \
  return t.id_; \
} \
}
#endif




#endif // OPAQUE_TYPEDEF_H


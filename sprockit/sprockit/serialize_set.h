#ifndef SERIALIZE_SET_H
#define SERIALIZE_SET_H

#include <set>
#include <sprockit/unordered.h>
#include <sprockit/serializer.h>

namespace sprockit {

namespace pvt {

template <class Set, class T>
void
serialize_set(Set& v, serializer& ser) {
  typedef typename Set::iterator iterator;
  switch(ser.mode())
  {
  case serializer::SIZER: {
    size_t size = v.size();
    ser.size(size);
    iterator it, end = v.end();
    for (it=v.begin(); it != end; ++it){
      T& t = const_cast<T&>(*it); 
      serialize<T>()(t,ser);
    }
    break;
  }
  case serializer::PACK: {
    size_t size = v.size();
    ser.pack(size);
    iterator it, end = v.end();
    for (it=v.begin(); it != end; ++it){
      T& t = const_cast<T&>(*it); 
      serialize<T>()(t,ser);
    }
    break;
  }
  case serializer::UNPACK: {
    size_t size;
    ser.unpack(size);
    for (int i=0; i < size; ++i){
      T t;
      serialize<T>()(t,ser);
      v.insert(t);
    }
    break;
  }
  }
}

} //end ns pvt

template <class T>
class serialize<std::set<T> > {
  typedef std::set<T> Set;
 public:
  void
  operator()(Set& v, serializer& ser) {
    pvt::serialize_set<Set,T>(v,ser);
  }
};

#if !SPKT_ENABLE_ORDERED_MAP
template <class T>
class serialize<spkt_unordered_set<T> > {
  typedef spkt_unordered_set<T> Set;
 public:
  void
  operator()(Set& v, serializer& ser) {
    pvt::serialize_set<Set,T>(v,ser);
  }
};
#endif

}

#endif // SERIALIZE_SET_H

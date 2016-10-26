#ifndef SERIALIZE_H
#define SERIALIZE_H

#include <sprockit/serializer.h>
#include <sprockit/serializable_type.h>
#include <sprockit/debug.h>
#include <type_traits>

DeclareDebugSlot(serialize);

namespace sprockit {

template <class T>
class serialize {
 public:
  inline void operator()(T& t, serializer& ser){
    ser.primitive(t); 
  }
};

template <>
class serialize<bool> {
 public:
  void operator()(bool &t, serializer& ser){
    int bval = t;
    ser.primitive(bval);
    t = bool(bval);
  }
};

namespace pvt {

void
size_serializable(serializable* s, serializer& ser);

void
pack_serializable(serializable* s, serializer& ser);

void
unpack_serializable(serializable*& s, serializer& ser);

}


template <>
class serialize<serializable*> {

 public:
  void
  operator()(serializable*& s, serializer& ser)
  {
    switch (ser.mode()){
  case serializer::SIZER:
    pvt::size_serializable(s,ser);
    break;
  case serializer::PACK:
    pvt::pack_serializable(s,ser);
    break;
  case serializer::UNPACK:
    pvt::unpack_serializable(s,ser);
    break;
    }
  }

};

template <class T,bool flag>
class serialize_ptr
{
 public:
  void
  operator()(serializer& ser, T*& t){
    abort();
  }
};

template <class T>
class serialize_ptr<T,false>
{
 public:
  void
  operator()(serializer& ser, T*& t){
    ser.primitive(t);
  }
};

template <class T>
class serialize_ptr<T,true>
{
 public:
  void
  operator()(serializer& ser, T*& t){
    serializable* s = t;
    serialize<serializable*>()(s, ser);
    t = static_cast<T*>(s);
  }
};

inline void
operator&(serializer& ser, void* v){
  ser.primitive(v);
}

template <class T>
inline void
operator&(serializer& ser, T*& t){
  serialize_ptr<T,std::is_base_of<serializable,T>::value>()(ser, t);
}

template <class T>
inline void
operator&(serializer& ser, T& t){
  serialize<T>()(t, ser);
}

}

#include <sprockit/serialize_array.h>
#include <sprockit/serialize_list.h>
#include <sprockit/serialize_map.h>
#include <sprockit/serialize_set.h>
#include <sprockit/serialize_vector.h>
#include <sprockit/serialize_string.h>

#endif // SERIALIZE_H

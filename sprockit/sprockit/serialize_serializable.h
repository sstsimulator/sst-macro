#ifndef SERIALIZE_SERIALIZABLE_H
#define SERIALIZE_SERIALIZABLE_H

#include <sprockit/serializable.h>
#include <sprockit/serializer.h>
#include <sprockit/serialize.h>
#include <sprockit/ptr_type.h>

namespace sprockit {

template <class T>
void
serialize_intrusive_ptr(T*& t, serializer& ser)
{
  serializable* s = t;
  switch (ser.mode()){
case serializer::SIZER:
  pvt::size_serializable(s,ser);
  break;
case serializer::PACK:
  pvt::pack_serializable(s,ser);
  break;
case serializer::UNPACK:
  pvt::unpack_serializable(s,ser);
  t = dynamic_cast<T*>(s);
  break;  
  }  
}

template <class T>
class serialize<sprockit::refcount_ptr<T> > {
 public:
  void operator()(sprockit::refcount_ptr<T>& o, serializer& ser){
    T* tmp = o.get();
    serialize_intrusive_ptr(tmp, ser);
    o = tmp;
  }
};

template <class T>
class serialize<const sprockit::refcount_ptr<T> > {
 public:
  void operator()(const sprockit::refcount_ptr<T>& o, serializer& ser){
    T* tmp = o.get();
    serialize_intrusive_ptr(tmp, ser);
  }
};


template <class T>
class serialize<sprockit::refcount_ptr<const T> > {
 public:
  void operator()(sprockit::refcount_ptr<const T>& o, serializer& ser){
    T* tmp = const_cast<T*>(o.get());
    serialize_intrusive_ptr(tmp, ser);
    o = tmp;    
  }
};


} 

#endif // SERIALIZE_SERIALIZABLE_H

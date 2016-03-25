#ifndef SERIALIZE_LIST_H
#define SERIALIZE_LIST_H

#include <list>
#include <deque>
#include <sprockit/serializer.h>

namespace sprockit {

namespace pvt {

template <class Container, class T>
void 
serialize_container(Container& v, serializer& ser){
  typedef typename Container::iterator iterator;
  switch(ser.mode())
  {
  case serializer::SIZER: {
    size_t size = v.size();
    ser.size(size);
    iterator it, end = v.end();
    for (it=v.begin(); it != end; ++it){
      T& t = *it;
      serialize<T>()(t, ser);
    }
    break;
  }
  case serializer::PACK: {
    size_t size = v.size();
    ser.pack(size);
    iterator it, end = v.end();
    for (it=v.begin(); it != end; ++it){
      T& t = *it;
      serialize<T>()(t, ser);
    }
    break;
  }
  case serializer::UNPACK: {
    size_t size;
    ser.unpack(size);
    for (int i=0; i < size; ++i){
      T t;
      serialize<T>()(t, ser);
      v.push_back(t);
    }
    break;
  }
  }
}

}

template <class T>
class serialize <std::list<T> > {
 typedef std::list<T> List; 
public:
 void
 operator()(List& v, serializer& ser) {
   pvt::serialize_container<List,T>(v,ser);
 }
};

template <class T>
class serialize <std::deque<T> > {
 typedef std::deque<T> DQ; 
public:
 void
 operator()(DQ& v, serializer& ser) {
   pvt::serialize_container<DQ,T>(v,ser);
 }
};


}

#endif // SERIALIZE_LIST_H

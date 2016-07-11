#ifndef SERIALIZE_ARRAY_H
#define SERIALIZE_ARRAY_H

#include <sprockit/serializer.h>

namespace sprockit {
namespace pvt {

template <class TPtr, class IntType>
class ser_array_wrapper
{
 public:
  TPtr& bufptr;
  IntType& sizeptr;
  ser_array_wrapper(TPtr& buf, IntType& size) :
    bufptr(buf), sizeptr(size) {}

};

template <class TPtr>
class raw_ptr_wrapper
{
public:
    TPtr*& bufptr;
    raw_ptr_wrapper(TPtr* ptr) :
        bufptr(ptr) {}
};

}

template <class T, int N>
class serialize<T[N]> {
 public:
  void operator()(T arr[N], serializer& ser){
    ser.array<T,N>(arr);
  }
};

/** I have typedefing pointers, but no other way.
 *  T could be "void and TPtr void* */
template <class TPtr, class IntType>
pvt::ser_array_wrapper<TPtr,IntType>
array(TPtr& buf, IntType& size)
{
  return pvt::ser_array_wrapper<TPtr,IntType>(buf, size);
}

template <class TPtr>
inline pvt::raw_ptr_wrapper<TPtr>
raw_ptr(TPtr*& ptr)
{
  return pvt::raw_ptr_wrapper<TPtr>(ptr);
}

template <class TPtr, class IntType>
inline void
operator&(serializer& ser, pvt::ser_array_wrapper<TPtr,IntType> arr){
  ser.binary(arr.bufptr, arr.sizeptr);
}

// Needed only because the default version in serialize.h can't get
// the template expansions quite right trying to look through several
// levels of expansion
template <class TPtr>
inline void
operator&(serializer& ser, pvt::raw_ptr_wrapper<TPtr> ptr){
  ser.primitive(ptr.bufptr);
}

}

#endif // SERIALIZE_ARRAY_H

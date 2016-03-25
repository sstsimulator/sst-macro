#ifndef SERIALIZE_UNPACKER_H
#define SERIALIZE_UNPACKER_H

#include <sprockit/serialize_buffer_accessor.h>

namespace sprockit {
namespace pvt {

class ser_unpacker :
  public ser_buffer_accessor
{
 public:
  template <class T>
  void
  unpack(T& t){
    T* bufptr = ser_buffer_accessor::next<T>();
    t = *bufptr;
  }

  void
  unpack_buffer(void* buf, int size);

  void
  unpack_string(std::string& str);

};

} }

#endif // SERIALIZE_UNPACKER_H

#ifndef METADATA_BITS_H
#define METADATA_BITS_H

#include <stdint.h>

namespace sprockit {

template <class integer>
class metadata_bits {
  
  template <class T> friend class serialize;

public:
  bool
  bit(int bitnum) const {
    return metadata_ & (1<<bitnum);
  }

  void
  clear() {
    metadata_ = 0;
  }

  void
  set_bit(int bitnum) {
    metadata_ = metadata_ | (1<<bitnum);
  }

  void
  unset_bit(int bitnum) {
    metadata_ = metadata_ & ~(1<<bitnum);
  }

  integer
  bit_integer() const {
    return metadata_;
  }

  void
  set_bit_integer(integer i) {
    metadata_ = i;
  }

  metadata_bits() :
    metadata_(0)
  {
  }

  metadata_bits(const metadata_bits& mdata) :
    metadata_(mdata.metadata_)
  {
  }

 protected:
  integer metadata_;
};

template <typename T>
class serialize<metadata_bits<T> > {
 public:
  void operator()(metadata_bits<T> &t, serializer& ser){
    ser & t.metadata_;
  }
};

}

#endif // METADATA_BITS_H

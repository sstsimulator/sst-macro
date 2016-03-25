#ifndef sprockit_PAYLOAD_H
#define sprockit_PAYLOAD_H

namespace sprockit {

template <class T>
class clonable : public T
{
 public:
  virtual clonable<T>*
  clone() const = 0;
};

}


#endif // PAYLOAD_H

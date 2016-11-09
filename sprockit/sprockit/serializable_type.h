#ifndef SERIALIZABLE_TYPE_H
#define SERIALIZABLE_TYPE_H

#include <sprockit/serializer_fwd.h>
#include <typeinfo>
#include <stdint.h>

namespace sprockit {

class serializable
{
 public:
  virtual const char*
  cls_name() const = 0;

  virtual void
  serialize_order(sprockit::serializer& ser) = 0;

  virtual uint32_t
  cls_id() const = 0;

  virtual ~serializable() { }

 protected:
  typedef enum { ConstructorFlag } cxn_flag_t;
};

template <class T>
class serializable_type
{
};

}

#endif // SERIALIZABLE_TYPE_H

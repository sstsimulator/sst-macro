#ifndef ROUTING_ENUM_H
#define ROUTING_ENUM_H

namespace sstmac {
    namespace hw {
      namespace routing {

  static const int uninitialized = -123;

  typedef enum {
    minimal,
    minimal_adaptive,
    valiant,
    ugal,
    deflt
  } algorithm_t;

  const char*
  tostr(algorithm_t algo);

    }
  }
}

#endif // ROUTING_ENUM_H

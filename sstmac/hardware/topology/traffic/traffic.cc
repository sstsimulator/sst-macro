#include <sstmac/hardware/topology/traffic/traffic.h>
#include <sprockit/errors.h>

#define enumcase(x) case x: return #x;

namespace sstmac {
namespace hw {

const char*
traffic_pattern::tostr(type_t ty)
{
  switch(ty) {
      enumcase(nearest_neighbor);
      enumcase(tornado);
      enumcase(bit_complement);
    default:
      spkt_throw_printf(sprockit::value_error,
                       "traffic_pattern::tostr: unknown type_t enum %d",
                       ty);
  }
}

}
}


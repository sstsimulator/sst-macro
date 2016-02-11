#ifndef sstmac_hardware_network_traffic_TRAFFIC_H
#define sstmac_hardware_network_traffic_TRAFFIC_H

namespace sstmac {
namespace hw {

class traffic_pattern
{

 public:
  typedef enum {
    nearest_neighbor,
    tornado,
    bit_complement
  } type_t;

  static const char* tostr(type_t ty);

};

}
}




#endif // TRAFFIC_H


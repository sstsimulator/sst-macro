#ifndef EVENT_LOCATION_H
#define EVENT_LOCATION_H

#include <sstmac/common/node_address.h>
#include <sprockit/errors.h>

namespace sstmac {

struct device_id {
  typedef enum {
    node=0,
    router=1,
    netlink=2,
    logp_overlay=3,
    null=4
  } type_t;

  explicit device_id(uint32_t id, type_t ty) :
    location_(id), type_(ty)
  {
  }

  device_id() :
    location_(0), type_(null)
  {
  }

  type_t
  type() const {
    return type_;
  }

  uint32_t
  id() const {
    return location_;
  }

  bool
  is_node_id() const {
    return type_ == node;
  }

  bool
  is_switch_id() const {
    return type_ == router;
  }

  bool
  is_netlink_id() const {
    return type_ == netlink;
  }

 private:
  uint32_t location_;
  type_t type_;

};

inline bool
operator==(const device_id& a, const device_id& b){
  return a.id() == b.id() && a.type() == b.type();
}

inline bool
operator<(const device_id& a, const device_id& b){
  if (a.id() != b.id()) return a.id() < b.id();
  else return a.type() < b.type();
}

}

#endif // EVENT_LOCATION_H


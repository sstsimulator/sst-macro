#ifndef EVENT_LOCATION_H
#define EVENT_LOCATION_H

#include <sstmac/common/node_address.h>
#include <sprockit/errors.h>

namespace sstmac {

struct event_loc_id {
  int32_t location;
  static event_loc_id null;
  static event_loc_id uninitialized;

  event_loc_id() :
    location(uninitialized.location) {
  }

  explicit event_loc_id(topology_id id) :
    location(uint32_t(id)) {
  }

  explicit event_loc_id(endpoint_id id) :
    location(-(uint32_t(id)+1)) {
  }

  bool
  is_node_id() const {
    return location < 0;
  }

  bool
  is_switch_id() const {
    return location >= 0 && location != null.location;
  }

  node_id
  convert_to_node_id() const {
#if SSTMAC_SANITY_CHECK
    if (is_switch_id()) {
      spkt_throw(sprockit::value_error,
                "event_location::convert_to_node_id: not a node id");
    }
    else if (is_null()) {
      spkt_throw(sprockit::value_error,
                "event_location::convert_node_id: null id");
    }
#endif
    return node_id(-(location+1));
  }

  switch_id
  convert_to_switch_id() const {
#if SSTMAC_SANITY_CHECK
    if (is_null()) {
      spkt_throw(sprockit::value_error,
                "event_location::convert_switch_id: null id");
    }
    else if (is_node_id()) {
      spkt_throw(sprockit::value_error,
                "event_location::convert_to_switch_id: not a switch id");
    }
#endif
    return switch_id(location);
  }

  bool
  is_null() const {
    return location == null.location;
  }

  event_loc_id&
  operator=(topology_id id) {
    location = uint32_t(id);
    return (*this);
  }

  event_loc_id&
  operator=(endpoint_id id) {
    location = -uint32_t(id);
    return (*this);
  }

};

inline bool
operator==(const event_loc_id& a, const event_loc_id& b){
  return a.location == b.location;
}

inline bool
operator!=(const event_loc_id& a, const event_loc_id& b){
  return a.location != b.location;
}

inline bool
operator<(const event_loc_id& a, const event_loc_id& b){
  return a.location < b.location;
}

inline std::ostream&
operator<<(std::ostream& os, const event_loc_id& loc){
  if(loc.is_node_id()){
    os << loc.convert_to_node_id();
  }
  else {
    os << loc.convert_to_switch_id();
  }
  return os;
}

}

#endif // EVENT_LOCATION_H


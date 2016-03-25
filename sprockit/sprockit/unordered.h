
#ifndef SPROCKIT_SPROCKIT_UNORDERED_H_
#define SPROCKIT_SPROCKIT_UNORDERED_H_


#include <sprockit/spkt_config.h>

#if SPKT_HAVE_CPP11

#include <unordered_map>
template <typename... Args>
using spkt_unordered_map = std::unordered_map<Args...>;

#include <unordered_set>
template <typename... Args>
using spkt_unordered_set = std::unordered_set<Args...>;

namespace sprockit {
struct enum_hash
{
  template <typename T>
  inline
  typename std::enable_if<std::is_enum<T>::value, std::size_t>::type
  operator()(T const value) const {
    return static_cast<std::size_t>(value);
  }
};
}

template <typename Key, typename Value>
using spkt_enum_map = std::unordered_map<Key, Value, sprockit::enum_hash>;

#elif defined(SPKT_HAVE_BOOST)

#include <boost/unordered_map.hpp>
#define spkt_unordered_map boost::unordered_map

#include <boost/unordered_set.hpp>
#define spkt_unordered_set boost::unordered_set

#elif SPKT_ENABLE_ORDERED_MAP

#include <map>
#define spkt_unordered_map std::map
#define spkt_enum_map std::map
#include <set>
#define spkt_unordered_set std::set

#else

#error Configure error - must either have C++11, external boost, order enable ordered maps

#endif



#endif /* SPROCKIT_SPROCKIT_UNORDERED_H_ */

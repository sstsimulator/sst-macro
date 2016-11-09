
#ifndef SPROCKIT_SPROCKIT_UNORDERED_H_
#define SPROCKIT_SPROCKIT_UNORDERED_H_


#include <sprockit/spkt_config.h>

#if !SPKT_HAVE_CPP11
#error Sprockit requires C++11
#endif

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


#endif /* SPROCKIT_SPROCKIT_UNORDERED_H_ */

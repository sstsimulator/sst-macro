#ifndef _sstmac_software_api_fwd_h
#define _sstmac_software_api_fwd_h

namespace sstmac {
namespace sw {

class api;

api*
static_get_api(const char* name);

template <class T>
T*
get_api(){
  return dynamic_cast<T*>(static_get_api(T::api_name));
}

} }

#endif

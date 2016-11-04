#include <sprockit/keyword_registration.h>

namespace sstmac {
  static const char* main_sstmac_valid_keywords[] = {
  "id",
  "negligible_size",
  "name",
  "model",
  "mtu",
  "type",
  "latency",
  "bandwidth",
  "arbitrator",
  "debug_startup",  
  "debug",  
  "timestamp_resolution",  
  "stop_time",          
  };
  static int main_sstmac_num_valid_keywords = sizeof(main_sstmac_valid_keywords) / sizeof(const char*);
sprockit::StaticKeywordRegister _main_static_keyword_init_(main_sstmac_num_valid_keywords, main_sstmac_valid_keywords);
}

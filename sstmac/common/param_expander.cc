#include <sstmac/common/param_expander.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

RegisterKeywords(
"amm_model",
"accuracy_parameter",
"network_bandwidth",
"network_switch_bandwidth",
"network_accuracy_parameter",
"network_hop_latency",
"injection_bandwidth",
"injection_latency",
"max_memory_bandwidth",
"memory_accuracy_parameter",
"memory_latency",
"memory_bandwidth",
"network_hop_latency",
"network_bandwidth",
"injection_latency",
"injection_bandwidth",
"injection_redundant",
"max_memory_bandwidth",
"network_switch_bandwidth",
"network_negligible_size",
"nic_negligible_size",
"congestion_model",
);

ImplementFactory(sstmac::param_expander)

namespace sstmac {

}

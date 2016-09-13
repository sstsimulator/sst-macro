#ifndef sstmac_software_process_PMI_H
#define sstmac_software_process_PMI_H

#include <sstmac/common/node_address.h>
#include <sstmac/software/process/software_id.h>

#include <sstmac/hardware/topology/topology_fwd.h>
#include <sstmac/hardware/interconnect/interconnect_fwd.h>
#include <sstmac/hardware/node/node_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>

#include <sprockit/unordered.h>

namespace sstmac {
namespace sw {

class process_manager  {

 public:
  process_manager(software_id sid, operating_system* os);

  virtual ~process_manager();

  int
  get_partner(node_id addr) const;

  node_id
  my_addr() const {
    return my_addr_;
  }

  void kill_node();

  void kill_process();

 private:
  typedef spkt_unordered_map<int, node_id> proc_to_node_map;

  typedef spkt_unordered_map<int, proc_to_node_map> app_to_proc_to_node_map;

  typedef spkt_unordered_map<node_id, int> node_to_proc_map;

  typedef spkt_unordered_map<int, node_to_proc_map> app_to_node_to_proc_map;

  static app_to_proc_to_node_map node_map_;

  static app_to_node_to_proc_map proc_map_;

  node_id my_addr_;

  operating_system* my_os_;

  software_id sid_;


};

}
}



#endif // PMI_H


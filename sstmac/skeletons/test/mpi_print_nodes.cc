#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/app.h>
#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/topology/topology.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/common/runtime.h>
#include <sstmac/common/event_manager.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sstmac/skeleton.h>

using namespace sstmac;
using namespace sstmac::sw;

class node_name_logger
  : public stat_collector
{
 private:
  long max_tid_;

  spkt_unordered_map<long, std::string> node_names_;

 public:
  node_name_logger(const std::string& froot)
    :  max_tid_(0)
  {
    fileroot_ = froot;
  }

  virtual ~node_name_logger() {}

  void
  add_node(long tid, const std::string& name) {
    node_names_[tid] = name;
    max_tid_ = std::max(max_tid_, tid);
  }

  void
  simulation_finished(timestamp end) {
    for (long i=0; i <= max_tid_; ++i) {
      cout0 << sprockit::printf("Rank %5ld: ", i) << node_names_[i] << std::endl;
    }
  }

  void reduce(stat_collector *coll){}

  void clear(){}

  void dump_local_data(){}

  void dump_global_data(){}

  void global_reduce(parallel_runtime *rt){}

  stat_collector* clone() const { return new node_name_logger(fileroot_); }

  virtual std::string
  to_string() const {
    return "node name logger";
  }
};

static node_name_logger* nodelog = 0;

#define sstmac_app_name mpi_print_nodes

int USER_MAIN(int argc, char** argv)
{
  if (!nodelog) {
    nodelog = new node_name_logger("mpilog");
#if !SSTMAC_INTEGRATED_SST_CORE
    // TODO make this work with @integrated_core
    operating_system::current_os()->parent()->event_mgr()->register_stat(nodelog);
#endif
  }

  //hw::interconnect* interconn = safe_cast(hw::interconnect, sstmac_runtime::current_node()->interconn());
  node_id addr = operating_system::current_node_id();
  hw::structured_topology* top = safe_cast(hw::structured_topology, hw::topology::global());
  std::string label;
  if (top){
    label = top->node_coords(addr).to_string();
  }
  else {
    label = sprockit::printf("node(%d)", int(addr));
  }

  nodelog->add_node(operating_system::current_tid(), label);

  return 0;
}




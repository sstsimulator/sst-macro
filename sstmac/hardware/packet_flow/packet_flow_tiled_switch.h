#ifndef PACKET_FLOW_TILED_SWITCH_H
#define PACKET_FLOW_TILED_SWITCH_H

#include <sstmac/hardware/packet_flow/packet_flow_switch.h>
#include <sstmac/hardware/packet_flow/packet_flow_buffer.h>
#include <sstmac/hardware/packet_flow/packet_flow_crossbar.h>
#include <sstmac/hardware/packet_flow/packet_flow_arbitrator.h>
#include <sstmac/hardware/packet_flow/packet_flow_stats_fwd.h>

namespace sstmac {
namespace hw {

/**
 @class packet_flow_switch
 A switch in the network that arbitrates/routes packet_trains
 to the next link in the network
 */
class packet_flow_tiled_switch :
  public packet_flow_abstract_switch
{

 public:
#if !SSTMAC_INTEGRATED_SST_CORE
  packet_flow_tiled_switch(){}
#endif

  virtual void
  initialize();

  virtual void
  init_factory_params(sprockit::sim_parameters* params);

  int
  queue_length(int port) const;

  virtual void
  connect(
    int src_outport,
    int dst_inport,
    connection_type_t ty,
    connectable* mod,
    config* cfg);

  virtual void
  connect_output(int src_outport, int dst_inport, connectable* mod, config *cfg);

  virtual void
  connect_input(int src_outport, int dst_inport, connectable* mod, config *cfg);

  std::vector<switch_id>
  connected_switches() const;

  /**
   Cast message and pass to #send
   @param msg Incoming message (should cast to packet_train)
   */
  void
  handle(event* ev);

  /**
   Set the link to use when ejecting packets at their endpoint.  A packet_flow_switch
   can have any number of ejectors, corresponding to the number of nodes
   per switch.
   @param addr The compute node address of the endpoint to eject to
   @param link The link to the compute node for ejection
   */
  void
  add_ejector(node_id addr, event_handler* link);

  virtual void
  set_event_manager(event_manager* m);

  virtual std::string
  to_string() const;

  int
  initial_credits() const {
    return row_buffer_num_bytes;
  }

  virtual
  ~packet_flow_tiled_switch();

  event_handler*
  demuxer(int port) const {
    return row_input_demuxers_[port];
  }

  void
  deadlock_check();

 protected:
  virtual void
  connect_injector(int src_outport, int dst_inport, event_handler* nic);

  virtual void
  connect_ejector(int src_outport, int dst_inport, event_handler* nic);

 protected:
  std::vector<packet_flow_demuxer*> row_input_demuxers_;

  std::vector<packet_flow_crossbar*> xbar_tiles_;

  std::vector<packet_flow_muxer*> col_output_muxers_;

  int nrows_;

  int ncols_;

 private:
  int
  row_col_to_tile(int row, int col);

  void
  tile_to_row_col(int tile, int& row, int& col);

  void resize_buffers();

  void init_components();

  void
  connect_output(
    int src_outport,
    int dst_inport,
    event_handler* mod,
    config* cfg);

  void
  connect_input(
    int src_outport,
    int dst_inport,
    event_handler* mod,
    config* cfg);

};

}
}

#endif // PACKET_FLOW_TILED_SWITCH_H

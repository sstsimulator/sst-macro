/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#ifndef SSTMAC_BACKENDS_NATIVE_COMPONENTS_NIC_circuitnic_H_INCLUDED
#define SSTMAC_BACKENDS_NATIVE_COMPONENTS_NIC_circuitnic_H_INCLUDED

#include <sstmac/hardware/nic/nic.h>
#include <sstmac/hardware/circuit/circuit_message.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <queue>
#include <sprockit/unordered.h>

namespace sstmac {
namespace hw {

class circuit_nic : public nic
{

 public:
  circuit_nic();

  virtual std::string
  to_string() const {
    return sprockit::printf("circuitnic(%ld)", long(parent_->id()));
  }

  virtual ~circuit_nic() throw () {}

#if !SSTMAC_INTEGRATED_SST_CORE
  virtual void
  set_event_manager(event_manager* m) {
    nic::set_event_manager(m);
  }
#endif

  virtual void
  finalize_init();

  virtual void
  handle(message* msg);

  void
  timeout(message* msg);

  void
  send_out_resetup(message* msg);

  bool
  msg_supported(message* msg) const;

  void
  newmsg(message*m) {
    busy_ = false;
    check_jobs();
  }

 protected:
  struct job {
    node_id recver;
    message* payload;
    timestamp arrived;
  };

  virtual void
  do_send(network_message* payload);

  void
  recv_chunk(message* chunk);

  void
  init_factory_params(sprockit::sim_parameters* params);

  virtual void
  connect(
    int outport,
    int inport,
    connection_type_t ty,
    connectable* mod) {
    spkt_throw(sprockit::unimplemented_error, "circuit_nic::connect");
  }

  event_handler*
  injector() {
    spkt_throw(sprockit::unimplemented_error, "circuit_nic::get_injector");
  }

  event_handler*
  ejector() {
    spkt_throw(sprockit::unimplemented_error, "circuit_nic::get_ejector");
  }

  timestamp
  injection_latency() const;

  void
  check_jobs();

 private:
  void
  send_to_network_link(message* msg,
                       node_id recver, const timestamp &arrived);

 protected:
  timestamp propdelay_;
  double bw_;

  std::queue<job> jobs_;
  bool busy_;
  bool firstsendset_;

  timestamp setup_timeout_;
  circuit_message::ptr current_;

  typedef spkt_unordered_map<node_id, circuit_nic*> map_type;
  static map_type node_map_;

  static long got_datas_;
  static long got_path_acks_;
  static long got_setups_;
  static long got_teardowns_;
  static long numsends_;
  static bool printed_;
  static long timeouts_;

};

}
} // end of namespace sstmac.

#endif


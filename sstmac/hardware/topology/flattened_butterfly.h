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

// flattenedbutterfly.h: Interface for torus networks.
//
// Author: Jeremiah Wilke <jjwilke@sandia.gov>

#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_flattenedbutterfly_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_flattenedbutterfly_H_INCLUDED

#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/hardware/topology/butterfly.h>

namespace sstmac {
namespace hw {

/**
 * @brief The flattened_butterfly class
 * Encapsulates a flattened butterfly topology as described in
 * "High Performance Datacenter Networks" by Abts and Kim
 */
class flattened_butterfly :
  public abstract_butterfly
{

 public:
  flattened_butterfly(sprockit::sim_parameters *params);

  std::string
  to_string() const override {
    return "flattened butterfly";
  }

  virtual ~flattened_butterfly() {}

  int
  num_switches() const override {
    return nswitches_per_col_;
  }

  virtual void
  connect_objects(sprockit::sim_parameters* params,
                  internal_connectable_map& switches) override;

  void minimal_route_to_switch(switch_id src,
                          switch_id dst,
                          routable::path &path) const override;

  int minimal_distance(switch_id src, switch_id dst) const override;

 private:
  int convert_to_port(int dim, int dir) const;


};

}
} //end of namespace sstmac

#endif


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

// clos.h: Interface for CLOS networks.
//
// Based on clos.c by Jim Schutt
// Adapted by Curtis Janssen <cljanss@ca.sandia.gov>
#ifndef SSTMAC_HARDWARE_NETWORK_TOPOLOGY_CLOS_H_INCLUDED
#define SSTMAC_HARDWARE_NETWORK_TOPOLOGY_CLOS_H_INCLUDED

#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac {
namespace hw {

/**
 */
class clos : public structured_topology
{

 public:

  static clos*
  construct(sprockit::sim_parameters* params);

  int ndimensions() const {
    return nfly_ - 1;
  }

  virtual
  ~clos();

  virtual int
  radix() const {
    return kary_;
  }

  static size_t
  size(int n);

  virtual int64_t
  n() const;

  int64_t
  diameter() const {
    2*nfly_ + 1;
  }

  std::string
  name() const;

  int clos_param_n() const {
    return n_;
  }

  int clos_param_k() const {
    return k_;
  }

  int clos_param_m() const {
    return m_;
  }

  virtual void
  connect_switches(topology::switch_map &switches);

  virtual int64_t
  topology_size() const {

    return n();
  }

  node_id
  node_to_ejector_addr(
    nodeidnodeaddr,
    int &switch_port) const;

  virtual std::string
  to_string() const {
    return "clostopology";
  }

 protected:
  clos(int kary, int nfly, int nps);

 private:
  int nfly_;
  int kary_;

};

}
} //end of namespace sstmac

#endif


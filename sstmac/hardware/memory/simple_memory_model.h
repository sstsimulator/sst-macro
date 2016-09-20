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

#ifndef SIMPLE_MEMORYMODEL_H_
#define SIMPLE_MEMORYMODEL_H_

#include <sstmac/hardware/memory/memory_model.h>

namespace sstmac {
namespace hw {

class simple_memory_model : public memory_model
{
 public:
  simple_memory_model(sprockit::sim_parameters* params, node* nd);

  virtual ~simple_memory_model();

  std::string
  to_string() const override {
    return "simple memory model";
  }

  virtual void
  access(long bytes, double max_bw, callback* cb) override;

  double
  max_single_bw() const override {
    return bw_;
  }

 protected:
  class link  {
   public:
    link(double bw, timestamp lat) :
      bw_(bw), lat_(lat), last_access_(0) {
    }

    virtual ~link() { }

    virtual timestamp
    new_access(timestamp now, long size, double max_bw);

    virtual void
    access_done();

   protected:
    double bw_;
    timestamp lat_;
    timestamp last_access_;

  };

 protected:
  link* link_;

  double bw_;

  timestamp lat_;

};

}
} /* namespace sstmac */
#endif /* SIMPLE_MEMORYMODEL_H_ */


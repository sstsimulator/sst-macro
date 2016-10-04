#ifndef NODE_ID_INDEXING_H
#define NODE_ID_INDEXING_H

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

#include <sstmac/software/launch/task_mapper.h>

namespace sstmac {
namespace sw {


class node_id_task_mapper : public task_mapper
{
 public:
  node_id_task_mapper(sprockit::sim_parameters* params);

  std::string
  to_string() const override {
    return "node id task mapper";
  }

  virtual ~node_id_task_mapper() throw() {}

  void map_ranks(const ordered_node_set& nodes,
                int ppn,
                std::vector<node_id>& result,
                int nproc) override;

 protected:
  std::string listfile_;

};


}
}


#endif // NODE_ID_INDEXING_H

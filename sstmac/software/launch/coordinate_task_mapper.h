#ifndef COORDINATE_INDEXING_H
#define COORDINATE_INDEXING_H

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

#ifndef SSTMAC_SOFTWARE_SERVICES_LAUNCH_INDEXING_FROMFILEINDEXER_H_INCLUDED
#define SSTMAC_SOFTWARE_SERVICES_LAUNCH_INDEXING_FROMFILEINDEXER_H_INCLUDED

#include <sstmac/software/launch/task_mapper.h>

namespace sstmac {
namespace sw {


class coordinate_task_mapper : public task_mapper
{

 public:
  virtual ~coordinate_task_mapper() throw() {}

  void
  map_ranks(const app_id& aid,
        const ordered_node_set& nodes,
        int ppn,
        std::vector<node_id> &result,
        int nproc);

  virtual void
  init_factory_params(sprockit::sim_parameters *params);

 protected:
  std::string listfile_;

};


}
}
#endif



#endif // COORDINATE_INDEXING_H

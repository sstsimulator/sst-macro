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

#include <sprockit/serializable.h>
#include <sprockit/statics.h>
#include <sprockit/delete.h>
#include <sprockit/errors.h>
#include <cstring>
#include <iostream>

namespace sprockit {

static need_delete_statics<serializable_factory> del_statics;
serializable_factory::builder_map* serializable_factory::builders_ = 0;

uint32_t
serializable_factory::add_builder(serializable_builder* builder, const char* name)
{
  if (builders_ == 0) {
    builders_ = new builder_map;
  }

  const char* key = name;
  int len = ::strlen(key);
  uint32_t hash = 0;
  for(int i = 0; i < len; ++i)
  {
    hash += key[i];
    hash += (hash << 10);
    hash ^= (hash >> 6);
  }
  hash += (hash << 3);
  hash ^= (hash >> 11);
  hash += (hash << 15);


  builder_map& bmap = *builders_;
  serializable_builder*& current = bmap[hash];
  if (current != 0){
    std::cerr << sprockit::printf(
      "amazingly %s and %s both hash to same serializable id %u",
      current->name(), builder->name(), hash) << std::endl;
    abort();
  }
  current = builder;
  return hash;
}

void
serializable_factory::delete_statics()
{
  delete_vals(*builders_);
  delete builders_;
}

serializable*
serializable_factory::get_serializable(uint32_t cls_id)
{
  auto it = builders_->find(cls_id);
  if (it == builders_->end()) {
    spkt_abort_printf("class id %ld is not a valid serializable id",
                     cls_id);
  }
  serializable_builder* builder = it->second;
  return builder->build();
}

}


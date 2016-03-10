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

#ifndef SSTMAC_SOFTWARE_LIBRARIES_COMPUTE_LIB_COMPUTE_MEMMOVE_H_INCLUDED
#define SSTMAC_SOFTWARE_LIBRARIES_COMPUTE_LIB_COMPUTE_MEMMOVE_H_INCLUDED

#include <sstmac/software/libraries/compute/lib_compute_inst.h>
#include <sstmac/common/event_callback.h>

namespace sstmac {
namespace sw {

class lib_compute_memmove :
  public lib_compute_inst
{

 public:
  static key::category key_category;

  virtual
  ~lib_compute_memmove() {
  }

  lib_compute_memmove(software_id id);

  lib_compute_memmove(const std::string& libname);

  virtual void
  unregister_all_libs();

  static std::string
  static_name(const std::string& n) {
    return "computelibmemmove" + n;
  }

  virtual bool
  supported() const;

  void
  consume_params(sprockit::sim_parameters* params);

  void
  read(long bytes);


  void
  write(long bytes);

  void
  copy(long bytes);

 protected:
  static const long unlimited_page_size = -1;
  static const long default_page_size = unlimited_page_size;

  void init();

  void
  do_access(long bytes);

  void
  compute(sst_message* msg);

  void
  finish(sst_message* msg);


 protected:
  int access_width_bytes_;

};

}
} //end of namespace sstmac

#endif


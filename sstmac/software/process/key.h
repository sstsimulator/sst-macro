/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#ifndef SSTMAC_SOFTWARE_PROCESS_KEY_H_INCLUDED
#define SSTMAC_SOFTWARE_PROCESS_KEY_H_INCLUDED

#include <unordered_map>
#include <stdint.h>
#include <sstmac/software/process/thread_fwd.h>

#include <cstring>
#include <string>
#include <set>

namespace sstmac {
namespace sw {

/**
 * @brief The ftq_tag class
 * A descriptor for labeling the type of computation currently being done.
 * Needed by FTQ for knowing how to label (tag) elapsed time.
 */
class ftq_tag {

 private:
  friend class library;
  friend class thread;

  ftq_tag() : id_(0) {} //default initialization for thread

  int id_;

 public:
  ftq_tag(const char* name);

  int id() const {
   return id_;
  }

 public:
  static std::string name(int keyname_id) {
    return (*category_id_to_name_)[keyname_id];
  }

  static int allocate_category_id(const std::string& name);

  static int event_typeid(const std::string& event_name);

  static int num_categories() {
    return category_name_to_id_->size();
  }

  static ftq_tag null;
  static ftq_tag compute;

 private:
  static std::unordered_map<std::string, int>* category_name_to_id_;
  static std::unordered_map<int, std::string>* category_id_to_name_;

};

}
} // end of namespace sstmac

#endif

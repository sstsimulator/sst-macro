/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
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

#include <gni/gni_transport.h>
#include <algorithm>
#include <sstream>
#include <fstream>
#include <iostream>
#include <sys/types.h>
#include <sys/mman.h>


static std::map<std::string, uint16_t> gni_enums;
#define add_gni_enum(x) gni_enums[#x] = x

namespace sumi {

void
gni_transport::init()
{
  int spawned;
  int appnum;
  int ret;
  if ((ret = PMI2_Init (&spawned, &nproc_, &rank_, &appnum)) != PMI_SUCCESS) {
      gni_error("PMI Init");
  }

  active_msg_transport::init();

  add_gni_enum(GNI_DLVMODE_PERFORMANCE);
  add_gni_enum(GNI_DLVMODE_NO_ADAPT   );
  add_gni_enum(GNI_DLVMODE_NO_HASH    );
  add_gni_enum(GNI_DLVMODE_NO_RADAPT  );
  add_gni_enum(GNI_DLVMODE_IN_ORDER   );
  add_gni_enum(GNI_DLVMODE_MNON_HASH  );
  add_gni_enum(GNI_DLVMODE_NMIN_HASH  );
  add_gni_enum(GNI_DLVMODE_MIN_HASH   );
  add_gni_enum(GNI_DLVMODE_ADAPTIVE0  );
  add_gni_enum(GNI_DLVMODE_ADAPTIVE1  );
  add_gni_enum(GNI_DLVMODE_ADAPTIVE2  );
  add_gni_enum(GNI_DLVMODE_ADAPTIVE3  );
  add_gni_enum(GNI_RDMAMODE_PHYS_ADDR);
  add_gni_enum(GNI_RDMAMODE_FENCE    );
  add_gni_enum(GNI_RDMAMODE_GETWC_DIS);

  const char* rdma_mode = getenv("GNI_RDMAMODE");
  if (rdma_mode && rdma_mode[0] != '\0'){
    std::map<std::string, uint16_t>::iterator it = gni_enums.find(rdma_mode);
    if (it == gni_enums.end()){
      gni_error("Invalid GNI_RDMAMODE environment variable: %s", rdma_mode);
    }
    rdma_mode_ = it->second;
    if (rank_ == 0){
      std::cout << "Using GNI_RDMAMODE=" << rdma_mode << std::endl;
    }
  }
  else {
    if (rank_ == 0){
      std::cout << "Using GNI_RDMAMODE=GNI_RDMAMODE_PHYS_ADDR" << std::endl;
    }
  }

  const char* dlvr_mode = getenv("GNI_DLVMODE");
  if (dlvr_mode && dlvr_mode[0] != '\0'){
    std::map<std::string, uint16_t>::iterator it = gni_enums.find(dlvr_mode);
    if (it == gni_enums.end()){
      gni_error("Invalid GNI_DLVMODE environment variable: %s", dlvr_mode);
    }
    dlvr_mode_ = it->second;
    if (rank_ == 0){
      std::cout << "Using GNI_DLVMODE=" << dlvr_mode << std::endl;
    }
  }
  else {
    if (rank_ == 0){
      std::cout << "Using GNI_DLVMODE=GNI_DLVMODE_PERFORMANCE" << std::endl;
    }
  }

  tx_context_.ep_handles = new gni_ep_handle_t[nproc_];
  nic_addrs_ = new uint32_t[nproc_];

  init_cdm();

  init_cq(tx_context_, &tx_context_.cq_handle, 4096);
  init_cq(tx_context_, &smsg_rx_cq_, 4096);
  init_cq(tx_context_, &rdma_rx_cq_, 4096);

  gather_nic_data();

  init_end_points(tx_context_);

  init_smsg_metadata();

  init_smsg_buffer();

  int num_ping_bufs = 32;
  public_buffer ping_buf = allocate_public_buffer(num_ping_bufs*sizeof(int));
  ping_mem_handle_ = ping_buf.mem_handle;
  ping_buffer_ = (int*) ping_buf.ptr;
  *ping_buffer_ = i_am_alive;

  for (int i=1; i < num_ping_bufs; ++i){
    ping_response_buffers_.push_back(ping_buffer_ + i);
  }

  gather_peer_data();

  init_smsg();

  PMI_Barrier();
}

}
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
#include <cstdlib>
#include <stdlib.h>
#include <unistd.h>
#include <cstring>

namespace sumi {

template <class peer_data_t>
peer_data_t* 
gather_data(int rank, int nproc, peer_data_t* mydata)
{
  mydata->rank = rank;
  peer_data_t* tmp = new peer_data_t[nproc];
  peer_data_t* output = new peer_data_t[nproc];
  ::memcpy(&tmp[rank], mydata, sizeof(peer_data_t));
  PMI_Allgather(&tmp[rank], tmp, sizeof(peer_data_t));
  for (int i=0; i < nproc; ++i){
     int wrong_order_i = tmp[i].rank;        
     peer_data_t* dst = output + wrong_order_i;
     peer_data_t* src = tmp + i;
     ::memcpy(dst, src, sizeof(peer_data_t));
  }
  delete[] tmp;
  return output;
}

void
gni_transport::gather_nic_data()
{
  nic_data_t mydata;
  mydata.nic_addr = my_global_nic_addr_;
  nics_ = gather_data(rank_, nproc_, &mydata);
}

void
gni_transport::gather_peer_data()
{
  peer_segment_data_t mydata;
  gethostname(mydata.hostname, MAX_HOSTNAME_LENGTH);
  mydata.smsg_attr = my_smsg_attr_;
  mydata.ping_buffer = ping_buffer_;
  mydata.ping_mem_handle = ping_mem_handle_;
#if 0
  printf("init smsg on node %d with msg_type=%d maxcredit=%d maxsize=%d, buffsize=%d msqid=%d hostname=%s\n",
    rank_,
    mydata.smsg_attr.msg_type,
    mydata.smsg_attr.mbox_maxcredit,
    mydata.smsg_attr.msg_maxsize,
    mydata.smsg_attr.buff_size,
    mydata.msqid,
    mydata.hostname);
#endif
  peers_ = gather_data(rank_, nproc_, &mydata);
}

}
/**
Copyright 2009-2020 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2020, NTESS

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
#include <mpi.h>
#include <stddef.h>
#include <stdio.h>
#include <vector>
#include <sstmac/skeleton.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/errors.h>
#include <algorithm>

#define sstmac_app_name dfly_worst_case

static const std::vector<int> primes = { 17, 19, 103, 107, 59, 41, 37 };

static uint32_t crc32_for_byte(uint32_t r) {
  for(int j = 0; j < 8; ++j)
    r = (r & 1? 0: (uint32_t)0xEDB88320L) ^ r >> 1;
  return r ^ (uint32_t)0xFF000000L;
}

static uint32_t crc32(const void *data, size_t n_bytes)
{
  uint32_t crc = 11;
  static uint32_t table[0x100];
  static bool inited = false;
  if(!inited){
    for(size_t i = 0; i < 0x100; ++i){
      table[i] = crc32_for_byte(i);
    }
    inited = true;
  }
  for(size_t i = 0; i < n_bytes; ++i){
    crc = table[(uint8_t)crc ^ ((uint8_t*)data)[i]] ^ crc >> 8;
  }
  return crc;
}

std::vector<int> pseudo_random_shuffle(int seed, const std::vector<int>& vec)
{
  int idx = seed % primes.size();
  int prime = primes[idx];
  std::vector<int> clone(vec.size());
  std::vector<std::pair<uint32_t,int>> sorter;
  for (int i=0; i < vec.size(); ++i){
    uint32_t tmp = (vec[i]+prime) * prime;
    uint32_t hash = crc32(&tmp, sizeof(uint32_t));
    sorter.emplace_back(hash,vec[i]);
  }
  std::sort(sorter.begin(), sorter.end());
  for (int i=0; i < vec.size(); ++i){
    clone[i] = sorter[i].second;
  }
  return clone;
}

static void sendrecv(int partner, int msize) {
  MPI_Request send, recv;
  MPI_Isend(NULL, msize, MPI_INT, partner, 0, MPI_COMM_WORLD, &send);
  MPI_Irecv(NULL, msize, MPI_INT, partner, 0, MPI_COMM_WORLD, &recv);
  MPI_Wait(&send, MPI_STATUS_IGNORE);
  MPI_Wait(&recv, MPI_STATUS_IGNORE);
}

int USER_MAIN(int argc, char** argv)
{ 
  int num_groups = sstmac::getParam<int>("num_groups");
  int group_size = sstmac::getParam<int>("group_size");
  int seed = sstmac::getParam<int>("shuffle_seed");
  int concentration = sstmac::getParam<int>("concentration");

  MPI_Init(&argc, &argv);
  int rank, size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);

  int my_switch = rank / concentration;
  int my_node_offset = rank % concentration;
  int my_group = my_switch / group_size;
  int my_intra_grp = my_switch % group_size;

  int grp_midpoint = num_groups / 2;
  std::vector<int> group_pairing(grp_midpoint);
  for (int i=0; i < grp_midpoint; ++i){
    group_pairing[i] = i + grp_midpoint;
  }
  if (seed != -1){
    pseudo_random_shuffle(seed, group_pairing);
  }
  int dst_group = -1;
  if (my_group >= grp_midpoint){
    for (int g=0; g < group_pairing.size(); ++g){
      if (group_pairing[g] == my_group){
        dst_group = g;
        break;
      }
    }
  } else {
    dst_group = group_pairing[my_group];
  }

  if (dst_group == -1){
    MPI_Finalize();
    return 0; //no partner
  }

  std::vector<int> pairing(group_size);
  for (int i=0; i < group_size; ++i){
    pairing[i] = i;
  }

  if (seed != -1){
    pseudo_random_shuffle(seed, pairing);
  }

  int dst_intra_grp = 0;
  if (my_group < dst_group){
    dst_intra_grp = pairing[my_intra_grp];
  } else {
    for (int i=0; i < group_size; ++i){
      if (pairing[i] == my_intra_grp){
        dst_intra_grp = i;
        break;
      }
    }
  }

  int dst_rank = concentration * (dst_group * group_size + dst_intra_grp) + my_node_offset;
  if (dst_rank >= size){
    spkt_abort_printf("got bad dest rank for %d: %d,%d,%d", 
                    rank, my_node_offset, dst_intra_grp, dst_group);
  }
  static const int msize = 326144;
  static const int repeats = 4;

  double start = MPI_Wtime();
  for (int r=0; r < repeats; ++r){
    sendrecv(dst_rank, msize);
  }

  MPI_Finalize();
  double end = MPI_Wtime();
  double total = (end-start);
  if (rank == 0){
    printf("Total: %12.8fs\n", total);
  }

  return 0;
}

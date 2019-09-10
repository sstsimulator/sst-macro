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


#include <mpi.h>
#include <unistd.h>
#include <random>
#include <set>
#include <stddef.h>
#include <stdio.h>
#include <iostream>
#include <cstdlib>
#include <vector>
#include <algorithm>
#include <sys/time.h>
#include <cstring>
#include <utility>
#include <fstream>

static const int nrepeat = 40;
static const int warmup = 10;
static const int max_buffer_size = 1024*1024;
std::vector<int> default_buffer_sizes =
  { 256,512,1024,2048,3072,4096,6144,8192,12288,16384,32768,49152,65536,98304,131072,262144,524288,1048576};
std::vector<int> default_total_num_sends =
  { 64, 64, 64,  64,  64,  64,  16,  16,  16,   16,   16,   16,   16,   16,   16,    16,    16,    16};
std::vector<int> default_window_num_sends =
  { 16, 16, 16,  16,  16,  16,  4,   4,   4,    4,    4,    4,    4,    4,    4,     4,     4,     4};
std::vector<int> big_buffer_sizes = { 10485760 };
std::vector<int> big_total_num_sends = { 10 };
std::vector<int> big_window_num_sends = { 1 };
const std::vector<int> primes = { 17, 19, 103, 107, 59, 41, 37 };
std::vector<int> *buffer_sizes;
std::vector<int> *total_num_sends;
std::vector<int> *window_num_sends;

void usage(std::ostream& os){
  os << "usage: ./run <num_senders> <num_recvers> <send_seed> <recv_seed>" << std::endl;
}

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

void crash(const std::string& err){
  std::cerr << err << std::endl;
  usage(std::cerr);
  ::abort();
}

std::vector<int>
shuffled_recvers(int numSenders, const std::vector<int>& senderRanks,
                 int seed, int numPossibleRecvers, int numRecvers)
{
  std::vector<int> recvers(numPossibleRecvers);
  for (int i=0; i < numPossibleRecvers; ++i){
    recvers[i] = 2*i + 1;
  }
  bool invalid = true;
  int loops = 0;
  int sendersPerRecver = numSenders / numRecvers;
  while (invalid){
    recvers = pseudo_random_shuffle(loops, recvers);
    invalid = false;
    for (int sender=0; sender < numSenders; ++sender){
      int senderNode = senderRanks[sender] / 2;
      int recverRank = recvers[sender / sendersPerRecver];
      int recverNode = recverRank / 2;
      if (senderNode == recverNode){
        invalid = true;
        break;
      }
    }
    ++loops;
    if (loops == 10000){
      std::cerr << "Failed to find a valid matching for the seed" << std::endl;
      abort();
    }
  }
  recvers.resize(numRecvers);
  return recvers;
}

void check_argument(int arg, const char* descr){
   if (arg == 0){
    crash(std::string(descr) + ": mis-formmatted input");
   }
}

void send_traffic(int recver, MPI_Request* reqs, void* buffer, std::vector<double>& tputs);
void recv_traffic(const std::vector<int>& senders, MPI_Request* reqs, void* buffer);
void do_nothing();
void read_infile(std::string filename, std::vector< std::pair<int,int> >& flows);

int main(int argc, char** argv)
{
  MPI_Request reqs[100];
 #pragma sst new
  char* buffer = new char[max_buffer_size];
  ::memset(buffer, 0, max_buffer_size);

  MPI_Init(&argc, &argv);
  int rank, nproc;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);
  if (nproc % 2 != 0){
    crash("must have even number of ranks");
  }
  if (nproc < 4){
    crash("must have at least 4 MPI ranks");
  }

  char hostname[64];
  int rc = gethostname(hostname, 64);
  if (rc != 0){
    std::cerr << "unable to get hostname" << std::endl;
    return 1;
  }

  std::cout << (const char*) hostname << std::endl;

  char* allHostnames = rank == 0 ? new char[64*nproc] : nullptr;

  MPI_Gather(hostname, 64, MPI_BYTE, allHostnames, 64, MPI_BYTE, 0, MPI_COMM_WORLD);

  if (rank == 0){
    std::set<std::string> existingHosts;
    int currentHost = -1;
    for (int i=0; i < nproc; ++i){
      const char* name = &allHostnames[i*64];
      if (existingHosts.find(name) == existingHosts.end()){
        ++currentHost;
        existingHosts.insert(name);
      }
      printf("Rank %2d is on host %d\n", i, currentHost);
    }
  }


/**
  This randomly generates a list of senders and recvers.
  The skeleton is set up assuming one recver and one sender process per node.
  Thus a node might be a sender AND recver, sender OR recver, or neither.
  Sender ranks are even and recver ranks are odd to simplify slurm/aprun.
  The lists of senders and recvers are randomly shuffled to create the pairings.
*/



  std::string arg1(argv[1]);
  bool randomizing = true;
  std::vector< std::pair<int,int> > flows;
  if (arg1.compare("-f") == 0) {
    randomizing = false;
    std::string infile(argv[2]);
    int go_big;
    read_infile(infile,flows,go_big);
    go_big = bool(temp);
    if (go_big) {
      buffer_sizes = &big_buffer_sizes;
      total_num_sends = &big_total_num_sends;
      window_num_sends = &big_window_num_sends;
    }
    else {
      buffer_sizes = &default_buffer_sizes;
      total_num_sends = &default_total_num_sends;
      window_num_sends = &default_window_num_sends;
    }
  }

  int num_senders, num_recvers;
  int recv_seed = 0;
  int send_seed = 0;
  if (randomizing) {
    if (argc != 5){
      crash("wrong number of arguments");
    }

    num_senders = std::atoi(argv[1]);
    check_argument(num_senders, "number of senders");
    if (num_senders == 0){
      crash("num_senders mis-formatted");
    } else if (num_senders > nproc/2){
      crash("num_senders cannot be more than half the total number of ranks");
    }

    num_recvers = std::atoi(argv[2]);
    if (num_recvers == 0){
      crash("num_recvers mis-formatted");
    } else if (num_senders > nproc/2){
      crash("num_recvers cannot be more than half the total number of ranks");
    }

    send_seed = std::atoi(argv[3]);
    if (send_seed == 0){
      crash("bad seed value - must be non-zero integer or negative to indicate no shuffle");
    }

    recv_seed = std::atoi(argv[4]);
    if (recv_seed == 0){
      crash("bad seed value - must be non-zero integer or negative to indicate no shuffle");
    }

    if (num_senders % num_recvers){
      crash("num_recvers must evenly divide num_senders");
    }
  }
  else {
    std::set<int> send_set, recv_set;
    for( auto it = flows.begin(); it != flows.end(); ++it) {
      send_set.insert((*it).first);
      recv_set.insert((*it).second);
    }
    num_senders = send_set.size();
    num_recvers = recv_set.size();
  }

  int senders_per_recver = num_senders / num_recvers;

  int num_half = nproc/2;
  std::vector<int> recvers(num_half);
  for (int i=0; i < num_half; ++i){
    recvers[i] = 2*i + 1; //recvers are odd
  }

  std::vector<int> senders(num_half);
  for (int i=0; i < num_half; ++i){
    senders[i] = 2*i; //senders are even
  }

  if (send_seed > 0){ //don't shuffle if negative
    senders = pseudo_random_shuffle(send_seed, senders);
  }

  recvers.resize(num_recvers);
  if (recv_seed > 0)
    recvers = shuffled_recvers(num_senders, senders, recv_seed, num_half, num_recvers);

  std::vector<int> senders_to_me;
  int my_send_idx = -1;
  int recver_from_me = -1;
  if (randomizing) {
    int sender = 0;
    for (int s=0; s < num_senders; ++s){
      int recver = recvers[s/senders_per_recver];
      int sender = senders[s];
      if ( (sender+1) == recver && recv_seed > 0 && send_seed > 0 ){
        //don't let sender and recver be on same node
        std::cerr << "sender and recver are same node! invalid" << std::endl;
        MPI_Finalize();
        return 0;
      }
      if (rank == 0){
        printf("Rank %-3d -> Rank %-3d\n", sender, recver);
      }
      if (recver == rank){
        senders_to_me.push_back(sender);
      } else if (sender == rank){
        recver_from_me = recver;
        my_send_idx = s;
      }
    }
  }
  else {
    for (auto it = flows.begin(); it != flows.end(); ++it) {
      int src = it->first;
      int dst = it->second;
      if (src == rank) {
        my_send_idx = rank;
        recver_from_me = dst;
      }
      else if (dst == rank)
        senders_to_me.push_back(src);
      if (rank == 0){
        printf("Rank %-3d -> Rank %-3d\n", src, dst);
      }
    }
  }

  MPI_Comm roleComm;

  MPI_Barrier(MPI_COMM_WORLD);
  if (recver_from_me != -1){
    MPI_Comm_split(MPI_COMM_WORLD, 0, my_send_idx, &roleComm);
    std::vector<double> tputs((*buffer_sizes).size());
    send_traffic(recver_from_me, reqs, buffer, tputs);
    std::vector<double> allTputs((*buffer_sizes).size()*num_senders);
  #pragma sst keep
    MPI_Allgather(tputs.data(), (*buffer_sizes).size(), MPI_DOUBLE,
                  allTputs.data(), (*buffer_sizes).size(), MPI_DOUBLE, roleComm);
    int roleRank = 0;
    MPI_Comm_rank(roleComm, &roleRank);
    if (roleRank == 0){
      int num_times = (*buffer_sizes).size();
      for (int i=0; i < num_senders; ++i){
        double* tputs = &allTputs[num_times*i];
        int recver, sender;
        if (randomizing) {
          recver = recvers[i/senders_per_recver];
          sender = senders[i];
        }
        else {
          sender = flows[i].first;
          recver = flows[i].second;
        }
        for (int t=0; t < num_times; ++t){
          printf("Rank %-3d -> %-3d %8d: %10.6f GB/s  %10.6fus\n", 
                 sender, recver, (*buffer_sizes)[t], tputs[t]/1e9, (*buffer_sizes)[t]*1e6/tputs[t]);
        }
      }
    }
  } else if (!senders_to_me.empty()){
    MPI_Comm_split(MPI_COMM_WORLD, 1, 0, &roleComm);
    recv_traffic(senders_to_me, reqs, buffer);
  } else {
    MPI_Comm_split(MPI_COMM_WORLD, 2, 0, &roleComm);
    do_nothing();
  }
  MPI_Barrier(MPI_COMM_WORLD);

  MPI_Finalize();
  return 0;
}

void send_traffic(int partner, int niter, int buffer_size, int num_sends, int window_size,
           MPI_Request* reqs, void* buffer){
  for (int i=0 ; i < niter; ++i){
    int num_windows = num_sends / window_size;
    for (int w=0; w < num_windows; ++w){
      for (int send=0; send < window_size; ++send){
        int tag = i;
        MPI_Isend(buffer, buffer_size, MPI_BYTE, partner, tag,
                  MPI_COMM_WORLD, &reqs[send]);
      }
      MPI_Waitall(window_size, reqs, MPI_STATUSES_IGNORE);
    }
  }
  int tag = 42;
  int ack;
  MPI_Recv(&ack,1,MPI_INT,partner,tag,MPI_COMM_WORLD,MPI_STATUS_IGNORE);
}

void send_traffic(int recver, MPI_Request* reqs, void* buffer, std::vector<double>& tputs){
  //send a total of 100MB in 1MB window sizes, starting from 1KB base size
  int num_buffer_sizes = (*buffer_sizes).size();
  for (int i=0; i < num_buffer_sizes; ++i){
    send_traffic(recver, warmup, (*buffer_sizes)[i], (*total_num_sends)[i],
          (*window_num_sends)[i], reqs, buffer);
    struct timeval t_start;
    gettimeofday(&t_start, NULL);
    send_traffic(recver, nrepeat, (*buffer_sizes)[i], (*total_num_sends)[i],
          (*window_num_sends)[i], reqs, buffer);

    struct timeval t_stop;
    gettimeofday(&t_stop, NULL);
    double time = (t_stop.tv_sec-t_start.tv_sec) + 1e-6*(t_stop.tv_usec-t_start.tv_usec);
    auto total_bytes_sent = uint64_t((*buffer_sizes)[i]) * uint64_t((*total_num_sends)[i]) * uint64_t(nrepeat);
    double tput = total_bytes_sent / time;
    tputs[i] = tput;
    MPI_Barrier(MPI_COMM_WORLD);
  }
}

void recv_traffic(const std::vector<int>& senders, int niter, 
                 int buffer_size, int num_sends, int window_size,
                 MPI_Request* reqs, void* buffer){
  for (int i=0 ; i < niter; ++i){
    int num_windows = num_sends / window_size;
    for (int w=0; w < num_windows; ++w){
      MPI_Request* reqPtr = reqs;
      for (int send=0; send < window_size; ++send){
        int tag = i;
        for (int sender : senders){
          MPI_Irecv(buffer, buffer_size, MPI_BYTE, sender, tag,
                    MPI_COMM_WORLD, reqPtr);
          ++reqPtr;
        }
      }
      MPI_Waitall(window_size*senders.size(), reqs, MPI_STATUSES_IGNORE);
    }
  }
  for (int sender : senders){
    int ack;
    int tag = 42;
    MPI_Send(&ack,1,MPI_INT,sender,tag,MPI_COMM_WORLD);
  }
}

void recv_traffic(const std::vector<int>& senders, MPI_Request* reqs, void* buffer){
  //send a total of 100MB in 1MB window sizes, starting from 1KB base size
  int num_buffer_sizes = (*buffer_sizes).size();
  for (int i=0; i < num_buffer_sizes; ++i){
    recv_traffic(senders, warmup, (*buffer_sizes)[i], (*total_num_sends)[i],
          (*window_num_sends)[i], reqs, buffer);
    recv_traffic(senders, nrepeat, (*buffer_sizes)[i], (*total_num_sends)[i],
          (*window_num_sends)[i], reqs, buffer);
    MPI_Barrier(MPI_COMM_WORLD);
  }
}

void do_nothing()
{
  int num_buffer_sizes = (*buffer_sizes).size();
  for (int i=0; i < num_buffer_sizes; ++i){
    MPI_Barrier(MPI_COMM_WORLD);
  }
}

void read_infile(std::string filename, std::vector< std::pair<int,int> >& flows, int go_big)
{
  std::ifstream in;
  in.open(filename);
  char c;
  in >> go_big;
  if (go_big != 0 && go_big != 1) {
    std::cerr << "first line of input file should be boolean indicating whether to use big buffer send";
    abort();
  }
  while (in.get(c)) {
    in.unget();
    int src, dst;
    in >> src;
    in >> dst;
    in >> std::ws;
    flows.push_back( std::pair<int,int>(src,dst));
  }
  in.close();
}


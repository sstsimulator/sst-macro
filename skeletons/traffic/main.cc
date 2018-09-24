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

#define sstmac_app_name traffic_pattern



static const int nrepeat = 40;
static const int warmup = 10;
static const int max_buffer_size = 1024*1024;
const std::vector<int> buffer_sizes =
  { 256,512,1024,2048,3072,4096,6144,8192,12288,16384,32768,49152,65536,98304,131072,262144,524288,1048576};
const std::vector<int> total_num_sends =
  { 64, 64, 64,  64,  64,  64,  16,  16,  16,   16,   16,   16,   16,   16,   16,    16,    16,    16};
const std::vector<int> window_num_sends =
  { 16, 16, 16,  16,  16,  16,  4,   4,   4,    4,    4,    4,    4,    4,    4,     4,     4,     4};

void usage(std::ostream& os){
  os << "usage: ./run <num_senders> <num_recvers> <send_seed> <recv_seed>" << std::endl;
}

void crash(const std::string& err){
  std::cerr << err << std::endl;
  usage(std::cerr);
  ::abort();
}

std::vector<int>
shuffled_recvers(int numSenders, const std::vector<int>& senderRanks, int seed, int numPossibleRecvers, int numRecvers)
{
  std::mt19937 mt(seed);

  int sendersPerRecver = numSenders / numRecvers;

  std::vector<int> vec(numRecvers);
  std::set<int> alreadyUsed;

  int idx = 0;
  while(idx < numRecvers){
    std::set<int> currentSenderNodes;
    int start = idx*sendersPerRecver;
    int stop = start + sendersPerRecver;
    for (int i=start; i < stop; ++i){
      int node = senderRanks[i]/2;
      currentSenderNodes.insert(node);
    }
    bool matchedSender = true;
    int recverNode = 0;
    int numTries = 0;
    while (matchedSender || alreadyUsed.find(recverNode) != alreadyUsed.end()){
      recverNode = mt() % numPossibleRecvers;
      matchedSender = currentSenderNodes.find(recverNode) != currentSenderNodes.end();
      if (numTries == 1000){
        std::cerr << "Too many tries to generate valid config - quitting" << std::endl;
        abort();
      }
      ++numTries;
    }

    //recver ranks are 2*n+1
    vec[idx] = 2*recverNode + 1;
    alreadyUsed.insert(recverNode);
    ++idx;
  }
  return vec;
}

void check_argument(int arg, const char* descr){
   if (arg == 0){
    crash(std::string(descr) + ": mis-formmatted input");
   }
}

void blast(int recver, MPI_Request* reqs, void* buffer, std::vector<double>& tputs);
void get_blasted(const std::vector<int>& senders, MPI_Request* reqs, void* buffer);
void be_useless();

int main(int argc, char** argv)
{
  if (argc != 5){
    crash("wrong number of arguments");
  }

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

  int num_senders = std::atoi(argv[1]);
  check_argument(num_senders, "number of senders");
  if (num_senders == 0){
    crash("num_senders mis-formatted"); 
  } else if (num_senders > nproc/2){
    crash("num_senders cannot be more than half the total number of ranks");
  } 

  int num_recvers = std::atoi(argv[2]);
  if (num_recvers == 0){
    crash("num_recvers mis-formatted"); 
  } else if (num_senders > nproc/2){
    crash("num_recvers cannot be more than half the total number of ranks");
  } 

  int send_seed = std::atoi(argv[3]);
  if (send_seed == 0){
    crash("bad seed value - must be non-zero integer or negative to indicate no shuffle");
  }

  int recv_seed = std::atoi(argv[4]);
  if (recv_seed == 0){
    crash("bad seed value - must be non-zero integer or negative to indicate no shuffle");
  }

  if (num_senders % num_recvers){
    crash("num_recvers must evenly divide num_senders");
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
    std::mt19937 mt(send_seed);
    std::shuffle(senders.begin(), senders.end(), mt);
  }

  if (recv_seed > 0){
    recvers = shuffled_recvers(num_senders, senders, recv_seed, num_half, num_recvers);
  } else {
    recvers.resize(num_recvers);
    for (int i=0; i < num_recvers; ++i){
      recvers[i] = 2*i + 1;
    }
  }

  std::vector<int> senders_to_me;
  int recver_from_me = -1;
  int sender = 0;
  int my_send_idx = -1;
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



  MPI_Comm roleComm;

  MPI_Barrier(MPI_COMM_WORLD);
  if (recver_from_me != -1){
    MPI_Comm_split(MPI_COMM_WORLD, 0, my_send_idx, &roleComm);
    std::vector<double> tputs(buffer_sizes.size());
    blast(recver_from_me, reqs, buffer, tputs);
    std::vector<double> allTputs(buffer_sizes.size()*num_senders);
  #pragma sst keep
    MPI_Allgather(tputs.data(), buffer_sizes.size(), MPI_DOUBLE,
                  allTputs.data(), buffer_sizes.size(), MPI_DOUBLE, roleComm);
    int roleRank = 0;
    MPI_Comm_rank(roleComm, &roleRank);
    if (roleRank == 0){
      int num_times = buffer_sizes.size();
      for (int i=0; i < num_senders; ++i){
        double* tputs = &allTputs[num_times*i];
        int recver = recvers[i/senders_per_recver];
        int sender = senders[i];
        for (int t=0; t < num_times; ++t){
          printf("Rank %-3d -> %-3d %8d: %10.6f GB/s  %10.6fus\n", 
                 sender, recver, buffer_sizes[t], tputs[t]/1e9, buffer_sizes[t]*1e6/tputs[t]);
        }
      }
    }
  } else if (!senders_to_me.empty()){
    MPI_Comm_split(MPI_COMM_WORLD, 1, 0, &roleComm);
    get_blasted(senders_to_me, reqs, buffer);
  } else {
    MPI_Comm_split(MPI_COMM_WORLD, 2, 0, &roleComm);
    be_useless();
  }
  MPI_Barrier(MPI_COMM_WORLD);

  MPI_Finalize();
  return 0;
}

void blast(int partner, int niter, int buffer_size, int num_sends, int window_size,
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

void blast(int recver, MPI_Request* reqs, void* buffer, std::vector<double>& tputs){
  //send a total of 100MB in 1MB window sizes, starting from 1KB base size
  int num_buffer_sizes = buffer_sizes.size();
  for (int i=0; i < num_buffer_sizes; ++i){
    blast(recver, warmup, buffer_sizes[i], total_num_sends[i], 
          window_num_sends[i], reqs, buffer);
    struct timeval t_start;
    gettimeofday(&t_start, NULL);
    blast(recver, nrepeat, buffer_sizes[i], total_num_sends[i], 
          window_num_sends[i], reqs, buffer);

    struct timeval t_stop;
    gettimeofday(&t_stop, NULL);
    double time = (t_stop.tv_sec-t_start.tv_sec) + 1e-6*(t_stop.tv_usec-t_start.tv_usec);
    auto total_bytes_sent = uint64_t(buffer_sizes[i]) * uint64_t(total_num_sends[i]) * uint64_t(nrepeat);
    double tput = total_bytes_sent / time;
    tputs[i] = tput;
    MPI_Barrier(MPI_COMM_WORLD);
  }
}

void get_blasted(const std::vector<int>& senders, int niter, 
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

void get_blasted(const std::vector<int>& senders, MPI_Request* reqs, void* buffer){
  //send a total of 100MB in 1MB window sizes, starting from 1KB base size
  int num_buffer_sizes = buffer_sizes.size();
  for (int i=0; i < num_buffer_sizes; ++i){
    get_blasted(senders, warmup, buffer_sizes[i], total_num_sends[i], 
          window_num_sends[i], reqs, buffer);
    get_blasted(senders, nrepeat, buffer_sizes[i], total_num_sends[i], 
          window_num_sends[i], reqs, buffer);
    MPI_Barrier(MPI_COMM_WORLD);
  }
}

void be_useless()
{
  int num_buffer_sizes = buffer_sizes.size();
  for (int i=0; i < num_buffer_sizes; ++i){
    MPI_Barrier(MPI_COMM_WORLD);
  }
}

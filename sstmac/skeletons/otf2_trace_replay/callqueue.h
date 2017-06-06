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

#ifndef sstmac_skeletons_otf2_CALLQUEUE_H_
#define sstmac_skeletons_otf2_CALLQUEUE_H_

#include <queue>
#include <deque>
#include <iostream>
#include <sumi-mpi/mpi_api.h>

#include <sstmac/skeletons/otf2_trace_replay/mpicall.h>
#include <sstmac/skeletons/otf2_trace_replay/callid.h>

// forward declare
class MpiCall;
class OTF2TraceReplayApp;

// http://stackoverflow.com/questions/1259099/stdqueue-iteration
template<typename T, typename Container=std::deque<T> >
class iterable_queue : public std::queue<T,Container> {
 public:
  typedef typename Container::iterator iterator;
  typedef typename Container::const_iterator const_iterator;

  iterator begin() {
      return this->c.begin();
  }
  iterator end() {
      return this->c.end();
  }
  std::reverse_iterator<iterator> rbegin() {
      return this->c.rbegin();
  }
  std::reverse_iterator<iterator> rend() {
      return this->c.rend();
  }
  const_iterator begin() const {
      return this->c.begin();
  }
  const_iterator end() const {
      return this->c.end();
  }
  const_iterator rbegin() const {
      return this->c.rbegin();
  }
  const_iterator rend() const {
      return this->c.rend();
  }
};

class CallQueue {
 public:
  CallQueue();
  CallQueue(OTF2TraceReplayApp*);

  MpiCall* find_latest(MPI_CALL_ID id) {
    for (auto iter = call_queue.rbegin(); iter != call_queue.rend(); iter++)
       if (id == (*iter)->id) return *iter;

    std::cout << "Failed to find " << id << ", printing the queue in reverse" << std::endl;
    for (auto iter = call_queue.rbegin(); iter != call_queue.rend(); iter++)
      std::cout << (*iter)->id << std::endl;
    return nullptr;
  }

  MpiCall* find_earliest(MPI_CALL_ID id) {
    for (auto iter = call_queue.begin(); iter != call_queue.end(); iter++)
      if (id == (*iter)->id) return (*iter);
    return nullptr;
  }

  // Push a new call onto the back of the CallQueue
  void AddCall(MpiCall*);

  // Notify the CallQueue handlers that a given call was finished
  // Returns the number of calls triggered
  int CallReady(MpiCall*);

  // Returns the number of calls waiting in the queue
  int GetDepth();

  // Get the entry at the front of the queue
  MpiCall* Peek();

  // Get the entry from the back of the queue
  MpiCall* PeekBack();

  // Begin tracking a pending MPI call with a request
  void AddRequest(MPI_Request req, MpiCall*);

  // Finds an MPI call based on a request
  MpiCall* FindRequest(MPI_Request);

  // Stop tracking a pending MPI call
  void RemoveRequest(MPI_Request);

 private:
  iterable_queue<MpiCall*> call_queue;
  std::unordered_map<MPI_Request, MpiCall*> request_map;
  OTF2TraceReplayApp* app;

  friend class OTF2TraceReplayApp;
};

#endif /* CALLQUEUE_H_ */
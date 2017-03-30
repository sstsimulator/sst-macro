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
  void AddRequest(MpiCall*);

  // Finds an MPI call based on a request
  MpiCall* FindRequest(MPI_Request); // TODO: is request collision possible with the current implementation?

  // Stop tracking a pending MPI call
  void RemoveRequest(MPI_Request);

 private:
  iterable_queue<MpiCall*> call_queue;
  std::unordered_map<MPI_Request, MpiCall*> request_map;
  OTF2TraceReplayApp* app;

  friend class OTF2TraceReplayApp;
};

#endif /* CALLQUEUE_H_ */

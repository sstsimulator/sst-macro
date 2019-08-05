#ifndef sstmac_software_process_cq_h
#define sstmac_software_process_cq_h

#include <queue>
#include <map>
#include <list>
#include <sprockit/errors.h>
#include <sstmac/software/process/thread_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>

namespace sstmac {
namespace sw {

struct ProgressQueue {
  OperatingSystem* os;

  ProgressQueue(OperatingSystem* os) : os(os)
  {
  }

  void block(std::list<Thread*>& q, double timeout);
  void unblock(std::list<Thread*>& q);

};

template <class Item>
struct SingleProgressQueue : public ProgressQueue {
  std::queue<Item*> items;
  std::list<Thread*> pending_threads;

  SingleProgressQueue(OperatingSystem* os) :
    ProgressQueue(os)
  {
  }

  Item* front(bool blocking = true, double timeout = -1){
    if (items.empty()){
      if (blocking){
        block(pending_threads, timeout);
      } else {
        return nullptr;
      }
    }
    if (items.empty()){
#if SSTMAC_SANITY_CHECK
      if (timeout <= 0){
        spkt_abort_printf("not given a timeout, but unblocked to find no items");
      }
#endif
      return nullptr;
    } else {
      auto it = items.front();
      return it;
    }
  }

  void pop(){
    items.pop();
  }

  void incoming(Item* i){
    items.push(i);
    if (!pending_threads.empty()){
      unblock(pending_threads);
    }
  }


};

template <class Item>
struct MultiProgressQueue : public ProgressQueue {
  std::list<Thread*> any_threads;
  std::map<int,std::queue<Item*>> queues;
  std::map<int,std::list<Thread*>> pending_threads;
  OperatingSystem* os;

  MultiProgressQueue(OperatingSystem* os) : ProgressQueue(os)
  {
  }

  Item* find_any(bool blocking = true, double timeout = -1){
    for (auto& pair : queues){
      if (!pair.second.empty()){
        auto it = pair.second.front();
        pair.second.pop();
        return it;
      }
    }
    if (blocking){
      block(any_threads, timeout);
    } else {
      return nullptr;
    }

    for (auto& pair : queues){
      if (!pair.second.empty()){
        auto it = pair.second.front();
        pair.second.pop();
        return it;
      }
    }

#if SSTMAC_SANITY_CHECK
    if (timeout <= 0){
      spkt_abort_printf("unblocked on CQ without timeout, but there are no messages");
    }
#endif
    return nullptr;
  }

  Item* find(int cq, bool blocking = true, double timeout = -1){
    if (queues[cq].empty()){
      if (blocking){
        block(pending_threads[cq], timeout);
      } else {
        return nullptr;
      }
    }


    if (queues[cq].empty()){
#if SSTMAC_SANITY_CHECK
      if (timeout <= 0){
        spkt_abort_printf("unblocked on CQ with no timeout, but there are no items");
      }
#endif
      return nullptr;
    } else {
      auto it = queues[cq].front();
      queues[cq].pop();
      return it;
    }
  }

  void incoming(int cq, Item* it){
    queues[cq].push(it);
    if (!pending_threads[cq].empty()){
      unblock(pending_threads[cq]);
    } else if (!any_threads.empty()){
      unblock(any_threads);
    } else {
      //pass, nothing to do
    }
  }

};

}
}


#endif

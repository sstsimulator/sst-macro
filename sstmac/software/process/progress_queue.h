#ifndef sstmac_software_process_cq_h
#define sstmac_software_process_cq_h

#include <queue>
#include <map>
#include <sstmac/software/process/thread_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>

namespace sstmac {
namespace sw {

struct progress_queue {
  operating_system* os;

  progress_queue(operating_system* os) : os(os)
  {
  }

  void block(std::queue<thread*>& q, double timeout);
  void unblock(std::queue<thread*>& q);

};

template <class item>
struct single_progress_queue : public progress_queue {
  std::queue<item*> items;
  std::queue<thread*> pending_threads;

  single_progress_queue(operating_system* os) :
    progress_queue(os)
  {
  }

  item* pop(bool blocking = true, double timeout = -1){
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
      items.pop();
      return it;
    }
  }

  void incoming(item* i){
    items.push(i);
    if (!pending_threads.empty()){
      unblock(pending_threads);
    }
  }

};

template <class item>
struct multi_progress_queue : public progress_queue {
  std::queue<thread*> any_threads;
  std::map<int,std::queue<item*>> queues;
  std::map<int,std::queue<thread*>> pending_threads;
  operating_system* os;

  multi_progress_queue(operating_system* os) : progress_queue(os)
  {
  }

  item* find_any(bool blocking = true, double timeout = -1){
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

  item* find(int cq, bool blocking = true, double timeout = -1){
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

  void incoming(int cq, item* it){
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

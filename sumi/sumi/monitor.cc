#include <sumi/monitor.h>
#include <sumi/transport.h>

RegisterDebugSlot(sumi_ping,
  "print all debug output associated with ping/timeout operations in the sumi framework");
RegisterDebugSlot(sumi_failure,
  "print all the debug output associated with all node failures detected by the sumi framework");

ImplementFactory(sumi::activity_monitor);

namespace sumi {

void
function_set::timeout_all_listeners(int dst)
{
  //we have failed! loop through the functions and time them out
  std::list<timeout_function*> tmp = listeners_;
  std::list<timeout_function*>::iterator it, end = tmp.end();
  int idx = 0;
  for (it=tmp.begin(); it != end; ++it, ++idx){
    timeout_function* func = *it;
    debug_printf(sprockit::dbg::sumi_ping,
     "\ttiming out ping %p to %d",
      func, dst);
    func->time_out(dst);
  }
}

int
function_set::erase(timeout_function* func)
{
  debug_printf(sprockit::dbg::sumi_ping,
   "\terasing ping %p", func);

  std::list<timeout_function*>::iterator tmp,
    it = listeners_.begin(),
    end = listeners_.end();
  bool found = false;
  while(it != end){
    tmp = it++;
    timeout_function* test = *tmp;
    if (test == func){
      listeners_.erase(tmp);
      found = true;
      break;
    }
  }

  if (!found){
    spkt_throw_printf(sprockit::illformed_error,
        "pinger::cancel: unknown pinger %p",
        func);
  }

  return listeners_.size();
}

}



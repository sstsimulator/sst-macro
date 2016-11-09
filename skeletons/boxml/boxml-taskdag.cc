#include "boxml.h"
#include <sched.h>
#include <test.h>

namespace lblxml {

//#ifdef BOXML_HAVE_TEST

Task*
boxml::get_task(int id) {
  Task* tsk;
  if (g_task_map[id] == NULL) {
    ++ncomp_;
    //tsk = new_task(my_sleep,int( timestamp(comp->time()).msec() ));
    tsk = new_task(my_sleep,0);
    g_task_map[id] = tsk;
  }
  else
    tsk = g_task_map[id];
  return tsk;
}

/*
   * Only computation tasks are included in the task DAG.  Things get a little
   * tricky as we recursively examine the event graph; if the event
   * corresponding to id is not a computation, then we must provide the
   * proper computation event using comp_id.
   * comp_id defaults to -1 (not used)
   **/
void
boxml::add_comp_tasks(int id, int comp_id) {

  // if we've already processed this task, move on
  if (task_processed_.count(id))
    return;

  task_processed_.insert(id);
  event* ev = g_events[id];

  // get parent comp task, comp_id is the parent task if > -1
  Task* tsk;
  if (comp_id > -1)
    tsk = get_task(comp_id);
  else
    tsk = get_task(id);

  // handle simple events
  if (ev->event_type() != event::collective) {
    simple_event* sev = static_cast<simple_event*>(ev);
    int_container_t& children = sev->get_listeners();
    for(int_container_iter child_it = children.begin();
        child_it != children.end(); ++child_it){
      event* child_ev = g_events[*child_it];
      if (child_ev->event_type() == event::computation) {
        Task* child_tsk = get_task(*child_it);
        child_tsk->dependsOn(tsk);
        add_comp_tasks(*child_it);
      }
      else {
        if (comp_id > -1)
          add_comp_tasks(*child_it,comp_id);
        else
          add_comp_tasks(*child_it,id);
      }
    }
  }

  // handle reductions
  else {
    reduce_t* red_ev;
    red_ev = static_cast<reduce_t*>(ev);
    const int* boxes = red_ev->box_array();
    for (int i=0; i < red_ev->nboxes(); ++i) {
      for (reduce_t::listener_iterator it = red_ev->listener_begin(boxes[i]);
           it != red_ev->listener_end(boxes[i]); ++it) {
        event* child_ev = g_events[*it];
        if (child_ev->event_type() == event::computation) {
          Task* child_tsk = get_task(*it);
          child_tsk->dependsOn(tsk);
          add_comp_tasks(*it);
        }
        else {
          if (comp_id > -1)
            add_comp_tasks(*it,comp_id);
          else
            add_comp_tasks(*it,id);
        }
      }
    }
  }
}

Task*
boxml::build_task_graph() {

  Task* root_task = new_task(my_sleep,0);
  ++ncomp_;

  //int ncomp_verify = 0;
  //std::set<int> comps_xml;

  //make "root computes" dependent on dummy root_task
  for (int i=0; i < g_events.size(); ++i) {
    event* ev = g_events[i];
    if (ev == NULL)
      continue;

    if (ev->event_type() == event::computation){
      //++ncomp_verify;
      //comps_xml.insert(i);
      if (ev->n_dep() == 0) {

        Task* tsk = get_task(i);
        tsk->dependsOn(root_task);
        // this recursively adds comp listeners
        add_comp_tasks(i);
      }
    }
  }
  std::cout << "created " << ncomp_ << " computation tasks\n";

  // USEFUL FOR DEBUGGING
  //std::vector<int> v( std::max(comps_xml.size(),task_processed_.size()) );
  //std::vector<int>::iterator it;
  //it=std::set_difference (comps_xml.begin(), comps_xml.end(),
  //                        task_processed_.begin(), task_processed_.end(),
  //                        v.begin());
  //v.resize(it-v.begin());
  //std::cerr << v.size() << " Missing comps:\n";
  //for (int i=0; i < v.size(); ++i) {
  //  std::cerr << v[i] << " ";
  //}
  //std::cerr << "\n";

  return root_task;
}

//#endif

} // end namespace lblxml

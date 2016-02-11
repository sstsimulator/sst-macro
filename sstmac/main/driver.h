#ifndef sstmac_main_DRIVER_H
#define sstmac_main_DRIVER_H

#include <sprockit/sim_parameters.h>
#include <sstmac/main/sstmac.h>
#include <list>
#include <iostream>

namespace sstmac {

template <class T>
class Labeler {
 public:
  static int assign_label(char* buffer, const T& t){
    T* tPtr = reinterpret_cast<T*>(buffer);
    *tPtr = t;
    return sizeof(T);
  }

  static int extract_label(char* buffer, T& t){
    T* tPtr = reinterpret_cast<T*>(buffer);
    t = *tPtr;
    return sizeof(T);
  }
};

template <>
class Labeler<const char*> {
 public:
  static int assign_label(char* buffer, const char* msg){
   const char** tPtr = reinterpret_cast<const char**>(buffer);
   *tPtr = msg;
   return sizeof(const char*);
  }

  static int extract_label(char* buffer, const char*& msg){
   const char** tPtr = reinterpret_cast<const char**>(buffer);
   msg = *tPtr;
   return sizeof(const char*);
  }
};

template <class T>
int
append_label(char* buffer, const T& t){
  return Labeler<T>::assign_label(buffer, t);
}

template <class T>
int
extract_label(char* buffer, T& t){
  return Labeler<T>::extract_label(buffer, t);
}


typedef int pipe_t[2];

class Simulation
{
  friend class SimulationQueue;

 public:
  double wallTime() const {
    return wall_time_;
  }

  double simulatedTime() const {
    return sim_time_;
  }

  void wait();

  template <class A>
  void
  setLabel(const A& a){
    label_offset_ = 0;
    add(a);
  }

  template <class A, class B>
  void
  setLabel(const A& a, const B& b){
    label_offset_ = 0;
    add(a); add(b);
  }

  template <class A, class B, class C>
  void
  setLabel(const A& a, const B&b, const C& c){
    label_offset_ = 0;
    add(a); add(b); add(c);
  }

  template <class A, class B, class C, class D>
  void
  setLabel(const A& a, const B&b, const C& c, const D& d){
    label_offset_ = 0;
    add(a); add(b); add(c); add(d);
  }

  template <class A>
  void
  getLabel(A& a){
    label_offset_ = 0;
    extract(a);
  }

  template <class A, class B>
  void
  getLabel(A& a, B& b){
    label_offset_ = 0;
    extract(a); extract(b);
  }

  template <class A, class B, class C>
  void
  getLabel(A& a, B&b, C& c){
    label_offset_ = 0;
    extract(a); extract(b); extract(c);
  }

  template <class A, class B, class C, class D>
  void
  getLabel(A& a, B&b, C& c, D& d){
    label_offset_ = 0;
    extract(a); extract(b); extract(c); extract(d);
  }

  pid_t
  pid() const {
    return pid_;
  }

 private:
  template <class T>
  void
  add(const T& t){
    label_offset_ += append_label(label_ + label_offset_, t);
  }

  template <class T>
  void
  extract(T& t){
    label_offset_ += extract_label(label_ + label_offset_, t);
  }

  void
  setPid(pid_t pid){
    pid_ = pid;
  }

  void
  setWallTime(double wallTime){
    wall_time_ = wallTime;
  }

  void
  setSimulatedTime(double simTime){
    sim_time_ = simTime;
  }

  void
  setPipe(pipe_t p){
    pfd_[0] = p[0];
    pfd_[1] = p[1];
  }

  int
  readPipe() const {
    return pfd_[0];
  }

  int
  writePipe() const {
    return pfd_[1];
  }

  void
  setParameters(sprockit::sim_parameters* params);

  sprockit::sim_parameters params_;
  double sim_time_;
  double wall_time_;
  char label_[256];
  int label_offset_;
  pid_t pid_;
  pipe_t pfd_;

};


class SimulationQueue
{
 public:
  Simulation*
  fork(sprockit::sim_parameters& params){
    return fork(&params);
  }

  void init(int argc, char** argv);

  void finalize();

  Simulation*
  fork(sprockit::sim_parameters* params);

  Simulation*
  waitForCompleted();

  void
  clear(Simulation* sim);

  void
  run(sprockit::sim_parameters* params, sim_stats& stats);

 private:
  std::list<Simulation*> pending_;
  parallel_runtime* rt_;
  sprockit::sim_parameters template_params_;
  opts template_opts_;

};

}


#endif // DRIVER_H


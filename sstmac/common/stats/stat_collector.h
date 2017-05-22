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

#ifndef SSTMAC_COMMON_STATS_STAT_LOGGER_H_INCLUDED
#define SSTMAC_COMMON_STATS_STAT_LOGGER_H_INCLUDED

#include <iostream>
#include <fstream>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_scheduler_fwd.h>
#include <sstmac/backends/common/parallel_runtime_fwd.h>
#include <sprockit/factories/factory.h>
#include <sprockit/printable.h>

namespace sstmac {

struct stat_descr_t {
  bool dump_all;
  bool reduce_all;
  bool dump_main;
  const char* suffix;

  stat_descr_t() :
    dump_all(false), reduce_all(true),
    dump_main(true), suffix(nullptr)
  {
  }
};

/**
 * A type of logger that collects some kind of statistic
 * and outputs to a file during or at the end of a simulation.
 * Usually, a static instance of this class should be used,
 * because no merging of stat objects takes place, which means
 * you'll get one file for each stat object.
 */
class stat_collector : public sprockit::printable
{
  DeclareFactory(stat_collector)
 public:
  virtual
  ~stat_collector();

  /** This is to notify that the statistics collector is done.
   *  Do any necessary data post-processing, but do NOT dump to file */
  virtual void
  simulation_finished(timestamp end) = 0;

  /** After post-processing, this notifies the collector to dump data to a file
   *  @param name The root of the filename to dump to */
  virtual void
  dump_local_data() = 0;

  /** After post-processing, this notifies the collector to dump data to a file
   *  @param name The root of the filename to dump to */
  virtual void
  dump_global_data() = 0;

  virtual void
  global_reduce(parallel_runtime* rt) = 0;

  virtual void
  reduce(stat_collector* coll) = 0;

  virtual void
  clear() = 0;

  bool
  registered() const {
    return registered_;
  }

  virtual void
  set_id(int id){
    id_ = id;
  }

  int
  id() const {
    return id_;
  }

  void
  set_registered(bool reg) {
    registered_ = reg;
  }

  std::string
  fileroot() const {
    return fileroot_;
  }

  stat_collector*
  clone() const {
    return do_clone(params_);
  }

  /**
   * @brief optional_build Build a stats object with all paramters
   *                configured in a particular namespace with the exact type
   *                of stats object determined by the parameter ''type''
   * @param params  The parameters for building the stats object
   * @param ns      The parameter namespace
   * @param deflt   The default parameter value to use if ''type'' is not specified
   * @param suffix  An optional suffix to apply if the multiple stats objects
   *                are configured in the same namespace
   * @return        The corresponding stat_collector object otherwise
   *                a nullptr if no parameters exist in the given namespace
   */
  static stat_collector*
  optional_build(sprockit::sim_parameters* params,
                const std::string& ns,
                const std::string& deflt,
                const char* suffix);
  /**
   * @brief optional_build Build a stats object with all paramters
   *                configured in a particular namespace with the exact type
   *                of stats object determined by the parameter ''type''
   * @param params  The parameters for building the stats object
   * @param ns      The parameter namespace
   * @param deflt   The default parameter value to use if ''type'' is not specified
   * @param suffix  An optional suffix to apply if the multiple stats objects
   *                are configured in the same namespace
   * @return        The corresponding stat_collector object, otherwise abort
   *                if no parameters exist in the given namespace
   */
  static stat_collector*
  required_build(sprockit::sim_parameters* params,
                const std::string& ns,
                const std::string& deflt,
                const char* suffix);

  static void
  stats_error(sprockit::sim_parameters* params,
             const std::string& ns,
             const std::string& deflt);

  static void
  register_optional_stat(event_scheduler* parent, stat_collector* coll, stat_descr_t* descr);

 protected:
  stat_collector(sprockit::sim_parameters* params);

  /**
   * Check to see if the file is open.  If not, try and open it and set
   * the current output stream to it, and throw an sprockit::spkt_error if something goes
   * wrong.
   * @return
   */
  static bool
  check_open(std::fstream& myfile, const std::string& fname,
             std::ios::openmode flags = std::ios::out);

  virtual stat_collector*
  do_clone(sprockit::sim_parameters* params) const = 0;

 protected:
  int id_;
  std::string fileroot_;

 private:
  sprockit::sim_parameters* params_;
  bool registered_;

};

class stat_value_base : public stat_collector
{
 public:
  void
  set_label(std::string label) {
    label_ = label;
  }

 protected:
  stat_value_base(sprockit::sim_parameters* params);

  int id_;

  std::string label_;

};

template <class T>
class stat_value : public stat_value_base
{
 public:
  void collect(const T& val){
    value_ += val;
  }

 protected:
  stat_value(sprockit::sim_parameters* params) :
    stat_value_base(params)
  {
  }

  T value_;
};


/**
 * See documentation for stat_collector::required_build
 */
template <class T>
T*
required_stats(event_scheduler* parent,
              sprockit::sim_parameters* params,
              const std::string& ns,
              const std::string& deflt,
              stat_descr_t* descr = nullptr){
  stat_collector* coll = stat_collector::required_build(params,ns,deflt,descr ? descr->suffix : nullptr);
  T* t = dynamic_cast<T*>(coll);
  if (!t){
    stat_collector::stats_error(params, ns, deflt);
  }
  stat_collector::register_optional_stat(parent, t, descr);
  return t;
}

template <class T>
T*
required_stats(event_scheduler* parent,
              sprockit::sim_parameters* params,
              const std::string& ns,
              const std::string& deflt,
              const char* suffix){
  stat_descr_t descr;
  descr.suffix = suffix;
  return required_stats<T>(parent, params, ns, deflt, suffix);
}

/**
 * See documentation for stat_collector::optional_build
 */
template <class T>
T*
optional_stats(event_scheduler* parent,
              sprockit::sim_parameters* params,
              const std::string& ns,
              const std::string& deflt,
              stat_descr_t* descr = nullptr){

  stat_collector* coll = stat_collector::optional_build(params, ns, deflt, descr ? descr->suffix : nullptr);
  if (coll){
    T* t = dynamic_cast<T*>(coll);
    if (!t){
      stat_collector::stats_error(params, ns, deflt);
    }
    stat_collector::register_optional_stat(parent, t, descr);
    return t;
  }
  else return nullptr;
}

template <class T>
T*
optional_stats(event_scheduler* parent,
              sprockit::sim_parameters* params,
              const std::string& ns,
              const std::string& deflt,
              const char* suffix)
{
  stat_descr_t descr;
  descr.suffix = suffix;
  return optional_stats<T>(parent, params, ns, deflt, &descr);
}


} // end of namespace sstmac
#endif
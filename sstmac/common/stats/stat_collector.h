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

#ifndef SSTMAC_COMMON_STATS_STAT_LOGGER_H_INCLUDED
#define SSTMAC_COMMON_STATS_STAT_LOGGER_H_INCLUDED

#include <iostream>
#include <fstream>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_scheduler_fwd.h>
#include <sstmac/common/stats/stat_collector_fwd.h>
#include <sstmac/backends/common/parallel_runtime_fwd.h>
#include <sprockit/printable.h>
#include <sprockit/sim_parameters_fwd.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/statapi/statbase.h>
#include <sst/core/statapi/statoutput.h>
#else
#include <sprockit/factory.h>
#endif
#include <sstmac/sst_core/integrated_component.h>

#if !SSTMAC_INTEGRATED_SST_CORE
namespace sstmac {

class StatisticBase {
 public:
  virtual bool specialOutput() const { return false; }

  virtual void registerOutputFields(StatisticOutput* statOutput) = 0;

  virtual void outputStatisticData(StatisticOutput* output, bool endOfSimFlag) = 0;

  std::string name() const {
    return name_;
  }

 protected:
  StatisticBase(EventScheduler* parent,
                const std::string& name, const std::string& subName,
                SST::Params& params) :
    name_(name)
  {
  }

 private:
  std::string name_;
};

class StatisticOutput
{
 public:
  SST_ELI_DECLARE_BASE(StatisticOutput)
  SST_ELI_DECLARE_CTOR(SST::Params&)

  StatisticOutput(SST::Params& params){}
  ~StatisticOutput(){}

 public:
  using fieldHandle_t = int;

  template<typename T> fieldHandle_t registerField(const char* fieldName){
    return implRegisterField(fieldName);
  }

  virtual void outputField(fieldHandle_t fieldHandle, int32_t data) = 0;
  virtual void outputField(fieldHandle_t fieldHandle, uint32_t data) = 0;
  virtual void outputField(fieldHandle_t fieldHandle, int64_t data) = 0;
  virtual void outputField(fieldHandle_t fieldHandle, uint64_t data) = 0;
  virtual void outputField(fieldHandle_t fieldHandle, float data) = 0;
  virtual void outputField(fieldHandle_t fieldHandle, double data) = 0;

  void outputEntries(StatisticBase* stat, bool endOfSimFlag) {
    startOutputEntries(stat);
    stat->outputStatisticData(this, endOfSimFlag);
    stopOutputEntries();
  }

  void startRegisterGroup(const std::string& name){
    groups_.emplace(name, Group());
    active_group_ = &groups_[name];
  }

  void stopRegisterGroup(){
    active_group_ = nullptr;
  }

  void startRegisterFields(StatisticBase *statistic) {
    active_stat_ = statistic;
  }

  void stopRegisterFields() {
    active_stat_ = nullptr;
  }

  virtual void startOutputGroup(const std::string& name) = 0;
  virtual void stopOutputGroup() = 0;

  virtual void startOutputEntries(StatisticBase* statistic) = 0;
  virtual void stopOutputEntries() = 0;

 protected:
  struct Group {
    std::map<std::string, int> columns;
  };

 private:
  fieldHandle_t implRegisterField(const char* fieldName){
    Group* grp = active_group_ ? active_group_ : &default_group_;
    std::string fullName = active_stat_->name() + "." + fieldName;
    auto iter = grp->columns.find(fieldName);
    if (iter == grp->columns.end()){
      int idx = grp->columns.size();
      grp->columns[fullName] = idx;
      return idx;
    } else {
      return iter->second;
    }
  }

  std::map<std::string, Group> groups_;

  StatisticBase* active_stat_;

  Group* active_group_;

  Group default_group_;

};

class StatOutputCSV : public StatisticOutput {
 public:
  SST_ELI_REGISTER_DERIVED(
      StatisticOutput,
      StatOutputCSV,
      "macro",
      "csv",
      SST_ELI_ELEMENT_VERSION(1,0,0),
      "writes csv output")

  StatOutputCSV(SST::Params& params) : StatisticOutput(params) {}

  void outputField(fieldHandle_t fieldHandle, int32_t data) override {
    output(fieldHandle, data);
  }

  void outputField(fieldHandle_t fieldHandle, uint32_t data) override {
    output(fieldHandle, data);
  }

  void outputField(fieldHandle_t fieldHandle, int64_t data) override {
    output(fieldHandle, data);
  }

  void outputField(fieldHandle_t fieldHandle, uint64_t data) override {
    output(fieldHandle, data);
  }

  void outputField(fieldHandle_t fieldHandle, float data) override {
    output(fieldHandle, data);
  }

  void outputField(fieldHandle_t fieldHandle, double data) override {
    output(fieldHandle, data);
  }

  void startOutputGroup(const std::string& name) override {
    csv_out_.open(name.c_str());
  }

  void startOutputEntries(StatisticBase *stat) override {
    nextField_ = 0;
  }

  void stopOutputEntries() override {}

  void stopOutputGroup() override {
    csv_out_.close();
  }

 private:
  template <class T> void output(fieldHandle_t handle, T&& data){
    if (handle != 0) csv_out_ << ",";
    if (handle != nextField_){
      std::cout << "Fields not output in order" << std::endl;
      abort();
    }
    csv_out_ << data;
    ++nextField_;
  }

  std::ofstream csv_out_;
  int nextField_;

};


/**
 \class StatisticCollector
 * Base type that creates the virtual addData(...) interface
 * Used for distinguishing fundamental types (collected by value)
 * and composite struct types (collected by reference)
 */
template <class T, bool F=std::is_fundamental<T>::value>
struct StatisticCollector { };

template <class T> struct StatisticCollector<T,true> {
 void addData(T t){
   addData_impl(t);
 }

 virtual void addData_impl(T data) = 0;
};

template <class T>
struct StatisticCollector<T,false>
{
 virtual void addData_impl(T&& data) = 0;
 virtual void addData_impl(const T& data) = 0;
};

template <class... Args>
struct StatisticCollector<std::tuple<Args...>, false>
{
  virtual void addData_impl(Args... args) = 0;

  template <class... InArgs>
  void addData(InArgs&&... in){
    addData_impl(std::forward<InArgs>(in)...);
  }
};

/**
 * A type of logger that collects some kind of statistic
 * and outputs to a file during or at the end of a simulation.
 * Usually, a static instance of this class should be used,
 * because no merging of stat objects takes place, which means
 * you'll get one file for each stat object.
 */
template <class T>
class Statistic :
  public StatisticBase,
  public StatisticCollector<T>
{

 public:
  SST_ELI_DECLARE_BASE(Statistic)
  SST_ELI_DECLARE_CTOR(EventScheduler*, const std::string&, const std::string&, SST::Params&)
  virtual ~Statistic(){}

 protected:
  Statistic(EventScheduler* parent,
            const std::string& name, const std::string& subName,
            SST::Params& params) :
    StatisticBase(parent, name, subName, params)
  {
  }
};

#define SST_ELI_DECLARE_STATISTIC_TEMPLATE(cls,lib,name,version,desc,interface) \
  static const char* SPKT_getLibrary(){ \
    return lib; \
  } \
  static const char* SPKT_getName(){ \
    return name; \
  }

template <class T, bool isFundamental>
class NullStatisticBase :
  public Statistic<T>
{
};

template <class T>
class NullStatisticBase<T,false> : public Statistic<T>
{
 public:
  SST_ELI_DECLARE_STATISTIC_TEMPLATE(
     NullStatistic,
     "macro",
     "null",
     SST_ELI_ELEMENT_VERSION(1,0,0),
     "a null stat that collects nothing",
     "Statistic<...>")

  NullStatisticBase(EventScheduler* parent,
            const std::string& name, const std::string& subName,
            SST::Params& params) :
    Statistic<T>(parent, name, subName, params)
  {
  }

  void addData_impl(T&& data) override {}

  void addData_impl(const T& data) override {}

};

template <class... Args>
class NullStatisticBase<std::tuple<Args...>,false> :
    public Statistic<std::tuple<Args...>>
{
 public:
  SST_ELI_DECLARE_STATISTIC_TEMPLATE(
     NullStatistic,
     "macro",
     "null",
     SST_ELI_ELEMENT_VERSION(1,0,0),
     "a null stat that collects nothing",
     "Statistic<...>")

  NullStatisticBase(EventScheduler* parent,
            const std::string& name, const std::string& subName,
            SST::Params& params) :
    Statistic<std::tuple<Args...>>(parent, name, subName, params)
  {
  }

  void addData_impl(Args... data) override {}

};

template <class T>
class NullStatisticBase<T,true> : public Statistic<T> {
 public:
  SST_ELI_DECLARE_STATISTIC_TEMPLATE(
     NullStatistic,
     "macro",
     "null",
     SST_ELI_ELEMENT_VERSION(1,0,0),
     "a null stat that collects nothing",
     "Statistic<...>")

  NullStatisticBase(EventScheduler* parent,
            const std::string& name, const std::string& subName,
            SST::Params& params) :
    Statistic<T>(parent, name, subName, params)
  {
  }

  void addData_impl(T data) override {}

};

template <class T, bool isFund=std::is_fundamental<T>::value>
class NullStatistic : public NullStatisticBase<T,isFund> {
 public:
  NullStatistic(EventScheduler* parent, const std::string& name,
                const std::string& subName, SST::Params& params) :
    NullStatisticBase<T,isFund>(parent, name, subName, params)
  {
  }

  void outputStatisticData(StatisticOutput *output, bool endOfSimFlag) override {}

  void registerOutputFields(StatisticOutput *statOutput) override {}

  static bool isLoaded(){
    return loaded_;
  }

 private:
  static bool loaded_;
};
template <class T, bool isFund> bool NullStatistic<T,isFund>::loaded_ = true;


} // end of namespace sstmac

namespace SST {
namespace Statistics {

template <class... Args>
using MultiStatistic = Statistic<std::tuple<Args...>>;

}
}

#define SST_ELI_REGISTER_CUSTOM_STATISTIC(parent,cls,lib,name,version,desc) \
  SPKT_REGISTER_DERIVED(parent,cls,lib,name,desc)

#define SST_ELI_INSTANTIATE_STATISTIC(cls,field) \
  struct cls##_##field##_##shortName : public cls<field> { \
    cls##_##field##_##shortName(SST::BaseComponent* bc, const std::string& sn, \
           const std::string& si, SST::Params& p) : \
      cls<field>(bc,sn,si,p) {} \
    bool SPKT_isLoaded() const { \
     return sprockit::InstantiateBuilder< \
         SST::Statistics::Statistic<field>, \
         cls##_##field##_##shortName>::isLoaded() \
        && sprockit::InstantiateBuilder< \
         SST::Statistics::Statistic<field>, \
         sstmac::NullStatistic<field>>::isLoaded(); \
    } \
  };

#define PP_NARG(...) PP_NARG_(__VA_ARGS__, PP_NSEQ())
#define PP_NARG_(...) PP_ARG_N(__VA_ARGS__)
#define PP_ARG_N(_1,_2,_3,_4,_5,N,...) N
#define PP_NSEQ() 5,4,3,2,1,0

#define PP_GLUE(X,Y) PP_GLUE_I(X,Y)
#define PP_GLUE_I(X,Y) X##Y

#define STAT_NAME1(base,a) base##a
#define STAT_NAME2(base,a,b) base##a##b
#define STAT_NAME3(base,a,b,c) base##a##b##c
#define STAT_NAME4(base,a,b,c,d) base##a##b##c##d

#define STAT_GLUE_NAME(base,...) PP_GLUE(STAT_NAME,PP_NARG(__VA_ARGS__))(base,__VA_ARGS__)
#define STAT_TUPLE(...) std::tuple<__VA_ARGS__>

#define MAKE_MULTI_STATISTIC(cls,name,tuple,...) \
  struct name : public cls<__VA_ARGS__> { \
    name(SST::BaseComponent* bc, const std::string& sn, \
         const std::string& si, SST::Params& p) : \
      cls<__VA_ARGS__>(bc,sn,si,p) {} \
    bool SPKT_isLoaded() const { \
        return sprockit::InstantiateBuilder<SST::Statistics::Statistic<tuple>,name>::isLoaded() \
        && sprockit::InstantiateBuilder<SST::Statistics::Statistic<tuple>,sstmac::NullStatistic<tuple>>::isLoaded(); \
    } \
    static const char* SPKT_fieldName(){ return #tuple; } \
    static const char* SPKT_fieldShortName(){ return #tuple; } \
  };

#define SST_ELI_INSTANTIATE_MULTI_STATISTIC(cls,...) \
  MAKE_MULTI_STATISTIC(cls,STAT_GLUE_NAME(cls,__VA_ARGS__),STAT_TUPLE(__VA_ARGS__),__VA_ARGS__)


#endif
#endif

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
#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_scheduler_fwd.h>
#include <sstmac/backends/common/parallel_runtime_fwd.h>
#include <sprockit/factories/factory.h>
#include <sprockit/printable.h>

namespace sstmac {

class StatisticBase {

};

#if 0
class StatisticOutput : public Module
{
 public:
  StatisticOutput(Params& outputParameters);
  ~StatisticOutput();

 public:
  template<typename T> fieldHandle_t registerField(const char* fieldName){
    StatisticFieldInfo::fieldType_t FieldType =
        StatisticFieldInfo::StatisticFieldInfo::getFieldTypeFromTemplate<T>();

    auto res = generateFileHandle(addFieldToLists(fieldName, FieldType));
    implRegisteredField(res);
    return res;
  }

  /** Return the information on a registered field via the field handle.
   * @param fieldHandle - The handle of the registered field.
   * @return Pointer to the registered field info.
   */
  // Get the Field Information object, NULL is returned if not found
  StatisticFieldInfo* getRegisteredField(fieldHandle_t fieldHandle);

  /** Return the information on a registered field via known names.
   * @param componentName - The name of the component.
   * @param statisticName - The name of the statistic.
   * @param fieldName - The name of the field .
   * @return Pointer to the registered field info.
   */
  // Get Registered Fields
  // ONLY SUPPORTED TYPES ARE int32_t, uint32_t, int64_t, uint64_t, float, double
  template<typename T>
  StatisticFieldInfo* getRegisteredField(const char* statisticName, const char* fieldName)
  {
      StatisticFieldInfo*             NewStatFieldInfo;
      StatisticFieldInfo*             ExistingStatFieldInfo;
      StatisticFieldInfo::fieldType_t FieldType = StatisticFieldInfo::StatisticFieldInfo::getFieldTypeFromTemplate<T>();

      NewStatFieldInfo = new StatisticFieldInfo(statisticName, fieldName, FieldType);

      // Now search the FieldNameMap_t of type for a matching entry
      FieldNameMap_t::const_iterator found = m_outputFieldNameMap.find(NewStatFieldInfo->getFieldUniqueName());
      if (found != m_outputFieldNameMap.end()) {
          // We found a map entry, now get the StatFieldInfo from the m_outputFieldInfoArray at the index given by the map
          // and then delete the NewStatFieldInfo to prevent a leak
          ExistingStatFieldInfo = m_outputFieldInfoArray[found->second];
          delete NewStatFieldInfo;
          return ExistingStatFieldInfo;
      }

      delete NewStatFieldInfo;
      return NULL;
  }

  /** Return the array of registered field infos. */
  FieldInfoArray_t& getFieldInfoArray() {return m_outputFieldInfoArray;}

/////////////////
  // Methods for Outputting Fields  (Called by Statistic Objects)
  // Output fields (will call virtual functions of Derived Output classes)
  // These aren't really part of a generic interface - optimization purposes only
  /** Output field data.
   * @param fieldHandle - The handle of the registered field.
   * @param data - The data to be output.
   */
  virtual void outputField(fieldHandle_t fieldHandle, int32_t data);
  virtual void outputField(fieldHandle_t fieldHandle, uint32_t data);
  virtual void outputField(fieldHandle_t fieldHandle, int64_t data);
  virtual void outputField(fieldHandle_t fieldHandle, uint64_t data);
  virtual void outputField(fieldHandle_t fieldHandle, float data);
  virtual void outputField(fieldHandle_t fieldHandle, double data);

  /** Output field data.
   * @param type - The field type to get name of.
   * @return String name of the field type.
   */
  const char* getFieldTypeShortName(fieldType_t type);

 protected:
  friend class SST::Simulation;
  friend class SST::Statistics::StatisticProcessingEngine;

  // Routine to have Output Check its options for validity
  /** Have the Statistic Output check its parameters
   * @return True if all parameters are ok; False if a parameter is missing or incorrect.
   */
  virtual bool checkOutputParameters() = 0;

  /** Have Statistic Object print out its usage and parameter info.
   *  Called when checkOutputParameters() returns false */
  virtual void printUsage() = 0;


  virtual void implStartRegisterFields(StatisticBase *UNUSED(statistic)) {}
  virtual void implRegisteredField(fieldHandle_t UNUSED(fieldHandle)) {}
  virtual void implStopRegisterFields() {}

  // Simulation Events
  /** Indicate to Statistic Output that simulation has started.
    * Allows object to perform any setup required. */
  virtual void startOfSimulation() = 0;

  /** Indicate to Statistic Output that simulation has ended.
    * Allows object to perform any shutdown required. */
  virtual void endOfSimulation() = 0;

  // Start / Stop of output
  /** Indicate to Statistic Output that a statistic is about to send data to be output
    * Allows object to perform any initialization before output. */
  virtual void implStartOutputEntries(StatisticBase* statistic) = 0;

  /** Indicate to Statistic Output that a statistic is finished sending data to be output
    * Allows object to perform any cleanup. */
  virtual void implStopOutputEntries() = 0;

  virtual void implStartRegisterGroup(StatisticGroup* UNUSED(group)) {}
  virtual void implStopRegisterGroup() {}
  virtual void implStartOutputGroup(StatisticGroup* UNUSED(group)) {}
  virtual void implStopOutputGroup() {}


 private:
  // Start / Stop of register Fields
  void registerStatistic(StatisticBase *stat);

  void startRegisterFields(StatisticBase *statistic);
  void stopRegisterFields();

  // Start / Stop of output
  void outputEntries(StatisticBase* statistic, bool endOfSimFlag);
  void startOutputEntries(StatisticBase* statistic);
  void stopOutputEntries();

  // Other support functions
  StatisticFieldInfo* addFieldToLists(const char* fieldName, fieldType_t fieldType);
  fieldHandle_t generateFileHandle(StatisticFieldInfo* FieldInfo);


 protected:
  StatisticOutput() {;} // For serialization only

};
#endif

/**
 \class StatisticCollector
 * Base type that creates the virtual addData(...) interface
 * Used for distinguishing fundamental types (collected by value)
 * and composite struct types (collected by reference)
 */
template <class T, bool F=std::is_fundamental<T>::value>
struct StatisticCollector { };

template <class T>
struct StatisticCollector<T,true> {
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
  DeclareFactory(Statistic)
 public:
  virtual ~Statistic(){}

  // Required Virtual Methods:
  //virtual void registerOutputFields(StatisticOutput* statOutput) = 0;

  //virtual void outputStatisticData(StatisticOutput* statOutput, bool EndOfSimFlag) = 0;
 protected:
  Statistic(sprockit::sim_parameters::ptr& params){}
};

template <class... Args>
using MultiStatistic = Statistic<std::tuple<Args...>>;

template <class T>
class StatValue : public Statistic<T>
{
 public:
  void addData_impl(T val){
    value_ += val;
  }

 protected:
  StatValue(sprockit::sim_parameters::ptr& params) :
    Statistic<T>(params)
  {
  }

  T value_;
};



} // end of namespace sstmac
#endif

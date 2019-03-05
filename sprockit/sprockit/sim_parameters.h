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

#ifndef SPROCKIT_COMMON_SIM_PARAMETERS_H_INCLUDED
#define SPROCKIT_COMMON_SIM_PARAMETERS_H_INCLUDED

#include <sprockit/debug.h>

#include <sprockit/sim_parameters_fwd.h>
#include <unordered_map>

#include <sstream>
#include <iostream>
#include <list>
#include <vector>
#include <set>

#include <sstmac/common/sstmac_config.h>
#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/params.h>
#include <sst/core/unitAlgebra.h>
#else
namespace SST {
class Params;
}
#include <sprockit/serialize.h>
#endif

#include <sprockit/basic_string_tokenizer.h>

DeclareDebugSlot(params)
DeclareDebugSlot(read_params)
DeclareDebugSlot(write_params)

namespace sprockit {

bool getQuantityWithUnits(const char *value, double& ret);
double getQuantityWithUnits(const char *value, const char* key);

class ParamAssign {
 public:
  ParamAssign(std::string& p, const std::string& k) :
    param_(p), key_(k)
  {
  }

  void operator=(int a);
  void operator=(double x);
  void operator=(const std::string& str){
    param_ = str;
  }

  const std::string& setByteLength(long x, const char* units);
  const std::string& setBandwidth(double x, const char* units);
  const std::string& setFrequency(double x, const char* units);
  const std::string& setTime(double x, const char* units);
  const std::string& setValue(double x, const char* units);
  const std::string& set(const char* str);
  const std::string& set(const std::string& str);

  long getByteLength() const;
  double getBandwidth() const;
  double getTime() const;
  double getFrequency() const;

  operator int() const;

  operator double() const;

  operator std::string() const {
    return param_;
  }

 private:
  std::string& param_;
  const std::string& key_;

};

class ParamBcaster {
 public:
  virtual void bcast(void* buf, int size, int me, int root) = 0;

  void bcastString(std::string& str, int me, int root);
};

class SimParameters  {
 public:
  friend class SST::Params;

  using ptr = std::shared_ptr<SimParameters>;
  using const_ptr = std::shared_ptr<const SimParameters>;

  struct parameter_entry
  {
    parameter_entry() : read(false) {}
    std::string value;
    bool read;
  };

  bool empty() const {
    return params_.empty();
  }

  void reproduceParams(std::ostream& os) const ;

  typedef std::unordered_map<std::string, parameter_entry> key_value_map;

  SimParameters();

  SimParameters(const key_value_map& p);

  SimParameters(const std::string& filename);

  SimParameters(sprockit::SimParameters::const_ptr params); //deep copy

  /**
   * In a parallel environment (abstracted through a param_bcaster object),
   * have rank 0 read params from a file and bcast result to all other ranks
   * @param params  A parameter object (already allocated)
   * @param me  The rank of the calling process
   * @param nproc The total number of ranks
   * @param filename
   * @param bcaster
   */
  static void
  parallelBuildParams(sprockit::SimParameters::ptr& params,
                        int me, int nproc,
                        const std::string& filename,
                        ParamBcaster* bcaster,
                        bool fail_if_not_found = true);

  virtual ~SimParameters();

  /**
   * Needed in conjunction with moved constructors to clear map
   * and keep things from getting deleted
   */
  void moved();

  void removeParam(const std::string &key);

  std::string getVariable(const std::string& str);

  std::string getParam(const std::string& key, bool throw_on_error = true);

  std::string getLowercaseParam(const std::string& key, bool throw_on_error = true);

  std::string getScopedParam(const std::string& key, bool throw_on_error = true);

  void insertInto(SST::Params& params);

  SimParameters::ptr getOptionalLocalNamespace(const std::string& ns);

  /// Return the value of the keyword if it exists. Otherwise return
  /// a default value.
  /// @param key gives the keyword
  /// @param def gives the default value (used if has_param(key) is false)
  /// @return the value if it exists, otherwise the default
  std::string getOptionalParam(const std::string &key, const std::string &def);

  void addParam(const std::string& key, const std::string& val);

  void copyParam(const std::string& oldname, const std::string& newname);

  void copyOptionalParam(const std::string& oldname, const std::string& newname);

  void addParamOverride(const std::string& key, const std::string& val);

  void addParamOverride(const std::string &key, double val);

  void addParamOverride(const std::string& key, double val, const char* units);

  void addParamOverride(const std::string& key, int val);

  void addParamOverrideRecursive(const std::string& key, int val);

  void addParamOverrideRecursive(const std::string& key, const std::string& val);

  void combineInto(SimParameters::ptr sp,
               bool fail_on_existing = false,
               bool override_existing = true,
               bool mark_as_read = true);

  std::string printScopedParams(std::ostream& os) const;

  std::string print_scopes(std::ostream& os);

  void printLocalParams(std::ostream& os, const std::string& prefix) const;

  void printParams(std::ostream& os = std::cerr, const std::string& prefix = "") const;

  bool hasParam(const std::string& key) const;

  int getIntParam(const std::string& key);

  /// Return the value of the keyword if it exists. Otherwise return
  /// a default value.
  /// @param key gives the keyword
  /// @param def gives the default value (used if has_param(key) is false)
  /// @return the value if it exists, otherwise the default
  int getOptionalIntParam(const std::string &key, int def);

  /// Returns the value of the key as a boolean.
  bool getBoolParam(const std::string &key);

  /// Return the value of the keyword if it exists. Otherwise return
  /// a default value.
  /// @param key gives the keyword
  /// @param def gives the default value (used if has_param(key) is false)
  /// @return the value if it exists, otherwise the default
  bool getOptionalBoolParam(const std::string &key, bool def);

  double getBandwidthParam(const std::string& key);

  /**
   @param key The parameter name
   @return A value with units. This loops through bandwidth, frequency, time, etc
           to return any value that can have units. Everything is normalized to baseslines
           of B/s, s, Hz, etc
  */
  double getQuantity(const std::string& key);

  double getOptionalQuantity(const std::string& key, double def);

  double getOptionalBandwidthParam(const std::string &key, double def);

  double getOptionalBandwidthParam(
    const std::string& key,
    const std::string& def);

  long getByteLengthParam(const std::string& key);

  long getOptionalByteLengthParam(const std::string& key, long def);

  double getOptionalFreqParam(const std::string& key, double def);

  double getFreqParam(const std::string& key);

  /// Return the value of the keyword if it exists. Otherwise return
  /// a default value.
  /// @param key gives the keyword
  /// @param def gives the default value (used if has_param(key) is false)
  /// @return the value if it exists, otherwise the default
  long getOptionalLongParam(const std::string &key, long def);

  long getLongParam(const std::string& key);

  double getDoubleParam(const std::string& key);

  /// Return the value of the keyword if it exists. Otherwise return
  /// a default value.
  /// @param key gives the keyword
  /// @param def gives the default value (used if has_param(key) is false)
  /// @return the value if it exists, otherwise the default
  double getOptionalDoubleParam(const std::string &key, double def);

  double getTimeParam(const std::string& key);

  double getOptionalTimeParam(const std::string &key, double def);

  template <class T> void getVectorParam(const std::string& key, std::vector<T>& vals){
    std::deque<std::string> toks = getTokenizer(key);
    for (auto& item : toks){
      if (item.size() > 0) {
        std::stringstream sstr(item);
        T val;
        sstr >> val;
        vals.push_back(val);
      }
    }
  }

  SimParameters::ptr getNamespace(const std::string& ns);

  SimParameters::ptr getOptionalNamespace(const std::string& ns);

  void setNamespace(const std::string& ns, SimParameters::ptr params){
    subspaces_[ns] = params;
  }

  static std::pair<std::string, std::string> split_line(const std::string& line);

  bool hasNamespace(const std::string& ns) const;

  void parseFile(const std::string& fname, bool fail_on_existing,
             bool override_existing, bool fail_if_not_found = true);

  void parseStream(std::istream& in, bool fail_on_existing, bool override_existing);

  void parseLine(const std::string& line, bool fail_on_existing, bool override_existing);

  /**
    @param key
    @param value
    @param fail_on_existing Fail if the parameter named by key already exists
  */
  void parseKeyval(const std::string& key,
    const std::string& value,
    bool fail_on_existing,
    bool override_existing,
    bool mark_as_read);


  void insertInto(const std::string& prefix, SST::Params& params);

  ParamAssign operator[](const std::string& key);

  key_value_map::iterator begin() { return params_.begin(); }
  key_value_map::const_iterator begin() const { return params_.begin(); }

  key_value_map::iterator end() { return params_.end(); }
  key_value_map::const_iterator end() const { return params_.end(); }

  using namespace_iterator = std::map<std::string, SimParameters::ptr>::iterator ;
  using const_namespace_iterator = std::map<std::string, SimParameters::ptr>::const_iterator;
  namespace_iterator nsBegin() { return subspaces_.begin(); }
  const_namespace_iterator nsBegin() const { return subspaces_.begin(); }
  namespace_iterator nsEnd() { return subspaces_.end(); }
  const_namespace_iterator nsEnd() const { return subspaces_.end(); }

 private:
  std::map<std::string, SimParameters::ptr> subspaces_;
  std::map<std::string, std::string> variables_;

  SimParameters* parent_;

  std::string namespace_;

  key_value_map params_;

  uint64_t current_id_;

  /**
   * @brief _get_namespace Get a parameter namespace. If the namespace does not exist in the current scope locally,
   *  search through any parent namespaces. This follow C++ scoping rules as if you had requested ns::variable.
   * @param ns The namespace to get
   * @return The set of all parameters in a given param namespace
   */
  SimParameters::ptr _get_namespace(const std::string &ns);

  SimParameters::ptr buildLocalNamespace(const std::string& ns);

  void throwKeyError(const std::string& key) const;

  void setParent(SimParameters* p) {
    parent_ = p;
  }

  void setNamespace(const std::string& str) {
    namespace_ = str;
  }

  bool localHasNamespace(const std::string& ns) const {
    return subspaces_.find(ns) != subspaces_.end();
  }

  void tryToParse(const std::string& fname, bool fail_on_existing, bool override_existing);

  void printParams(const key_value_map& pmap, std::ostream& os, bool pretty_print, std::list<std::string>& ns) const;

  void doAddParam(const std::string& key,
    const std::string& val,
    bool fail_on_existing,
    bool override_existing,
    bool mark_as_read);


  SimParameters* getScopeAndKey(const std::string& key, std::string& final_key);

  bool getParam(std::string& inout, const std::string& key);

  bool getScopedParam(std::string& inout, const std::string& key);

  std::deque<std::string> getTokenizer(const std::string& key);

};

}

#if !SSTMAC_INTEGRATED_SST_CORE
namespace SST {

template <class T>
struct CallGetParam {};

template <> struct CallGetParam<long>  {
  static double get(sprockit::SimParameters::ptr& ptr, const std::string& key){
    return ptr->getLongParam(key);
  }
  static double getOptional(sprockit::SimParameters::ptr &ptr, const std::string& key, long def){
    return ptr->getOptionalLongParam(key, def);
  }
};

template <> struct CallGetParam<double>  {
  static double get(sprockit::SimParameters::ptr& ptr, const std::string& key){
    return ptr->getDoubleParam(key);
  }
  static double getOptional(sprockit::SimParameters::ptr &ptr, const std::string& key, double def){
    return ptr->getOptionalDoubleParam(key, def);
  }
};

template <> struct CallGetParam<int>  {
  static int get(sprockit::SimParameters::ptr& ptr, const std::string& key){
    return ptr->getIntParam(key);
  }
  static int getOptional(sprockit::SimParameters::ptr &ptr, const std::string& key, int def){
    return ptr->getOptionalIntParam(key, def);
  }
};

template <> struct CallGetParam<bool>  {
  static int get(sprockit::SimParameters::ptr& ptr, const std::string& key){
    return ptr->getBoolParam(key);
  }
  static int getOptional(sprockit::SimParameters::ptr &ptr, const std::string& key, bool def){
    return ptr->getOptionalBoolParam(key, def);
  }
};

template <> struct CallGetParam<std::string> {
  static std::string get(sprockit::SimParameters::ptr& ptr, const std::string& key){
    return ptr->getParam(key);
  }

  static std::string getOptional(sprockit::SimParameters::ptr& ptr,
                                 const std::string& key, std::string&& def){
    return ptr->getOptionalParam(key, def);
  }

  static std::string getOptional(sprockit::SimParameters::ptr& ptr,
                                 const std::string& key, const std::string& def){
    return ptr->getOptionalParam(key, def);
  }

};

struct UnitAlgebra
{
 public:
  UnitAlgebra(const std::string& val){
    double tmp;
    bool failed = sprockit::getQuantityWithUnits(val.c_str(), tmp);
    if (failed){
      std::cerr << "Failed to parse value with units from " << val << std::endl;
      ::abort();
    }
    value_ = tmp;
  }

  UnitAlgebra inverse(){
    return UnitAlgebra(1.0/value_);
  }

  int64_t getRoundedValue() const {
    return value_;
  }

  const UnitAlgebra& getValue() const {
    return *this;
  }

  double toDouble() const {
    return value_;
  }

  UnitAlgebra& getValue(){
    return *this;
  }

 private:
  UnitAlgebra(double v) : value_(v){}

  double value_;

};

template <> struct CallGetParam<UnitAlgebra>  {
  static UnitAlgebra get(sprockit::SimParameters::ptr& ptr, const std::string& key){
    return UnitAlgebra(ptr->getParam(key));
  }
  static UnitAlgebra getOptional(sprockit::SimParameters::ptr &ptr, const std::string& key, const std::string& def){
    return UnitAlgebra(ptr->getOptionalParam(key,def));
  }
};

class Params {
 public:
  Params() : params_(std::make_shared<sprockit::SimParameters>())
  {
  }

  Params(const std::string& name) : params_(std::make_shared<sprockit::SimParameters>(name))
  {
  }

  explicit operator bool(){
    return bool(params_);
  }

  Params(const sprockit::SimParameters::ptr& params) :
    params_(params)
  {
  }

  template <class T> void find_array(const std::string& key, std::vector<T>& vec){
    return params_->getVectorParam(key, vec);
  }

  bool contains(const std::string& k) const {
    return params_->hasParam(k);
  }

  void print_all_params(std::ostream& os){
    params_->printParams(os);
  }

  sprockit::SimParameters* operator->(){
    return params_.get();
  }

  SST::Params get_namespace(const std::string& name){
    return params_->getNamespace(name);
  }

  SST::Params find_prefix_params(const std::string& name){
    return params_->getOptionalNamespace(name);
  }

  void insert(const std::string& key, const std::string& value, bool overwrite=true){
    if (overwrite){
      params_->addParamOverride(key, value);
    } else {
      params_->addParam(key, value);
    }
  }

  std::set<std::string> getKeys() const {
    std::set<std::string> keys;
    for (auto it=params_->begin(); it != params_->end(); ++it){
      keys.insert(it->first);
    }
    return keys;
  }

  bool empty() const {
    return params_->empty();
  }

  void insert(const SST::Params& params){
    params.params_->combineInto(params_);
  }

  template <class T> T find(const std::string& key) {
    return CallGetParam<T>::get(params_, key);
  }

  template <class T, class U> T find(const std::string& key, U&& def) {
    return CallGetParam<T>::getOptional(params_, key, std::forward<U>(def));
  }

  sprockit::ParamAssign operator[](const std::string& key){
    return (*params_)[key];
  }

  sprockit::ParamAssign operator[](const char* key){
    return (*params_)[key];
  }

  void combine_into(SST::Params& params,
               bool fail_on_existing = false,
               bool override_existing = true,
               bool mark_as_read = true){
    params_->combineInto(params.params_,
      fail_on_existing,override_existing,mark_as_read);
  }

 private:
  sprockit::SimParameters::ptr params_;
};

}

namespace sprockit {

template <> class serialize<SST::Params>
{
 public:
  void operator()(SST::Params& p, serializer& ser){
    if (ser.mode() == ser.UNPACK){
      std::string paramStr;
      ser & paramStr;
      std::stringstream sstr(paramStr);
      p->parseStream(sstr, false, true);
    } else {
      std::stringstream sstr;
      p->reproduceParams(sstr);
      std::string paramStr = sstr.str();
      ser & paramStr;
    }
  }
};



}

#endif
//end if not integrated core


#endif

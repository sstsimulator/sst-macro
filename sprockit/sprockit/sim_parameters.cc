/**
Copyright 2009-2021 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2021, NTESS

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

#include <sprockit/spkt_string.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/units.h>
#include <sprockit/driver_util.h>
#include <sprockit/errors.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/fileio.h>
#include <sprockit/output.h>
#include <cstring>

RegisterDebugSlot(params,
    "print all the details of the initial reading parameters from the input file"
    ", all usage of parameters within the application"
    ", and also any internal overriding or automatic parameter generation");
RegisterDebugSlot(read_params,
  "print all the details of reading and using paramters within the application");
RegisterDebugSlot(write_params,
  "print all the details of writing or overriding parameters within the application"
  "  - this includes the initial reading of parameters from the input file");

namespace sprockit {

bool
getQuantityWithUnits(const char *value, double& ret)
{
  bool error;
  ret = getTimeDelta(value, error);
  if (!error) return false;
  ret= getBandwidth(value, error);
  if (!error) return false;
  ret = getFrequency(value, error);
  if (!error) return false;
  ret = byteLength(value, error);
  if (!error) return false;


  const char* begin = value;
  char* end = const_cast<char*>(begin);
  ret = ::strtod(begin, &end);

  while (*end==' ') ++end;
  int size = (int)((size_t)end - (size_t)begin);

  bool failed = begin == end || size != ::strlen(value);
  return failed;
}

double 
getQuantityWithUnits(const char* value, const char* key)
{
  double ret;
  bool failed = getQuantityWithUnits(value, ret);
  if (failed) {
    spkt_abort_printf("sim_parameters::get_quantity: param %s with value %s"
        " is not formatted as a double with units (Hz,GB/s,ns,KB)",
        key, value);
  }
  return ret;
}

double
get_freq_from_str(const char* val, const char* key)
{
  bool errorflag = false;
  double ret = getFrequency(val, errorflag);
  if (errorflag) {
    spkt_abort_printf("improperly formatted frequency (%s) for parameter %s", val, key);
  }
  return ret;
}

long
get_byte_length_from_str(const char* val, const char* key)
{
  bool errorflag = false;
  double ret = byteLength(val, errorflag);
  if (errorflag) {
    spkt_abort_printf("improperly formatted byte length (%s) for parameter %s", val, key);
  }
  return ret;
}

double
get_time_from_str(const char* val, const char* key)
{
  bool errorflag = false;
  double ret = getTimeDelta(val, errorflag);
  if (errorflag) {
    spkt_abort_printf("improperly formatted time (%s) for parameter %s", val, key);
  }
  return ret;
}

double
get_bandwidth_from_str(const char* val, const char* key)
{
  bool errorflag = false;
  double ret = getBandwidth(val, errorflag);
  if (errorflag) {
    spkt_abort_printf("improperly formatted bandwidth (%s) for parameter %s", val, key);
  }
  return ret;
}

ParamAssign::operator int() const
{
  return getQuantityWithUnits(param_.c_str(), key_.c_str());
}

ParamAssign::operator double() const
{
  return getQuantityWithUnits(param_.c_str(), key_.c_str());
}

void
ParamAssign::operator=(int x)
{
  param_ = sprockit::sprintf("%d", x); 
}

void
ParamAssign::operator=(double x)
{
  param_ = sprockit::sprintf("%f", x); 
}

double
ParamAssign::getBandwidth() const
{
  return get_bandwidth_from_str(param_.c_str(), key_.c_str()); 
}

double
ParamAssign::getFrequency() const
{
  return get_freq_from_str(param_.c_str(), key_.c_str()); 
}

long
ParamAssign::getByteLength() const
{
  return get_byte_length_from_str(param_.c_str(), key_.c_str());
}

double
ParamAssign::getTime() const
{
  return get_time_from_str(param_.c_str(), key_.c_str());
}

const std::string&
ParamAssign::set(const char* str)
{
  param_ = str;
  return param_;
}

const std::string&
ParamAssign::set(const std::string& str)
{
  param_ = str;
  return param_;
}

const std::string&
ParamAssign::setValue(double x, const char* units)
{
  param_ = sprockit::sprintf("%f%s", x, units);
  return param_;
}

const std::string&
ParamAssign::setTime(double x, const char* units)
{
  return setValue(x, units);
}

const std::string&
ParamAssign::setBandwidth(double x, const char* units)
{
  return setValue(x, units);
}

const std::string&
ParamAssign::setFrequency(double x, const char* units)
{
  return setValue(x, units);
}

const std::string&
ParamAssign::setByteLength(long x, const char* units)
{
  param_ = sprockit::sprintf("%ld%s", x, units);
  return param_;
}

SimParameters::SimParameters() :
  parent_(nullptr)
{
}

SimParameters::SimParameters(SimParameters::const_ptr params) :
  parent_(nullptr),
  namespace_(params->namespace_)
{
  ::abort();

  params_ = params->params_;
  for (auto& pair : params->subspaces_){
    subspaces_[pair.first] = std::make_shared<SimParameters>(pair.second);
  }
}

SimParameters::SimParameters(const key_value_map& p) :
  parent_(nullptr),
  namespace_("global"),
  params_(p)
{
}

void
SimParameters::moved()
{
  params_.clear();
}

SimParameters::SimParameters(const std::string& filename) :
  parent_(nullptr),
  namespace_("global")
{
  //don't fail, but don't overwrite anything
  //parameters from file get lowest priority
  parseFile(filename, false, false);
}

SimParameters::~SimParameters()
{
}

bool
SimParameters::hasNamespace(const std::string &ns) const
{
  bool found = localHasNamespace(ns);
  if (!found && parent_){
    return parent_->hasNamespace(ns);
  } else {
    return found;
  }
}

SimParameters::ptr
SimParameters::getNamespace(const std::string& ns)
{
  sprockit::SimParameters::ptr params = _get_namespace(ns);
  if (!params){
    print_scopes(std::cerr);
    spkt_abort_printf("cannot enter namespace %s, does not exist inside namespace %s",
      ns.c_str(), namespace_.c_str());
  }
  return params;
}

SimParameters::ptr
SimParameters::_get_namespace(const std::string &ns)
{
  KeywordRegistration::validateNamespace(ns);
  auto it = subspaces_.find(ns);
  if (it == subspaces_.end()){
    return nullptr;
  } else {
    return it->second;
  }
}

void
SimParameters::copyParam(const std::string &oldname, const std::string &newname)
{
  addParamOverride(newname, getParam(oldname));
}

void
SimParameters::copyOptionalParam(const std::string &oldname, const std::string &newname)
{
  if (hasParam(oldname))
    addParamOverride(newname, getParam(oldname));
}

void
SimParameters::addParamOverride(const std::string &key, double val, const char* units)
{
  addParamOverride(key, sprockit::sprintf("%20.8f%s", val, units));
}


void
SimParameters::addParamOverride(const std::string &key, double val)
{
  addParamOverride(key, sprockit::sprintf("%20.8f", val));
}

void
SimParameters::addParamOverride(const std::string &key, int val)
{
  addParamOverride(key, sprockit::sprintf("%d", val));
}

void
SimParameters::addParamOverrideRecursive(const std::string &key, int val)
{
  std::string valStr = sprockit::sprintf("%d", val);
  addParamOverrideRecursive(key, valStr);
}

void
SimParameters::addParamOverrideRecursive(const std::string &key, const std::string& val)
{
  addParamOverride(key,val);
  for (auto& pair : subspaces_){
    pair.second->addParamOverrideRecursive(key,val);
  }
}

std::string
SimParameters::getOptionalParam(const std::string &key, const std::string &def)
{
  if (hasParam(key)){
    return getParam(key);
  } else {
    return def;
  }
  /**
  if (has_param(key)) {
    return get_param(key);
  } else if (parent_){
    return parent_->get_optional_param(key,def);
  } else {
   return def;
  }
  */
}

SimParameters::ptr
SimParameters::getOptionalLocalNamespace(const std::string& ns)
{
  auto it = subspaces_.find(ns);
  if (it == subspaces_.end()){
    return buildLocalNamespace(ns);
  } else {
    return it->second;
  }
}

SimParameters::ptr
SimParameters::buildLocalNamespace(const std::string& ns)
{
  //need to make a new one
  auto params = std::make_shared<SimParameters>();
  params->setNamespace(ns);
  params->setParent(this);
  subspaces_[ns] = params;
  return params;
}

void
SimParameters::reproduceParams(std::ostream& os) const
{
  for (auto& pair : params_){
    os << pair.first << " = " << pair.second.value << "\n";
  }
  for (auto& pair : subspaces_){
    os << pair.first << " {\n";
    pair.second->reproduceParams(os);
    os << "}\n";
  }
}

SimParameters::ptr
SimParameters::getOptionalNamespace(const std::string& ns)
{
  //if the namespace does not exist locally, see if parent has it
  SimParameters::ptr params = _get_namespace(ns);

  //a bit dangerous, but, that's the fault of the person who made the input file
  //you might think you are operating on a private namespace
  //but in fact are operating on a shared namespace
  if (params) return params;

  return buildLocalNamespace(ns);
}

long
SimParameters::getLongParam(const std::string &key)
{
  std::string v = getParam(key);
  const char* begin = v.c_str();
  char* end = const_cast<char*>(begin);
  long ret = ::strtol(begin, &end, 0);
  if (begin == end) {
    spkt_abort_printf("sim_parameters::get_long_param: param %s with value %s is not formatted as an integer",
                     key.c_str(), v.c_str());
  }
  return ret;
}

long
SimParameters::getOptionalLongParam(const std::string &key, long def)
{
  if (hasParam(key)) {
    return getLongParam(key);
  }
  return def;
}


double
SimParameters::getTimeParam(const std::string& key)
{
  return get_time_from_str(getParam(key).c_str(), key.c_str());

}

double
SimParameters::getOptionalTimeParam(const std::string &key,
                                        double def)
{
  if (hasParam(key)) {
    return getTimeParam(key);
  }
  return def;
}

double
SimParameters::getQuantity(const std::string& key)
{
  std::string value = getParam(key);
  return getQuantityWithUnits(value.c_str(), key.c_str());
}

double
SimParameters::getOptionalQuantity(const std::string &key, double def)
{
  if (hasParam(key)){
    return getQuantity(key);
  } else {
    return def;
  }
}

double
SimParameters::getDoubleParam(const std::string& key)
{
  std::string v = getParam(key);
  const char* begin = v.c_str();
  char* end = const_cast<char*>(begin);
  double ret = ::strtod(begin, &end);
  if (begin == end) {
    spkt_abort_printf("sim_parameters::get_double_param: param %s with value %s is not formatted as a double",
                     key.c_str(), v.c_str());
  }
  return ret;
}

double
SimParameters::getOptionalDoubleParam(const std::string &key, double def)
{
  if (hasParam(key)) {
    return getDoubleParam(key);
  }

  return def;
}

int
SimParameters::getOptionalIntParam(const std::string &key, int def)
{
  if (hasParam(key)) {
    return getIntParam(key);
  }
  return def;
}

int
SimParameters::getIntParam(const std::string& key)
{
  std::string v = getParam(key);
  const char* begin = v.c_str();
  char* end = const_cast<char*>(begin);
  long ret = ::strtol(begin, &end, 0);
  if (begin == end) {
    spkt_abort_printf("sim_parameters::get_int_param: param %s with value %s is not formatted as an integer",
                     key.c_str(), v.c_str());
  }
  return ret;
}

bool
SimParameters::getOptionalBoolParam(const std::string &key, bool def)
{
  if (hasParam(key)) {
    return getBoolParam(key);
  }
  return def;
}

bool
SimParameters::getBoolParam(const std::string &key)
{
  std::string v = getParam(key);
  if (v == "true" || v == "1") {
    return true;
  } else if (v != "false" && v != "0") {
    spkt_abort_printf("sim_parameters::get_bool_param: param %s with value %s is not formatted as a proper boolean",
                     key.c_str(), v.c_str());
  }
  return false;
}

std::deque<std::string>
SimParameters::getTokenizer(const std::string& key)
{
  std::deque<std::string> tok;
  std::string space =  ",";
  std::string param_value_str = getParam(key);
  auto start_pos = param_value_str.find("[");
  auto end_pos = param_value_str.find("]");
  if (start_pos == std::string::npos || end_pos == std::string::npos){
    spkt_abort_printf("mis-formatted vector %s=%s - must be [a,b,c]",
                      key.c_str(), param_value_str.c_str());
  }
  param_value_str = param_value_str.substr(start_pos+1, end_pos-1);
  pst::BasicStringTokenizer::tokenize(param_value_str, tok, space);
  return tok;
}

double
SimParameters::getFreqParam(const std::string &key)
{
  std::string param_value_str = getParam(key);
  return get_freq_from_str(param_value_str.c_str(), key.c_str());
}

double
SimParameters::getOptionalFreqParam(const std::string &key, double def)
{
  std::string freq_str = sprockit::sprintf("%eHz", def);
  std::string param_value_str = getOptionalParam(key, freq_str);
  return get_freq_from_str(param_value_str.c_str(), key.c_str());
}

long
SimParameters::getByteLengthParam(const std::string &key)
{
  std::string param_value_str = getParam(key);
  return get_byte_length_from_str(param_value_str.c_str(), key.c_str());
}

long
SimParameters::getOptionalByteLengthParam(const std::string& key, long length)
{
  std::string length_str = sprockit::sprintf("%ldB", length);
  std::string param_value_str = getOptionalParam(key, length_str);
  return get_byte_length_from_str(param_value_str.c_str(), key.c_str());
}

double
SimParameters::getBandwidthParam(const std::string &key)
{
  std::string param_value_str = getParam(key);
  return get_bandwidth_from_str(param_value_str.c_str(), key.c_str());
}

double
SimParameters::getOptionalBandwidthParam(const std::string &key, const std::string& def)
{
  bool errorflag = false;
  std::string param_value_str = getOptionalParam(key, def);
  double val = getBandwidth(param_value_str.c_str(), errorflag);
  if (errorflag) {
    spkt_abort_printf("improperly formatted bandwidth (%s) for parameter %s",
                     param_value_str.c_str(), key.c_str());
  }
  return val;
}

double
SimParameters::getOptionalBandwidthParam(const std::string &key, double def)
{
  std::string bwstr = sprockit::sprintf("%ebytes/sec", def);
  return getOptionalBandwidthParam(key, bwstr);
}

void
SimParameters::tryToParse(
  const std::string& fname,
  bool fail_on_existing,
  bool override_existing)
{
  parseFile(fname, fail_on_existing, override_existing);
#if 0
  std::string inc_file = fname;
  std::string dir = "";
  std::string f_firstchar = inc_file.substr(0, 1);
  if (f_firstchar == "/") {
    //do nothing - this is an absolute path
  } else {
    size_t pos = fname.find_last_of('/');
    if (pos != std::string::npos) {
      dir = fname.substr(0, pos + 1);
    }
  }
  inc_file = trim_str(inc_file);
  try {
    parse_file(dir + inc_file, fail_on_existing, override_existing);
  } catch (io_error& e) {
    parse_file(inc_file, fail_on_existing, override_existing);
  }
#endif
}

SimParameters*
SimParameters::getScopeAndKey(const std::string& key, std::string& final_key)
{
  SimParameters* scope = this;
  final_key = key;
  size_t ns_pos = final_key.find(".");
  while (ns_pos != std::string::npos) {
    std::string ns = final_key.substr(0, ns_pos);
    final_key  = final_key.substr(ns_pos+1);
    scope = scope->getOptionalNamespace(ns).get();
    ns_pos = final_key.find(".");
  }
  return scope;
}

void
SimParameters::parseKeyval(
  const std::string& key,
  const std::string& value,
  bool fail_on_existing,
  bool override_existing,
  bool mark_as_read)
{
  std::string final_key;
  SimParameters* scope = getScopeAndKey(key, final_key);
  scope->doAddParam(final_key, value,
    fail_on_existing, override_existing, mark_as_read);
}

void
SimParameters::insertInto(SST::Params &params)
{
  insertInto("", params);
}

void
SimParameters::insertInto(const std::string& ns, SST::Params &params)
{
  std::string prefix;
  if (ns.empty() && namespace_.empty()){
    for (auto& pair : params_){
      params.insert(pair.first, pair.second.value);
    }
  } else {
    if (ns.empty()){
      prefix = namespace_ + ".";
    } else if (namespace_.empty()){
      prefix = ns;
    } else {
      prefix = ns + namespace_ + ".";
    }
    for (auto& pair : params_){
      params.insert(prefix + pair.first, pair.second.value);
    }
  }
  for (auto& pair : subspaces_){
    pair.second->insertInto(prefix, params);
  }
}

std::pair<std::string, std::string>
SimParameters::split_line(const std::string& line)
{
  std::pair<std::string, std::string> p;
  std::string lhs = line.substr(0, line.find("="));
  std::string rhs = line.substr(line.find("=") + 1);
  p.first = sprockit::trim_str(lhs);
  p.second = sprockit::trim_str(rhs);
  return p;
}

void
SimParameters::parseLine(const std::string& line,
  bool fail_on_existing, bool override_existing)
{
  auto keyval = SimParameters::split_line(line);
  parseKeyval(keyval.first, keyval.second,
   fail_on_existing,
   override_existing,
   false/*do not mark as read*/);
}

void
ParamBcaster::bcastString(std::string& str, int me, int root)
{
  if (me == root){
    int size = str.size();
    bcast(&size, sizeof(int), me, root);
    char* buf = const_cast<char*>(str.c_str());
    bcast(buf, size, me, root);
  } else {
    int size;
    bcast(&size, sizeof(int), me, root);
    str.resize(size);
    char* buf = const_cast<char*>(str.c_str());
    bcast(buf, size, me, root);
  }
}

void
SimParameters::parallelBuildParams(sprockit::SimParameters::ptr& params,
                                      int me, int nproc,
                                      const std::string& filename,
                                      ParamBcaster *bcaster,
                                      bool fail_if_not_found)
{
  bool fail_on_existing = false;
  bool overwrite_existing = true;
  try {
    int root = 0;
    if (me == 0){
      //I don't want all processes hitting the network and reading the file
      //Proc 0 reads it and then broadcasts
      params->parseFile(filename, fail_on_existing, overwrite_existing, fail_if_not_found);
      if (nproc > 1){
        //this is a bit more complicated than bcast_file_stream
        //in parsing the main file, root might open more files
        //thus read in all possible params chasing down all include files
        //then build the full text of all params
        std::stringstream sstr;
        params->reproduceParams(sstr);
        std::string all_text = sstr.str();
        bcaster->bcastString(all_text, me, root);
      }
    } else {
      std::string param_text;
      bcaster->bcastString(param_text, me, root);
      std::stringstream sstr(param_text);
      params->parseStream(sstr, fail_on_existing, overwrite_existing);
    }
  } catch (const std::exception &e) {
    std::cout.flush();
    std::cerr.flush();
    std::cerr << "Caught exception while setting up simulation:\n"
              << e.what() << "\n";
    throw;
  }
}

void
SimParameters::parseStream(std::istream& in,
  bool fail_on_existing, bool override_existing)
{
  std::list<SimParameters*> ns_queue;
  ns_queue.push_back(this);
  std::string line;
  while (in.good()) {
    std::getline(in, line);
    line = trim_str(line);
    SimParameters* active_scope = ns_queue.back();
    int last_idx = line.size() - 1;

    if (line.size() == 0) {
          //empty
    } else if (line[0] == '#') {
      //a comment
      continue;
    } else if (line[0] == '}'){
      //ending a namespace
      ns_queue.pop_back();
    } else if (line[last_idx] == '{'){ //opening a new namespace
      std::string ns = line.substr(0, last_idx);
      ns = trim_str(ns);
      SimParameters* scope = active_scope->getOptionalLocalNamespace(ns).get();
      ns_queue.push_back(scope);
    } else if (line.find("set var ") != std::string::npos) {
      line = line.substr(8);
      auto keyval = SimParameters::split_line(line);
      variables_[keyval.first] = keyval.second;
    } else if (line.find("=") != std::string::npos) {
      //an assignment
      active_scope->parseLine(line, fail_on_existing, override_existing);
    } else if (line.find("include") != std::string::npos) {
      //an include line
      std::string included_file = trim_str(line.substr(7));
      active_scope->tryToParse(included_file, fail_on_existing, override_existing);
    } else if (line.find("unset") != std::string::npos) {
      std::string key;
      SimParameters* scope = active_scope->getScopeAndKey(trim_str(line.substr(5)), key);
      scope->removeParam(key);
    } else {
      spkt_abort_printf("invalid input file line of size %d:\n%s---", line.size(), line.c_str());
    }
  }
}

void
SimParameters::parseFile(
  const std::string& input_fname,
  bool fail_on_existing,
  bool override_existing,
  bool fail_if_not_found)
{
  std::string fname = trim_str(input_fname);

  std::ifstream in;
  SpktFileIO::openFile(in, fname);

  if (in.is_open()) {
    parseStream(in, fail_on_existing, override_existing);
  } else if (fail_if_not_found){
    SpktFileIO::notFound(fname);
  }
}

void
SimParameters::removeParam(const std::string & key)
{
  params_.erase(key);
}

void
SimParameters::throwKeyError(const std::string& key) const
{
  printScopedParams(std::cerr);
  spkt_abort_printf(
           "sim_parameters: could not find parameter %s in namespace %s",
          key.c_str(), namespace_.c_str());
}

bool
SimParameters::getScopedParam(std::string& inout,
                          const std::string& key)
{
  auto it = params_.find(key);
  if (it == params_.end()){
    return false;
  }
  parameter_entry& entry = it->second;
  entry.read = true;
  inout = entry.value;
  return true;
}

bool
SimParameters::getParam(std::string& inout, const std::string& key)
{
  return getScopedParam(inout, key);
  /** don't do scoped params
  if (!found && parent_ && parent_->public_scope()){
    //never return anything from the top-level global namespace - no, don't
    return parent_->get_param(inout, key);
  } else {
    return found;
  }
  */
}

std::string
SimParameters::getLowercaseParam(const std::string& key, bool throw_on_error)
{
  std::string param = getParam(key, throw_on_error);
  std::transform(param.begin(), param.end(), param.begin(), ::tolower);
  return param;
}

std::string
SimParameters::getParam(const std::string& key, bool throw_on_error)
{
  debug_printf(dbg::params | dbg::read_params,
    "sim_parameters: getting key %s\n",
    key.c_str());

  std::string match;
  bool found = getParam(match, key);
  if (!found && throw_on_error){
    throwKeyError(key);
  }

  return match;
}

std::string
SimParameters::getScopedParam(const std::string& key, bool throw_on_error)
{
  std::string ret;
  bool found = getScopedParam(ret, key);
  if (!found && throw_on_error){
    throwKeyError(key);
  }
  return ret;
}

bool
SimParameters::hasParam(const std::string& key) const
{
  return params_.find(key) != params_.end();
}

std::string
SimParameters::getVariable(const std::string& str)
{
  auto it = variables_.find(str);
  if (it == variables_.end()){
    if (parent_){
      return parent_->getVariable(str);
    }
    //nope, no parent - crash band burn
    std::cerr << "Existing variables: " << std::endl;
    for (auto& pair : variables_){
      std::cerr << pair.first << " = " << pair.second << std::endl;
    }
    spkt_abort_printf("unknown variable name %s", str.c_str());
    return "";
  } else {
    return it->second;
  }
}

void
SimParameters::doAddParam(
  const std::string& key,
  const std::string& val,
  bool fail_on_existing,
  bool override_existing,
  bool mark_as_read)
{
  if (val.c_str()[0] == '$'){
    std::string varval = getVariable(val.substr(1));
    doAddParam(key, varval,
      fail_on_existing, override_existing, mark_as_read);
    return;
  }

  debug_printf(dbg::params, //| dbg::write_params,
    "sim_parameters: setting key %s to value %s\n",
    key.c_str(), val.c_str());

  //if (this->namespace_ != "env"){ //special reserved namespace
  //  KeywordRegistration::validate_keyword(key,val);
  //}

  key_value_map::iterator it = params_.find(key);

  if (it != params_.end()){
    if (fail_on_existing){
      spkt_abort_printf("sim_parameters::add_param - key already in params: %s", key.c_str());
    } else if (override_existing){
      parameter_entry& entry = it->second;
      entry.value = val;
      entry.read = mark_as_read;
    } else {
      //do nothing - don't override and don't fail
    }
  } else {
    parameter_entry entry;
    entry.value = val;
    entry.read = mark_as_read;
    params_.insert(it, std::make_pair(key, entry));
  }
}

ParamAssign
SimParameters::operator[](const std::string& key)
{
  std::string final_key;
  SimParameters* scope = getScopeAndKey(key, final_key);
  return ParamAssign(scope->params_[final_key].value, key);
}

void
SimParameters::addParam(
  const std::string &key,
  const std::string &val)
{
  parseKeyval(key, val,
   true/*fail on existing*/,
   false/*do not override existing*/,
   false/*do not mark as read*/);
}

void
SimParameters::addParamOverride(
  const std::string& key,
  const std::string& val)
{
  parseKeyval(key, val,
   false/*do not fail on existing*/,
   true/*override existing*/,
   true/*mark as read*/);
}

void
SimParameters::combineInto(SimParameters::ptr sp,
                             bool fail_on_existing,
                             bool override_existing,
                             bool mark_as_read)
{
  for (auto& pair : params_){
    const std::string& key = pair.first;
    const parameter_entry& value = pair.second;
    sp->parseKeyval(key, value.value,
                 fail_on_existing, override_existing, mark_as_read);
  }

  for (auto& pair : subspaces_){
    std::string name = pair.first;
    SimParameters::ptr my_subspace = pair.second;
    SimParameters::ptr his_subspace = sp->getOptionalNamespace(name);
    my_subspace->combineInto(his_subspace,
             fail_on_existing, override_existing, mark_as_read);
  }
}

void
SimParameters::printLocalParams(std::ostream& os, const std::string& prefix) const
{
  for (auto& pair : params_) {
    os << prefix << pair.first;
    for (int i=pair.first.size(); i < 25; ++i){
      os << " ";
    }
    os << " = " << pair.second.value << "\n";
  }
}

std::string
SimParameters::print_scopes(std::ostream& os)
{
  std::string prefix = "";
  if (parent_){
    prefix = parent_->print_scopes(os);
  }
  os << prefix << "namespace " << namespace_ << "\n";
  return prefix + "  ";
}

std::string
SimParameters::printScopedParams(std::ostream& os) const
{
  std::string prefix = "";
  if (parent_){
    prefix = parent_->printScopedParams(os);
  }
  std::string new_prefix = prefix + "  ";
  printLocalParams(os, prefix);
  return new_prefix;
}

void
SimParameters::printParams(
  std::ostream &os,
  const std::string& prefix) const
{
  std::string new_prefix = parent_ ? (prefix + " ") : prefix;
  if (!namespace_.empty()){
    os << prefix << namespace_ << " {\n";
    printLocalParams(os, new_prefix);
  } else {
    printLocalParams(os, prefix);
  }

  for (auto& pair : subspaces_){
    pair.second->printParams(os, new_prefix);
  }

  if (!namespace_.empty()){
    os << prefix << "}\n";
  }
}

}

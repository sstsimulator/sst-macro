/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#ifndef SPROCKIT_COMMON_SIM_PARAMETERS_H_INCLUDED
#define SPROCKIT_COMMON_SIM_PARAMETERS_H_INCLUDED

#include <sprockit/spkt_config.h>
#include <sprockit/debug.h>
#include <sprockit/unordered.h>

#include <iostream>
#include <list>
#include <vector>
#include <set>


DeclareDebugSlot(params)
DeclareDebugSlot(read_params)
DeclareDebugSlot(write_params)

namespace sprockit {

class param_assign {
 public:
  param_assign(std::string& p, const std::string& k) :
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

class param_bcaster {
 public:
  virtual void
  bcast(void* buf, int size, int me, int root) = 0;

  void
  bcast_string(std::string& str, int me, int root);
};

class sim_parameters  {

 public:
  struct parameter_entry
  {
    parameter_entry() : read(false) {}
    std::string value;
    bool read;
  };

  void
  reproduce_params(std::ostream& os);

  typedef spkt_unordered_map<std::string, parameter_entry> key_value_map;

  sim_parameters();

  sim_parameters(const key_value_map& p);

  sim_parameters(const std::string& filename);

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
  parallel_build_params(sprockit::sim_parameters* params,
                        int me, int nproc,
                        const std::string& filename,
                        param_bcaster* bcaster);

  virtual ~sim_parameters();

  void
  remove_param(const std::string &key);

  std::string
  get_variable(const std::string& str);

  std::string
  get_param(const std::string& key, bool throw_on_error = true);

  std::string
  get_scoped_param(const std::string& key, bool throw_on_error = true);

  sim_parameters*
  get_optional_local_namespace(const std::string& ns);

  std::string
  reread_param(const std::string& key) {
    return get_param(key);
  }

  std::string
  reread_optional_param(const std::string& key, const std::string& def) {
    return get_optional_param(key, def);
  }

  /// Return the value of the keyword if it exists. Otherwise return
  /// a default value.
  /// @param key gives the keyword
  /// @param def gives the default value (used if has_param(key) is false)
  /// @return the value if it exists, otherwise the default
  std::string
  get_optional_param(const std::string &key, const std::string &def);

  std::string
  deprecated_optional_param(const std::string &key, const std::string &def);

  std::string
  deprecated_param(const std::string& key);

  void
  add_param(const std::string& key, const std::string& val);

  void
  copy_param(const std::string& oldname, const std::string& newname);

  void
  copy_optional_param(const std::string& oldname, const std::string& newname);

  void
  add_param_override(const std::string& key, const std::string& val);

  void
  add_param_override(const std::string &key, double val);

  void
  add_param_override(const std::string& key, double val, const char* units);

  void
  add_param_override(const std::string& key, int val);

  void
  combine_into(sim_parameters* sp,
               bool fail_on_existing = false,
               bool override_existing = true,
               bool mark_as_read = true);

  std::string
  print_scoped_params(std::ostream& os) const;

  std::string
  print_scopes(std::ostream& os);

  void
  print_local_params(std::ostream& os, const std::string& prefix) const;

  void
  print_params(std::ostream& os = std::cerr, const std::string& prefix = "") const;

  bool
  print_unread_params(std::ostream& os = std::cerr) const;

  bool
  has_param(const std::string& key) const;

  bool
  has_scoped_param(const std::string& key) const;

  int
  get_int_param(const std::string& key);

  int
  deprecated_int_param(const std::string& key);

  int
  deprecated_optional_int_param(const std::string &key, int def);

  int
  reread_int_param(const std::string& key);

  int
  reread_optional_int_param(const std::string& key, int def);

  /// Return the value of the keyword if it exists. Otherwise return
  /// a default value.
  /// @param key gives the keyword
  /// @param def gives the default value (used if has_param(key) is false)
  /// @return the value if it exists, otherwise the default
  int
  get_optional_int_param(const std::string &key, int def);

  /// Returns the value of the key as a boolean.
  bool
  get_bool_param(const std::string &key);

  bool
  reread_bool_param(const std::string& key);

  bool
  deprecated_bool_param(const std::string& key);

  bool
  deprecated_optional_bool_param(const std::string& key, bool def);

  bool
  reread_optional_bool_param(const std::string& key, bool def);

  /// Return the value of the keyword if it exists. Otherwise return
  /// a default value.
  /// @param key gives the keyword
  /// @param def gives the default value (used if has_param(key) is false)
  /// @return the value if it exists, otherwise the default
  bool
  get_optional_bool_param(const std::string &key, int def);

  double
  get_bandwidth_param(const std::string& key);

  /**
   @param key The parameter name
   @return A value with units. This loops through bandwidth, frequency, time, etc
           to return any value that can have units. Everything is normalized to baseslines
           of B/s, s, Hz, etc
  */
  double
  get_quantity(const std::string& key);

  double
  get_optional_quantity(const std::string& key, double def);

  double
  deprecated_bandwidth_param(const std::string& key);

  double
  reread_bandwidth_param(const std::string& key);

  double
  reread_optional_bandwidth_param(const std::string& key, double def);

  double
  deprecated_optional_bandwidth_param(const std::string& key, double def);

  double
  get_optional_bandwidth_param(const std::string &key, double def);

  double
  get_optional_bandwidth_param(
    const std::string& key,
    const std::string& def);

  long
  get_byte_length_param(const std::string& key);

  long
  get_optional_byte_length_param(const std::string& key, long def);

  long
  deprecated_byte_length_param(const std::string& key);

  long
  deprecated_optional_byte_length_param(const std::string& key, long def);

  double
  get_optional_freq_param(const std::string& key, double def);

  double
  get_freq_param(const std::string& key);

  double
  deprecated_freq_param(const std::string& key);

  double
  deprecated_optional_freq_param(const std::string& key, double def);

  /// Return the value of the keyword if it exists. Otherwise return
  /// a default value.
  /// @param key gives the keyword
  /// @param def gives the default value (used if has_param(key) is false)
  /// @return the value if it exists, otherwise the default
  long
  get_optional_long_param(const std::string &key, long def);

  long
  get_long_param(const std::string& key);

  long
  deprecated_long_param(const std::string& key);

  long
  deprecated_optional_long_param(const std::string& key, long def);

  long
  reread_long_param(const std::string& key);

  void
  reread_optional_long_param(const std::string& key);

  double
  get_double_param(const std::string& key);

  /// Return the value of the keyword if it exists. Otherwise return
  /// a default value.
  /// @param key gives the keyword
  /// @param def gives the default value (used if has_param(key) is false)
  /// @return the value if it exists, otherwise the default
  double
  get_optional_double_param(const std::string &key, double def);

  double
  reread_double_param(const std::string& key);

  double
  reread_optional_double_param(const std::string &key, double def);

  double
  deprecated_double_param(const std::string& key);

  double
  deprecated_optional_double_param(const std::string& key, double def);

  double
  get_time_param(const std::string& key);

  double
  get_optional_time_param(const std::string &key, double def);

  double
  deprecated_optional_time_param(const std::string &key, double def);

  double
  deprecated_time_param(const std::string& key);

  void
  get_vector_param(const std::string& key, std::vector<int>& vals);

  void
  get_vector_param(const std::string& key, std::vector<double>& vals);

  void
  get_vector_param(const std::string& key, std::vector<std::string>& vals);

  void
  get_optional_vector_param(const std::string& key, std::vector<std::string>& vals);

  sim_parameters*
  get_namespace(const std::string& ns);

  sim_parameters*
  get_optional_namespace(const std::string& ns);

  void
  set_namespace(const std::string& ns, sim_parameters* params){
    subspaces_[ns] = params;
  }

  bool
  has_namespace(const std::string& ns) const;

  void
  parse_file(const std::string& fname, bool fail_on_existing, bool override_existing);

  void
  parse_stream(std::istream& in, bool fail_on_existing, bool override_existing);

  void
  parse_line(const std::string& line, bool fail_on_existing, bool override_existing);

  /**
    @param key
    @param value
    @param fail_on_existing Fail if the parameter named by key already exists
  */
  void
  parse_keyval(const std::string& key,
    const std::string& value,
    bool fail_on_existing,
    bool override_existing,
    bool mark_as_read);

  /**
   * This is a dirty hack to store non-sprockit parameter objects
   * on this parameter object
   */
  void
  append_extra_data(void* data) {
    extra_data_ = data;
  }

  template <class T>
  T*
  extra_data() const {
    void* ptr = _extra_data();
    return static_cast<T*>(ptr);
  }

  param_assign
  operator[](const std::string& key);

  key_value_map::iterator begin() { return params_.begin(); }
  key_value_map::const_iterator begin() const { return params_.begin(); }

  key_value_map::iterator end() { return params_.end(); }
  key_value_map::const_iterator end() const { return params_.end(); }

  typedef std::map<std::string, sim_parameters*>::iterator namespace_iterator;
  typedef std::map<std::string, sim_parameters*>::const_iterator const_namespace_iterator;
  namespace_iterator ns_begin() { return subspaces_.begin(); }
  const_namespace_iterator ns_begin() const { return subspaces_.begin(); }
  namespace_iterator ns_end() { return subspaces_.end(); }
  const_namespace_iterator ns_end() const { return subspaces_.end(); }

 private:
  std::map<std::string, sim_parameters*> subspaces_;
  std::map<std::string, std::string> variables_;

  sim_parameters* parent_;

  void* _extra_data() const;
  void* extra_data_;

  static sim_parameters* empty_ns_params_;

  std::string namespace_;

  key_value_map params_;

  uint64_t current_id_;

  /**
   * @brief _get_namespace Get a parameter namespace. If the namespace does not exist in the current scope locally,
   *  search through any parent namespaces. This follow C++ scoping rules as if you had requested ns::variable.
   * @param ns The namespace to get
   * @return The set of all parameters in a given param namespace
   */
  sim_parameters* _get_namespace(const std::string &ns);

  sim_parameters*
  build_local_namespace(const std::string& ns);

  void
  throw_key_error(const std::string& key) const;

  void
  set_parent(sim_parameters* p) {
    parent_ = p;
  }

  void
  set_namespace(const std::string& str) {
    namespace_ = str;
  }

  bool
  local_has_namespace(const std::string& ns) const {
    return subspaces_.find(ns) != subspaces_.end();
  }

  void
  split_line(const std::string& line, std::pair<std::string, std::string>& p);

  void
  try_to_parse(const std::string& fname, bool fail_on_existing, bool override_existing);

  void
  print_params(const key_value_map& pmap, std::ostream& os, bool pretty_print, std::list<std::string>& ns) const;

  void
  do_add_param(const std::string& key,
    const std::string& val,
    bool fail_on_existing,
    bool override_existing,
    bool mark_as_read);


  sim_parameters*
  get_scope_and_key(const std::string& key, std::string& final_key);

  bool
  get_param(std::string& inout, const std::string& key);

  bool
  get_scoped_param(std::string& inout, const std::string& key);




};

}




#endif


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

#ifndef SSTMAC_COMMON_LOGGER_H_INCLUDED
#define SSTMAC_COMMON_LOGGER_H_INCLUDED

#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_manager_fwd.h>
#include <stdint.h>
#include <sprockit/unordered.h>

#include <iostream>
#include <deque>
#include <list>

namespace sstmac {



/**
 * Base class of things which track events in the simulation, and output
 * in different ways.
 *
 * Upon construction, loggers are registered in a global variable, and provide
 * a name signature which has the form "<category> name | name2 | ...".  Then
 * loggers can be selectively turned on and off by calling set_user_param().
 * Loggers will be turned on using any of their supplied names, and all
 * loggers of a given category will be turned on by specifying "all" (e.g. "<debug> all")
 *
 * Currently supported categories are <debug>, <stats>, and <trace>
 *
 * Objects which are created and destroyed often should definitely use a static variable,
 * because a lot of std::string operations have to happen to check name signatures to see if
 * the logger is active.
 *
 * Also, because the logger constructed adds this to a static collection, static logger variables
 * should first use the fake constructor as initialization, then check is_real() after the executable
 * has started to see if they should
 * reinstantiate themselves.
 *
 */
class logger  {
 public:

  /**
   * Real constructor.  Adds this to the static logger collection that is updated
   * on set_user_param(), and calls update_active() in case set_user_param has already
   * been called.
   * @param namesig My name signature (e.g. "<debug> mpi | library | mpiserver").  It's usually a good
   * idea to provide multiple names so you don't have to remember the exact name, and also so you can turn on
   * whole groups of objects (like all of mpi, for instance).
   * @param os the output stream to use.
   * @param flush If true, the logger will always call flush() on the output stream after it writes something to it.
   * @param color Set color with escape characters, most useful for debugging in a console.
   */
  logger(const std::string& namesig, std::ostream *os, bool flush = false,
         const char* color = black);

  /**
   * Fake constructor for instantiating static loggers before they're sure that the
   * static logger variable has been instantiated.
   */
  logger() :
    is_real_(false) {
  }

  virtual
  ~logger();

  /**
   * Unfortunately, everyone who wants loggers to get turned on/off has
   * to call this
   * @param log
   */
  static void
  register_logger(logger* log) {
    loggers_.push_back(log);
  }

  static void
  delete_statics();

  /**
   * Indicates if this logger is turned on, which is useful
   * for bypassing std::stringifying everything
   * @return
   */
  inline bool
  is_active(int lev = 1) const {
    return (active_ >= lev);
  }

  /**
   * Static function which should be called to change
   * which loggers are currently turned on.  Calls update_all_active().
   * @param s Signature which indicates which loggers should be turned on.
   * For example "<debug> mpi | <stats> all | <debug> app | <trace> mpi"
   */
  static void
  set_user_param(const std::string& s);

  static bool user_params_checked_;

  static void
  check_user_params();

  /**
   * Clears all saved loggers that are updated on set_user_param.
   * It's usually a good idea to call this in between simulations
   * if running multiple in one executable.
   */
  static void
  clear();

  virtual logger&
  operator<<(int i);

  virtual logger&
  operator<<(long i);

  virtual logger&
  operator<<(long long int i);

  // note: size_t will be one of the unsigned integer types below
  virtual logger&
  operator<<(unsigned int i);

  virtual logger&
  operator<<(unsigned long int i);

  virtual logger&
  operator<<(unsigned long long int i);

  virtual logger&
  operator<<(bool i);

  virtual logger&
  operator<<(const char* i);

  virtual logger&
  operator<<(double i);

  virtual logger&
  operator<<(long double i);

  virtual logger&
  operator<<(const std::string& i);

  virtual logger&
  operator<<(timestamp i);

  virtual logger&
  operator<<(const std::pair<int, int>& p);

  template <class T>
  logger&
  operator<<(const T& t){
    (*outstream_) << t;
    return *this;
  }


  static const char* black;
  static const char* red;
  static const char* green;
  static const char* yellow;
  static const char* blue;
  static const char* magenta;
  static const char* cyan;
  static const char* white;

  /**
   * Sets whether we should turn text coloring on
   * (useful for debugging in a console)
   * @param c
   */
  static void
  set_debug_coloring(bool c) {
    debug_coloring_ = c;
  }

  /**
   * Indicates if this instance was constructed correctly.
   * This is useful for instantiating static logger variables,
   * and only making the real ones until the simulation has already started,
   * when the logger static variables have been set up first (which is required
   * for the whole logger active mechanism to work).
   * @return true if this instance was constructed properly
   */
  inline bool
  is_real() {
    return is_real_;
  }

  virtual std::string to_string() const;

  static event_manager* timer_;

  std::string time() const;

 protected:

  static std::deque<std::string> user_params;
  static spkt_unordered_map<std::string, bool> user_params_matched_;
  static std::list<logger*> loggers_;

  int active_;

  std::string signature_;
  std::string category_;

  std::ostream *outstream_;

  std::string my_color_;

  /**
   * updates the active status of this logger based on the current user parameter
   */
  void
  update_active();

  /**
   * updates all saved loggers based on the current user parameter
   */
  static void
  update_all_active();

  static bool debug_coloring_;

  bool always_flush_;

  bool is_real_;

  static bool timer_on_;

};
} // end of namespace sstmac
#endif


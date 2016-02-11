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

#include <sstream>
#include <cstdarg>
#include <sstmac/common/logger.h>
#include <sstmac/common/timestamp.h>
#include <sstmac/common/event_manager.h>
#include <sstmac/common/sstmac_env.h>
#include <sstmac/common/thread_info.h>
#include <sstmac/common/thread_lock.h>

#include <sprockit/statics.h>
#include <sprockit/util.h>
#include <sprockit/basic_string_tokenizer.h>
#include <sprockit/spkt_string.h>

namespace sstmac {

static sprockit::need_delete_statics<logger> del_statics;
static thread_lock update_lock_;
static thread_lock check_lock_;

std::deque<std::string> logger::user_params;
spkt_unordered_map<std::string, bool> logger::user_params_matched_;
std::list<logger*> logger::loggers_;
bool logger::timer_on_ = false;
event_manager* logger::timer_;
bool logger::user_params_checked_ = false;


const char* logger::black = "\033[0;30m";
const char* logger::red = "\033[0;31m";
const char* logger::green = "\033[0;32m";
const char* logger::yellow = "\033[0;33m";
const char* logger::blue = "\033[0;34m";
const char* logger::magenta = "\033[0;35m";
const char* logger::cyan = "\033[0;36m";
const char* logger::white = "\033[0;37m";

bool logger::debug_coloring_ = false;

logger::logger(const std::string& namesig, std::ostream *os, bool flush,
               const char* color) :
  active_(0), outstream_(os), my_color_(color), always_flush_(flush),
  is_real_(true)
{
  size_t left = namesig.find("<");
  size_t right = namesig.find(">");
  if (left != std::string::npos && right != std::string::npos) {
    category_ = namesig.substr(left + 1, right - left - 1);
    signature_ = namesig.substr(right + 1);
  }
  else {
    category_ = "";
    signature_ = namesig;
  }
  update_active();
}

void
logger::update_active()
{
  /** I am already active */
  if (active_) {
    return;
  }

  update_lock_.lock();
  std::deque<std::string> tok;

  std::string space = "|";
  pst::BasicStringTokenizer::tokenize(signature_, tok, space);

  std::deque<std::string>::iterator it, end = tok.end();
  std::deque<std::string>::iterator paramit, paramend = user_params.end();

  for (it = tok.begin(); it != end; it++) {
    std::string temp = *it;

    temp = sprockit::trim_str(temp);

    for (paramit = user_params.begin(); paramit != paramend; paramit++) {
      std::string t2 = sprockit::trim_str(*paramit);

      if (t2 == "timestamps") {
        timer_on_ = true;
      }
      else {

        size_t left = t2.find("<");
        size_t right = t2.find(">");
        std::string user_cat = "";
        std::string user_name = t2;
        if (left != std::string::npos && right != std::string::npos) {
          user_cat = t2.substr(left + 1, right - left - 1);
          user_name = t2.substr(right + 1);
        }

        user_name = sprockit::trim_str(user_name);
        int level = 1;
        if (user_name.find("(") != std::string::npos) {
          // setting a level also
          std::string levstr = user_name.substr(user_name.find("(") + 1,
                                                user_name.find(")"));
          level = atoi(levstr.c_str());
          user_name = user_name.substr(0, user_name.find("("));
        }

        if (user_cat == "" || user_cat == category_) {
          if (user_name == temp || user_name == "all") {
            active_ = level;
            user_params_matched_[t2] = true;
            update_lock_.unlock();
            return;
          }
        }
      }
    }
  }
  update_lock_.unlock();
}

std::string
logger::to_string() const
{
  return "logger";
}

void
logger::update_all_active()
{
  std::list<logger*>::iterator it, end = loggers_.end();
  for (it = loggers_.begin(); it != end; it++) {
    (*it)->update_active();
  }
}

logger::~logger()
{
}

void
logger::set_user_param(const std::string& s)
{
  std::string tosep = s;

  timer_on_ = false;

  std::deque<std::string> tok;

  std::string space = "|";
  pst::BasicStringTokenizer::tokenize(tosep, tok, space);
  user_params = tok;

  std::deque<std::string>::iterator paramit, paramend = user_params.end();

  for (paramit = user_params.begin(); paramit != paramend; paramit++) {
    std::string t2 = sprockit::trim_str(*paramit);
    user_params_matched_[t2] = false;
  }
  user_params_matched_["<debug> mpicheck"] = true; //hard code this guy

  update_all_active();

}

void
logger::check_user_params()
{
  if (!user_params_checked_) {
    //check_lock_.lock();
    std::deque<std::string>::iterator paramit, paramend = user_params.end();
    for (paramit = user_params.begin(); paramit != paramend; paramit++) {
      std::string t2 = sprockit::trim_str(*paramit);
      if (!user_params_matched_[t2]) {
        //cerr0 << "***WARNING: as of application startup, user debug flag " << t2 <<
        //          " does not match anything, so check if it's right.\n";
        //cerr0 << "\tThis could be caused by objects that get created after application startup, like dumpi tracer\n";
      }
    }
    user_params_checked_ = true;
    //check_lock_.unlock();
  }
}

void
logger::clear()
{
  loggers_.clear();
}

std::string
logger::time() const
{
#if SSTMAC_INTEGRATED_SST_CORE
  return "";
#else
  if (timer_on_ && timer_) {
    event_manager* evman = safe_cast(event_manager, timer_);
    timestamp t = evman->now();
    return sprockit::printf("[t=%12.4e] ", t.sec());
  }
  else {
    return "";
  }
#endif
}

logger&
logger::operator<<(int i)
{

  if (debug_coloring_) {
    (*outstream_) << my_color_ << i << black;
  }
  else {
    (*outstream_) << i;
  }

  if (always_flush_) {
    outstream_->flush();
  }

  return (*this);
}

logger&
logger::operator<<(long i)
{
  if (debug_coloring_) {
    (*outstream_) << my_color_ << i << black;
  }
  else {
    (*outstream_) << i;
  }

  if (always_flush_) {
    outstream_->flush();
  }

  return (*this);
}

logger&
logger::operator<<(unsigned int i)
{
  if (debug_coloring_) {
    (*outstream_) << my_color_ << i << black;
  }
  else {
    (*outstream_) << i;
  }

  if (always_flush_) {
    outstream_->flush();
  }

  return (*this);
}

logger&
logger::operator<<(long long int i)
{
  if (debug_coloring_) {
    (*outstream_) << my_color_ << i << black;
  }
  else {
    (*outstream_) << i;
  }

  if (always_flush_) {
    outstream_->flush();
  }

  return (*this);
}

logger&
logger::operator<<(unsigned long int i)
{
  if (debug_coloring_) {
    (*outstream_) << my_color_ << i << black;
  }
  else {
    (*outstream_) << i;
  }

  if (always_flush_) {
    outstream_->flush();
  }

  return (*this);
}

logger&
logger::operator<<(unsigned long long int i)
{
  if (debug_coloring_) {
    (*outstream_) << my_color_ << i << black;
  }
  else {
    (*outstream_) << i;
  }

  if (always_flush_) {
    outstream_->flush();
  }

  return (*this);
}

logger&
logger::operator<<(bool i)
{
  if (debug_coloring_) {
    (*outstream_) << my_color_ << i << black;
  }
  else {
    (*outstream_) << i;
  }

  if (always_flush_) {
    outstream_->flush();
  }

  return (*this);
}

logger&
logger::operator<<(const char* i)
{
  if (debug_coloring_) {
    (*outstream_) << my_color_ << i << black;
  }
  else {
    (*outstream_) << i;
  }

  if (always_flush_) {
    outstream_->flush();
  }

  return (*this);
}

logger&
logger::operator<<(double i)
{
  if (debug_coloring_) {
    (*outstream_) << my_color_ << i << black;
  }
  else {
    (*outstream_) << i;
  }

  if (always_flush_) {
    outstream_->flush();
  }

  return (*this);
}

logger&
logger::operator<<(long double i)
{
  if (debug_coloring_) {
    (*outstream_) << my_color_ << i << black;
  }
  else {
    (*outstream_) << i;
  }

  if (always_flush_) {
    outstream_->flush();
  }

  return (*this);
}

logger&
logger::operator<<(const std::string& i)
{
  if (debug_coloring_) {
    (*outstream_) << my_color_ << i << black;
  }
  else {
    (*outstream_) << i;
  }

  if (always_flush_) {
    outstream_->flush();
  }

  return (*this);
}

logger&
logger::operator<<(const std::pair<int, int>& p)
{
  if (debug_coloring_)
    (*outstream_) << my_color_ << "(" << p.first << ", " << p.second << ")"
                  << black;
  else {
    (*outstream_) << "(" << p.first << ", " << p.second << ")";
  }

  if (always_flush_) {
    outstream_->flush();
  }

  return (*this);
}

logger&
logger::operator<<(timestamp i)
{
  if (debug_coloring_) {
    (*outstream_) << my_color_ << i << black;
  }
  else {
    (*outstream_) << i;
  }

  if (always_flush_) {
    outstream_->flush();
  }

  return (*this);
}

void
logger::delete_statics()
{
}

} // end of namespace sstmac


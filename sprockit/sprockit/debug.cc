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

#include <sprockit/spkt_string.h>
#include <sprockit/errors.h>
#include <sprockit/debug.h>
#include <sprockit/basic_string_tokenizer.h>
#include <sprockit/statics.h>
#include <sprockit/delete.h>
#include <iostream>

DeclareDebugSlot(timestamp);
RegisterDebugSlot(timestamp, "turns on timestamps on all debug statements");

namespace sprockit {

static need_delete_statics<debug> del_statics;


debug_prefix_fxn* debug::prefix_fxn = 0;
debug_int debug::current_bitmask_;
debug_int debug::start_bitmask_;
std::map<std::string, debug_int*>* debug::debug_ints_ = 0;
std::map<std::string, std::string>* debug::docstrings_ = 0;
int debug::num_bits_assigned = 1; //the zeroth bit is reserved empty

#if SPROCKIT_ENABLE_DEBUG
debug_indent::debug_indent() : level(0)
{
  indents[0] = "";
  indents[1] = "  ";
  indents[2] = "    ";
  indents[3] = "      ";
  indents[4] = "        ";
  indents[5] = "          ";
}
#endif

void
debug::delete_statics()
{
  free_static_ptr(debug_ints_);
  free_static_ptr(docstrings_);
  if (prefix_fxn) delete prefix_fxn;
}

void
debug::turn_off(){
  current_bitmask_ = debug_int(); //clear it
}

void
debug::print_debug_string(const std::string &str, std::ostream& os)
{
  if (prefix_fxn)
    os << prefix_fxn->str();
  os << str << std::endl;
}

std::string
debug_int::to_string() const {
  std::stringstream sstr;
  sstr << fields  << " ";
  std::stringstream actives;
  for (int i=0; i < 4; ++i){
    int on = fields & (1ull<<i);
    if (on){
      sstr << "1";
      //actives << " " << debug::slot_name(debug::slot(i));
    }
    else {
      sstr << "0";
    }
  }
  std::string ret = sstr.str() + actives.str();
  return ret;
}

void
debug::turn_on(){
  current_bitmask_ = start_bitmask_;
}

void
debug::turn_off(debug_int& dint){
  if (dint.fields == 0){
    //was never turned on
    return;
  }
  debug_int offer = ~dint;
  start_bitmask_ = start_bitmask_ & offer;
  current_bitmask_ = current_bitmask_ & offer;
}

void
debug::turn_on(debug_int& dint){
  if (dint.fields == 0){
    assign_slot(dint);
  }
  start_bitmask_ = start_bitmask_ | dint;
  current_bitmask_ = current_bitmask_ | dint;
}

void
debug::assign_slot(debug_int& dint)
{
  //has not been assigned a bitfield
  if (num_bits_assigned > MAX_DEBUG_SLOT){
    spkt_throw_printf(illformed_error,
      "Too many debug slots turned on! Max is %d", MAX_DEBUG_SLOT);
  }
  int slot = num_bits_assigned++;
  dint.init(slot);
}

void
debug::turn_on(const std::string& str){
  std::map<std::string, debug_int*>::iterator it = debug_ints_->find(str);
  if (it == debug_ints_->end()){
    spkt_throw_printf(input_error,
        "debug::turn_on: unknown debug flag %s",
        str.c_str());
  }
  debug_int& dint = *(it->second);
  turn_on(dint);
}

void
debug::register_debug_slot(const std::string& str,
                           debug_int* dint_ptr,
                           const std::string& docstring){
  if (!debug_ints_){
    debug_ints_ = new std::map<std::string, debug_int*>;
    docstrings_ = new std::map<std::string, std::string>;
  }
  (*debug_ints_)[str] = dint_ptr;
  (*docstrings_)[str] = docstring;
}

static void
normalize_string(const std::string& thestr,
    const std::string& indent,
    std::ostream& os, int max_length)
{
  std::deque<std::string> tok;
  std::string space = " ";
  pst::BasicStringTokenizer::tokenize(thestr, tok, space);
  os << indent;
  int line_length = 0;
  for (auto& next : tok){
    if (line_length == 0){
        os << next;
        line_length += next.size();
    }
    else {
      line_length += next.size() + 1;
      if (line_length > max_length){
        os << "\n" << indent; // go to next line
        line_length = next.size();
      }
      else {
        os << " ";
      }
      os << next;
    }
  }
}

void
debug::print_all_debug_slots(std::ostream& os)
{
  std::string indent = printf("%22s", "");
  os << "Valid debug flags are:\n";
  std::map<std::string, std::string>::iterator it, end = docstrings_->end();
  for (it = docstrings_->begin(); it != end; ++it){
    const std::string& flag = it->first;
    const std::string& docstring = it->second;
    os << sprockit::printf("  %20s\n", flag.c_str());
    normalize_string(docstring, indent, os, 80);
    os << "\n";
  }
}



bool
debug::slot_active(const debug_int& allowed){
  debug_int bitmask = current_bitmask_ & allowed;
  return bool(bitmask);
}

debug_prefix_fxn::~debug_prefix_fxn()
{
}


}


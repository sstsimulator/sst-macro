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

#include <sprockit/spkt_config.h>
#include <stdio.h>
#include <string>
#include <iostream>
#include <sstream>
#include <ostream>
#include <fstream>

#include <sprockit/output.h>
#include <sprockit/regexp.h>
#include <sprockit/spkt_string.h>

#include <sprockit/spkt_config.h>
#if SPKT_HAVE_CPP11
#  include <regex>

std::string 
parseCode(std::regex_constants::error_type etype) {
  switch (etype) {
    case std::regex_constants::error_collate:
    return "error_collate: invalid collating element request";
    case std::regex_constants::error_ctype:
    return "error_ctype: invalid character class";
    case std::regex_constants::error_escape:
    return "error_escape: invalid escape character or trailing escape";
    case std::regex_constants::error_backref:
    return "error_backref: invalid back reference";
    case std::regex_constants::error_brack:
    return "error_brack: mismatched bracket([ or ])";
    case std::regex_constants::error_paren:
    return "error_paren: mismatched parentheses(( or ))";
    case std::regex_constants::error_brace:
    return "error_brace: mismatched brace({ or })";
    case std::regex_constants::error_badbrace:
    return "error_badbrace: invalid range inside a { }";
    case std::regex_constants::error_range:
    return "erro_range: invalid character range(e.g., [z-a])";
    case std::regex_constants::error_space:
    return "error_space: insufficient memory to handle this regular expression";
    case std::regex_constants::error_badrepeat:
    return "error_badrepeat: a repetition character (*, ?, +, or {) was not preceded by a valid regular expression";
    case std::regex_constants::error_complexity:
    return "error_complexity: the requested match is too complex";
    case std::regex_constants::error_stack:
    return "error_stack: insufficient memory to evaluate a match";
    default:
    return "";
  }
}

#define START_TRY_BLOCK try {
#define END_TRY_BLOCK } catch (std::regex_error& e) { std::cerr << e.what() << " ; code: " << parseCode(e.code()) << std::endl; throw; }

namespace regex_provider = std;
namespace regex_flags = std::regex_constants;
namespace std { namespace regex_constants {
  constexpr syntax_option_type normal = regex_flags::ECMAScript;
} }
typedef std::regex_constants::syntax_option_type regex_flag_type;


#elif defined(SPKT_HAVE_BOOST)
#  include <boost/regex.hpp>
#  include <boost/algorithm/string.hpp>

#define START_TRY_BLOCK 
#define END_TRY_BLOCK 

namespace regex_flags = boost::regex_constants;
namespace regex_provider = boost;
typedef boost::regbase::flag_type regex_flag_type;

#else
#  error "Either C++11 or Boost is required to build sprockit/regexp.cc"
#endif

bool 
has_regexp_match(const std::string &regexp, const std::string &text, int flags)
{
  START_TRY_BLOCK
  regex_provider::smatch matches;
  
  regex_flags::match_flag_type searchFlags = regex_flags::match_default;
  // The default behavior is multiline, so we'd better fix that
#if !SPKT_HAVE_CPP11
  if(! flags & IncludeLineBreaks) searchFlags |= regex_flags::match_single_line;
#else
  if(! flags & IncludeLineBreaks) {
    cerr0 << "WARNING: IncludeLineBreaks flag ignored in C++11" << std::endl;
  }
#endif
  // Make the search case-insensitive, if needed
  regex_flag_type  reFlags = regex_flags::normal;
  if((flags & LowerCase) || (flags & UpperCase)) reFlags |= regex_flags::icase;
  regex_provider::regex re(regexp, reFlags);
  return regex_provider::regex_search(text, matches, re, searchFlags);
  END_TRY_BLOCK
}

bool
get_regexp_integer(const std::string &text, int& ret)
{
  START_TRY_BLOCK
  static const char* regexp = "(\\d+)";
  regex_flags::match_flag_type searchFlags = regex_flags::match_default;
  regex_provider::smatch matches;
  regex_provider::regex re(regexp);
  if(regex_provider::regex_search(text, matches, re, searchFlags)){
      std::istringstream iss(matches[1]);
      if((iss >> ret).fail()){
          cerr0 << "Unable to convert '" << matches[1]
                    << "' to an int in get_regexp_integer" << std::endl;
          abort();
      }
      return true;
  }else{
    ret = -1;
    return false;
  }
  END_TRY_BLOCK
}

double 
get_regexp_double(const std::string &regexp, const std::string &text, int flags)
{
  START_TRY_BLOCK
  regex_flags::match_flag_type searchFlags = regex_flags::match_default;
  // The default behavior is multiline, so we'd better fix that
#if !SPKT_HAVE_CPP11
  if(! flags & IncludeLineBreaks) searchFlags |= regex_flags::match_single_line;
#else
  if(! flags & IncludeLineBreaks) {
    cerr0 << "WARNING: IncludeLineBreaks flag ignored in C++11" << std::endl;
  }
#endif
  double val = 0.0;
  regex_provider::smatch matches;
  regex_provider::regex re(regexp);
  if(regex_provider::regex_search(text, matches, re, searchFlags)){
      std::istringstream iss(matches[1]);
      if((iss >> val).fail()){
          cerr0 << "Unable to convert " << matches[1]
                    << " to a double in get_regexp_double" << std::endl;
          abort();
      }
  }else{
      cerr0 << text << " does not look like a double in get_regexp_double\n";
      abort();
  }
  return val;
  END_TRY_BLOCK
}


std::string
get_regexp_string(const std::string &regexp, const std::string &text, int flags)
{
  START_TRY_BLOCK
  regex_flags::match_flag_type searchFlags = regex_flags::match_default;
  // The default behavior is multiline, so we'd better fix that
#if !SPKT_HAVE_CPP11
  if(! flags & IncludeLineBreaks) searchFlags |= regex_flags::match_single_line;
#else
  if(! flags & IncludeLineBreaks) {
    cerr0 << "WARNING: IncludeLineBreaks flag ignored in C++11" << std::endl;
  }
#endif
  regex_flag_type  reFlags = regex_flags::normal;
  if((flags & LowerCase)  || (flags & UpperCase)) reFlags |= regex_flags::icase;
  regex_provider::regex re(regexp, reFlags);
  std::string str;
  regex_provider::smatch matches;
  if(regex_provider::regex_search(text, matches, re, searchFlags)){
      str = matches[1];
  }else{
      cerr0 << "Match failed in get_regexp_string" << std::endl;
  }
  return str;
  END_TRY_BLOCK
}


double* 
get_regexp_double_array(const std::string &regexp, const std::string &text, size_t& length, int flags)
{
  START_TRY_BLOCK
  std::istringstream iss;
  double val;
  if(flags) cerr0 << "WARNING: flags are ignored in get_regexp_double_array" << std::endl;
  regex_provider::smatch matches;
  regex_flags::match_flag_type searchFlags = regex_flags::match_default;
  // The default behavior is multiline, so we'd better fix that
#if !SPKT_HAVE_CPP11
  if(! flags & IncludeLineBreaks) searchFlags |= regex_flags::match_single_line;
#else
  if(! flags & IncludeLineBreaks) {
    cerr0 << "WARNING: IncludeLineBreaks flag ignored in C++11" << std::endl;
  }
#endif
  regex_provider::regex re(regexp);
  std::string::const_iterator start = text.begin();
  std::string::const_iterator end   = text.end();
  std::vector<double> tempVals;
  while(regex_provider::regex_search(start, end, matches, re, searchFlags)){
      iss.clear();
      size_t nMatches = matches.size();
      for(size_t m = 1; m < nMatches; ++m){
          iss.clear();
          iss.str(matches[m]);
          if((iss >> val).fail()){
              cerr0 << "Unable to convert " << iss.str()
                        << " to a double in get_regexp_double_array" << std::endl;
              abort();
          }
          tempVals.push_back(val);
      }
      // update search position: 
      start = matches[0].second; 
      searchFlags |= regex_flags::match_prev_avail;
#if !SPKT_HAVE_CPP11
      // Not in C++11, but shouldn't cause a problem (?)
      searchFlags |= regex_flags::match_not_bob;
#endif
  } 
  length = tempVals.size();
  double* vals = new double[length];
  for(size_t i = 0; i < length;  ++i){
      vals[i] = tempVals[i];
  }
  return vals;
  END_TRY_BLOCK
}


void
findmatch(std::vector<std::string>& matches, const std::string &regexp, const std::string &text, int flags)
{
  START_TRY_BLOCK
  regex_provider::smatch reMatches;
  regex_flags::match_flag_type searchFlags = regex_flags::match_default;
  // The default behavior is multiline, so we'd better fix that
#if !SPKT_HAVE_CPP11
  if(! flags & IncludeLineBreaks) searchFlags |= regex_flags::match_single_line;
#else
  if(! flags & IncludeLineBreaks) {
    cerr0 << "WARNING: IncludeLineBreaks flag ignored in C++11" << std::endl;
  }
#endif
  regex_flag_type  reFlags = regex_flags::normal;
  if((flags & LowerCase)  || (flags & UpperCase)) reFlags |= regex_flags::icase;
  regex_provider::regex re(regexp, reFlags);
  std::string::const_iterator start = text.begin();
  std::string::const_iterator end   = text.end();
  std::vector<double> tempVals;
  while(regex_provider::regex_search(start, end, reMatches, re, searchFlags)){
      size_t nMatches = reMatches.size();
      for(size_t m = 1; m < nMatches; ++m){
           matches.push_back(reMatches[m]);
           if(flags & UpperCase) sprockit::to_upper(matches.back());
           if(flags & LowerCase) sprockit::to_lower(matches.back());
           if(flags & StripWhitespace) sprockit::trim(matches.back());
      }
      // update search position: 
      start = reMatches[0].second; 
      searchFlags |= regex_flags::match_prev_avail;
#if !SPKT_HAVE_CPP11
      // Not in C++11, but shouldn't cause a problem (?)
      searchFlags |= regex_flags::match_not_bob;
#endif
  } 
  return;
  END_TRY_BLOCK
}
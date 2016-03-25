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

// A bare-bones implementation of a -*-C++-*- std::string tokenizer.
// Based on a verison I found on the web some time ago - no licence attached.

#ifndef SPROCKIT_COMMON_BASICSTRINGTOKENIZER_H_INCLUDED
#define SPROCKIT_COMMON_BASICSTRINGTOKENIZER_H_INCLUDED

#include <string>
#include <deque>

namespace sprockit {
inline std::string trim_str(const std::string& Src, const std::string& c =
                              " \r\n")
{
  size_t p2 = Src.find_last_not_of(c);
  if (p2 == std::string::npos) {
    return std::string();
  }
  size_t p1 = Src.find_first_not_of(c);
  if (p1 == std::string::npos) {
    p1 = 0;
  }
  return Src.substr(p1, (p2 - p1) + 1);
}
}
namespace pst {

/**
 * This nested namespace contains basic functions for std::string parsing.
 */
namespace BasicStringTokenizer {

/**
 * A utility function to tokenize a std::string.
 * Original version gleamed off the web some years ago
 * (no licensing or copyright information attached).
 *
 * Part of the Particle Simulation Toolkit (PST)
 *
 * Helgi
 * Nov. 27, 2001.
 */

//typedef void (*parsefun_t)(const std::deque<std::string>&);
//typedef std::map<std::string, parsefun_t> CallMap_t;


template <class CharType, class Traits, class StrAlloc, class DqAlloc>
void tokenize
(const typename std::basic_string<CharType, Traits, StrAlloc>& str,
 typename std::deque<
 typename std::basic_string<CharType, Traits, StrAlloc>, DqAlloc
 >& tok,
 const  typename std::basic_string<CharType, Traits, StrAlloc>& space
 = " \t\n")
{
  typedef typename std::basic_string<CharType, Traits, StrAlloc> StringType;

  typename StringType::size_type walk_pos = 0;

  while(walk_pos != StringType::npos && walk_pos < str.length()) {
    typename StringType::size_type token_pos = 0, token_len = 0;
    typename StringType::size_type delim_pos
      = str.find_first_of( space, walk_pos );

    if( delim_pos == StringType::npos ) {
      // no more spaces, a token starts at walk_pos
      token_pos = walk_pos;
      token_len = str.length() - token_pos;

      walk_pos += token_len;
    }
    else if( delim_pos > walk_pos ) {
      // more tokens / delims left, but a token starts at walk_pos
      token_pos = walk_pos;
      token_len = delim_pos - token_pos;

      walk_pos = delim_pos;
    }
    else if( delim_pos == walk_pos ) {
      // delimiters start at walk_pos
      walk_pos = str.find_first_not_of( space, walk_pos );

      if( walk_pos == StringType::npos ) {
        // only spaces left in str, no more tokens
        break;
      }
      else {
        // more tokens left
        continue;
      }
    }

    tok.push_back( str.substr( token_pos, token_len ) );
  }
}

/**
 * A utility function to trim spaces off a std::string.
 * Taken from the same web page as the simple std::string tokenizer above.
 *
 * Part of the Particle Simulation Toolkit (PST)
 *
 * Helgi
 * Nov. 27, 2001.
 */
template <class CharType, class Traits, class Alloc>
typename std::basic_string<CharType, Traits, Alloc>
trim(const typename std::basic_string<CharType, Traits, Alloc>& s,
     const typename std::basic_string<CharType, Traits, Alloc>& spaces
     = " \t")
{
  typedef typename std::basic_string<CharType, Traits, Alloc> StringType;

  if(s.length() == 0) {
    return s;
  }
  int b = s.find_first_not_of(spaces);
  int e = s.find_last_not_of(spaces);
  if(b == -1) { // No non-spaces
    return "";
  }

  return StringType(s, b, e - b + 1);
}

} // end of namespace pst::BasicStringTokenizer

} // end of namespace pst

#endif



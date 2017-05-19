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
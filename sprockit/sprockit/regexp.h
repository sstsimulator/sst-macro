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

#ifndef sprockit_regexp_h
#define sprockit_regexp_h

#include <sprockit/spkt_config.h>
#if !SPKT_DISABLE_REGEXP

#include <string>
#include <vector>

enum RegexpFlags {
  IgnoreCase=1,
  UpperCase=2,
  FindAll=4,
  LowerCase=8,
  ////////////////////////////////////////////////////////
  // Not available in C++11 (and also not currently used):
  IncludeLineBreaks=16,
  ////////////////////////////////////////////////////////
  StripWhitespace=32
};

/**
 * Extracts a double precision number from text.
 * @param regexp The regular expression containing the (captured) number.
 * @param text The text to apply the RegEx to.
 * @param flags An integer comprising bitwise options (see the PyFlags enum).
 * @return The value as a double.
 */
double get_regexp_double(const std::string &regexp, const std::string &text, int flags = 0);

/**
 * Extracts a string from text.
 * @param regexp The regular expression containing the (captured) string.
 * @param text The text to apply the RegEx to.
 * @param flags An integer comprising bitwise options (see the PyFlags enum).
 * @return The string.
 */
std::string get_regexp_string(const std::string &regexp, const std::string &text, int flags = 0);

/**
 * Extracts an array of doubles from text, according to a given RegEx.
 * Memory is allocated by the function and ownership is passed to calling routine.
 * @param regexp The regular expression containing the (captured) values.
 * @param text The text to apply the RegEx to.
 * @param flags An integer comprising bitwise options (see the PyFlags enum).
 * @return an array of all captured values.
 */
double* get_regexp_double_array(const std::string& regexp, const std::string &text, size_t& length, int flags = 0);

/**
 * Extracts strings from text, according to a given RegEx.
 * @param matches The vector of strings to hold the results.
 * @param regexp The regular expression containing the (captured) strings.
 * @param text The text to apply the RegEx to.
 * @param flags An integer comprising bitwise options (see the PyFlags enum).
 */
void
findmatch(std::vector<std::string>& matches, const std::string &regexp, const std::string &text, int flags = 0);

bool
get_regexp_integer(const std::string& string, int& ret);

inline bool
is_regexp_integer(const std::string& str){
  int ignore;
  return get_regexp_integer(str, ignore);
}

/**
 * Tests a string to see if it contains a certain RegEx.
 * @param regexp The regular expression to test.
 * @param text The text to apply the RegEx to.
 * @param flags An integer comprising bitwise options (see the PyFlags enum).
 * @return whether the text contains a match.
 */
bool has_regexp_match(const std::string& regexp, const std::string& text, int flags = 0);


#endif

#endif
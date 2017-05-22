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

#include <sstmac/dumpi_util/dumpi_meta.h>
#include <sprockit/errors.h>
#include <sprockit/basic_string_tokenizer.h>
#include <cstring>
#include <errno.h>


namespace sstmac {
namespace sw {

dumpi_meta::dumpi_meta(const std::string &filename)
{
  numprocscheck_ = false;
  fileprefixcheck_ = false;

  metafile_ = filename;

  openfile();
  init_callbacks();
  parsemeta();

  if(!numprocscheck_ && !fileprefixcheck_) {
    spkt_throw_printf(sprockit::value_error,
                     "The input dumpi metafile %s does not appear valid",
                     metafile_.c_str());
  }
}

dumpi_meta::~dumpi_meta() throw()
{

}

void dumpi_meta::parsemeta()
{

  std::deque<std::string> token;
  const std::string split_on = "=";
  std::string line;
  std::getline(infile_, line);

  while(infile_.good()) {
    token.clear();
    pst::BasicStringTokenizer::tokenize(line, token, split_on);
    if(token.size() != 2) {
    }
    else {
      CallMap_t::iterator it = callback_.find(token[0]);
      if(it != callback_.end()) {
        (this->*(it->second))(token);
      }
    }
    std::getline(infile_, line);
  }
  infile_.close();
}

void dumpi_meta::parse_numprocs(const std::deque<std::string> &token)
{
  numprocs_ = atoi(token[1].c_str());
  numprocscheck_ = true;
}

int dumpi_meta::getnumprocs()
{
  return numprocs_;
}

void dumpi_meta::parse_fileprefix(const std::deque<std::string> &token)
{

  fileprefix_ = token[1].c_str();
  //fileprefix_ += '-';

  size_t found;
  found = fileprefix_.find_first_of("/");

  if(found == 0) {
    dirplusfileprefix_ = fileprefix_;
  }
  else {
    found = metafile_.find_last_of("/");
    dirplusfileprefix_ = metafile_.substr(0, found+1) + fileprefix_;
  }

  fileprefixcheck_ = true;
}

void  dumpi_meta::init_callbacks()
{
  callback_["numprocs"] = &dumpi_meta::parse_numprocs;
  callback_["fileprefix"] = &dumpi_meta::parse_fileprefix;
}

bool dumpi_meta::openfile()
{
  infile_.open(metafile_.c_str());

  if(!infile_) {
    spkt_throw_printf(sprockit::io_error,
                     "dumpimeta::openfile: failed to open %s: error=%s",
                     metafile_.c_str(),
                     ::strerror(errno));
  }
  return true;
}
}
} //end sstmac namespace
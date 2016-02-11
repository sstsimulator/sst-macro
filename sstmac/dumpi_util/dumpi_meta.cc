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


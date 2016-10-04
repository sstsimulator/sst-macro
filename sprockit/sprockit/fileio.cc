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

#include <sprockit/fileio.h>
#include <sprockit/errors.h>

namespace sprockit {
std::list<std::string> SpktFileIO::file_paths_;

void
SpktFileIO::add_path(const std::string& path)
{
  file_paths_.push_front(path);
}

void
SpktFileIO::open_file(std::ifstream& in, const std::string& filename)
{
  //check to see if it is in the current folder
  in.open(filename.c_str());
  if (in.is_open()) {
    return;
  }

  for (auto& str : file_paths_) {
    std::string fullpath = str + "/" + filename;
    in.open(fullpath.c_str());
    if (in.is_open()) {
      return;
    }
  }
}

void
SpktFileIO::not_found(const std::string& filename)
{
  std::stringstream ss;
  ss << "File not found: " << filename << ".  Search path is: \n";
  for (auto& str : file_paths_) {
    ss << str << "\n";
  }

  spkt_throw(io_error, ss.str());
}

}


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

#ifndef SPROCKIT_FILEIO_H_INCLUDED
#define SPROCKIT_FILEIO_H_INCLUDED

#include <iostream>
#include <fstream>
#include <list>

namespace sprockit {

class SpktFileIO
{

 private:
  static std::list<std::string> file_paths_;

 public:
  static void open_file(std::ifstream& stream, const std::string& filename);

  static void add_path(const std::string& path);

  static void not_found(const std::string& filename);
};

}

#endif



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

#ifndef SSTMAC_SOFTWARE_SKELETONS_UNDUMPI_DUMPIMETA_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_UNDUMPI_DUMPIMETA_H_INCLUDED

#include <fstream>
#include <deque>
#include <map>

namespace sstmac {
namespace sw {


class dumpi_meta  {

 public:
  dumpi_meta(const std::string &filename);

  void parse_numprocs(const std::deque<std::string> &token);
  void parse_fileprefix(const std::deque<std::string> &token);

  void parsemeta();
  bool openfile();
  int getnumprocs();

  int num_procs() const {
    return numprocs_;
  }

  std::string fileprefix_;
  std::string dirplusfileprefix_;

  virtual ~dumpi_meta() throw();

 protected:
  std::string metafile_; //trace meta filename
  std::ifstream infile_;
  void init_callbacks();

  typedef void (dumpi_meta::*parsefun_t)(const
      std::deque<std::string>&);
  typedef std::map<std::string, parsefun_t> CallMap_t;
  CallMap_t callback_;
  int numprocs_;
  bool numprocscheck_;
  bool fileprefixcheck_;


};

}
} //end sstmac namespace

#endif // ! SSTMAC_SOFTWARE_SKELETONS_UNDUMPI_DUMPIMETA_H_INCLUDED


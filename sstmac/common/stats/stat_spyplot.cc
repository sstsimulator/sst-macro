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

#include <sstmac/common/stats/stat_spyplot.h>
#include <sstmac/common/lodepng.h>
#include <sstmac/backends/common/parallel_runtime.h>
#include <sprockit/output.h>
#include <sprockit/errors.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/util.h>
#include <algorithm>
#include <list>

namespace sstmac {

void
stat_spyplot::add_one(int source, int dest)
{
  add(source, dest, 1);
}

stat_spyplot_png::stat_spyplot_png(sprockit::sim_parameters* params) :
  stat_spyplot(params),
  fill_(false),
  normalization_(-1)
{
  normalization_ = params->get_optional_long_param("normalization", -1);
  fill_ = params->get_optional_bool_param("fill", false);
}

void
stat_spyplot_png::add(int source, int dest, long num)
{
  if (max_dest_ > 1024){
   //this is too big
   spkt_throw(sprockit::value_error,
     "stat_spyplot_png::add: PNG spyplots should never be run with more than 1024 ranks");
  }
  stat_spyplot::add(source, dest, num);
}

void
stat_spyplot::add(int source, int dest, long num)
{
  vals_[source][dest] += num;
  max_dest_ = std::max(max_dest_, dest);
}

void
stat_spyplot::reduce(stat_collector* coll)
{
  stat_spyplot* other = safe_cast(stat_spyplot, coll);
  spyplot_map::iterator it, end = other->vals_.end();
  for (it=other->vals_.begin(); it != end; ++it){
    int source = it->first;
    long_map& my_map = vals_[source];
    long_map& his_map = it->second;
    long_map::iterator lit, lend = his_map.end();
    for (lit=his_map.begin(); lit != lend; ++lit){
      int dest = lit->first;
      long count = lit->second;
      my_map[dest] += count;
      max_dest_ = std::max(max_dest_, dest);
    }
  }
}

void
stat_spyplot::global_reduce(parallel_runtime* rt)
{
  if (rt->nproc() == 1)
    return;

  int num_rows = max_dest_ + 1;
  num_rows = rt->global_max(num_rows);
  int num_vals = num_rows * num_rows;
  long* vals = new long[num_vals];
  ::memset(vals, 0, num_vals*sizeof(long));

  spyplot_map::iterator it, end = vals_.end();
  for (it=vals_.begin(); it != end; ++it){
    int source = it->first;
    long_map& my_map = vals_[source];
    long_map& his_map = it->second;
    long_map::iterator lit, lend = his_map.end();
    for (lit=his_map.begin(); lit != lend; ++lit){
      int dest = lit->first;
      int idx = source * num_rows + dest;
      long count = lit->second;
      //printf("writing value %ld to idx %d\n", count, idx);
      vals[idx] = count;
    }
  }

  int root = 0;
  rt->global_sum(vals, num_vals, root);

  long* bufptr = vals;
  for (int i=0; i < num_rows; ++i){
    long_map& my_map = vals_[i];
    for (int j=0; j < num_rows; ++j, ++bufptr){
      my_map[j] = *bufptr;
    }
  }

  delete[] vals;
}

void
stat_spyplot::clear()
{
  vals_.clear();
}

void
stat_spyplot::dump_local_data()
{
  spkt_throw(sprockit::unimplemented_error,
    "stat_spyplot::dump_local_data");
}

void
stat_spyplot::dump_global_data()
{
  dump_to_file(fileroot_);
}

void
stat_spyplot::dump_to_file(const std::string& froot)
{
  std::string filename = froot + ".csv";
  std::fstream myfile;
  if (check_open(myfile, filename)) {
    std::list<long> nodes;

    {
      spyplot_map::iterator it, end = vals_.end();
      for (it = vals_.begin(); it != end; it++) {
        nodes.push_back(it->first);
      }
    }

    nodes.sort();

    for (long nid : nodes) {
      long_map& submap = vals_[nid];
      std::list<long>::const_iterator it2 = nodes.begin(), end2 = nodes.end();
      long datapoint = submap[*it2];
      myfile << datapoint;
      ++it2;
      for (; it2 != end2; ++it2) {
        long datapoint = submap[*it2];
        myfile << "," << datapoint;
      }
      myfile << "\n";
    }

    myfile.close();
  }
}

void
stat_spyplot::simulation_finished(timestamp end)
{
}

/**
 *
 * @param val - a number between 0 and 1
 * @return a 24-bit pixel
 */
int
get_pixel(double val)
{
  /* if(val < 0.333333333){
   return 255 * 3 * val;
   }else if(val < 0.666666666){
   return ((int)(255 * 3 * (val - 0.333333333))) << 8;
   }else{
   return ((int)(255 * 3 * (val - 0.666666666))) << 16;
   }*/
  //return (1 << 23) * val;
  //return std::min((1<<23), (int)std::pow(10, 6.2 * val));
  //return ((int)(255 * (1.0 - val))) << 16;

  double quant = 1.0 / 3.0;
  int r = 0, g = 0, b = 0;
  /*if (val < quant) //blue
   {
   b = 255 * (val) / quant;
   }
   else if (val < 2 * quant) //blue-green
   {
   b = 255 * (1.0 - (val - quant)) / quant;
   g = ((int) (255 * ((val - quant)) / quant));
   }
   else if (val < 3 * quant)
   { //green
   g = 255 * (val) / quant;
   }
   else if (val < 4 * quant)
   {
   g = 255 * (1.0 - (val - 3 * quant)) / quant;
   r = ((int) (255 * ((val - 3 * quant)) / quant));
   }
   else
   {
   r = 255 * (val) / quant;
   }*/

  b = std::max(
        0,
        (int) ((val < quant) ? (255.0 * (val) / quant) : (255.0 * (quant - (val
               - quant)) / quant)));
  if (val < quant) {
    r = 0;
    g = 0;
  }
  else {
    g = std::max(
          0,
          (int) ((val < 2 * quant) ? (255.0 * (val - quant) / quant) : (255.0
                 * (3 * quant - (val)) / quant)));

    if (val < 2 * quant) {
      r = 0;
    }
    else {
      r = (255 * (val - 2 * quant) / quant);
    }
  }

  r = std::min(r, 255);
  g = std::min(g, 255);
  b = std::min(b, 255);

  return (r) | (g << 8) | (b << 16);
}

void
stat_spyplot_png::dump_to_file(const std::string& froot)
{
  stat_spyplot::dump_to_file(froot);
  long maxval = 0;
  long maxsrc = 0;
  long maxdest = 0;
  std::list<long> nodes;
  {
      spyplot_map::const_iterator it, end = vals_.end();
      for (it = vals_.begin(); it != end; it++) {
        long src = it->first;
        nodes.push_back(src);
        if (src > maxsrc) {
          maxsrc = src;
        }

        long_map::const_iterator it2, end2 = it->second.end();
        for (it2 = it->second.begin(); it2 != end2; ++it2) {
          long dest = it2->first;
          long val = it2->second;
          if (dest > maxdest) {
            maxdest = dest;
          }

          if (val > maxval) {
            maxval = val;
          }
        }
      }
    }
    maxval = normalization_ > 0 ? normalization_ : maxval;
    cout0 << "PNG Spyplot " << froot << " normalized to " << maxval << std::endl;

    nodes.sort();

    long pixels_per_point = 10;

    long dim = fill_ ? (std::max(maxsrc, maxdest) + 1) : vals_.size();

    long width = (dim + 2) * pixels_per_point;
    long height = dim * pixels_per_point;

    long imagesize = (width) * height * 4;
    std::vector<unsigned char> image;
    image.resize(imagesize);

    long cnt = 0;
    long heightcnt = height;

    if(vals_.size() > 1024) {
      cerr0 << "WARNING: disabling spyplot png because it would be too large\n";
      return;
    }

    for (long src = 0; src <= std::max(maxsrc, maxdest); src++) {
      if (fill_ || vals_.find(src) != vals_.end()) {
        for (int j = 0; j < pixels_per_point; j++) {
          if (fill_) {
            for (long dest = 0; dest <= std::max(maxsrc, maxdest); dest++) {

              int datapoint = get_pixel(
                                (double) vals_[src][dest] / (double) maxval);
              for (int i = 0; i < pixels_per_point; i++) {
                image[cnt++] = datapoint & 0xFF;
                image[cnt++] = (datapoint >> 8) & 0xFF;
                image[cnt++] = (datapoint >> 16) & 0xFF;
                image[cnt++] = 255; //alpha
              }
            }
          }
          else {
            long_map& submap = vals_[src];
            std::list<long>::const_iterator it2 = nodes.begin(), end2 =
                                                    nodes.end();

            for (; it2 != end2; it2++) {
              int datapoint = get_pixel(
                                (double) submap[*it2] / (double) maxval);
              for (int i = 0; i < pixels_per_point; i++) {
                image[cnt++] = datapoint & 0xFF;
                image[cnt++] = (datapoint >> 8) & 0xFF;
                image[cnt++] = (datapoint >> 16) & 0xFF;
                image[cnt++] = 255; //alpha
              }
            }

          }

          //a clear space
          for (int i = 0; i < pixels_per_point; i++) {
            image[cnt++] = 0;
            image[cnt++] = 0;
            image[cnt++] = 0;
            image[cnt++] = 0; //alpha
          }

          int legend = get_pixel((double) heightcnt / (double) height);
          for (int i = 0; i < pixels_per_point; i++) {
            image[cnt++] = legend & 0xFF;
            image[cnt++] = (legend >> 8) & 0xFF;
            image[cnt++] = (legend >> 16) & 0xFF;
            image[cnt++] = 255; //alpha
          }

          heightcnt--;

        }
      }
    }

    std::string fullpath = froot + ".png";

    //Encode the image
    unsigned error = lodepng::encode(fullpath, image, width, height);

    //if there's an error, display it
    if (error){
      cerr0 << "stat_spyplot_png: PNG encoder error " << error << ": "
            << lodepng_error_text(error) << std::endl;
    }
}

} //end namespace
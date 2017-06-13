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

#ifndef SSTMAC_SOFTWARE_SKELETONS_UNDUMPI_DUMPITYPEIO_H_INCLUDED
#define SSTMAC_SOFTWARE_SKELETONS_UNDUMPI_DUMPITYPEIO_H_INCLUDED

#include <dumpi/common/argtypes.h>
#include <iostream>
#include <sstream>
#include <iterator>
#include <algorithm>
#include <iomanip>

namespace sstmac {
namespace sw {

/*
 * Allow C++ IO on DUMPI types.
 */

/**
 * Print status info.
 */
inline
std::ostream& operator<<(std::ostream &os, const dumpi_status& status)
{
  if(&status) {
    os << "dumpi_status(";
    os << "bytes=" << status.bytes << ", ";
    os << "source=" << status.source << ", ";
    os << "tag=" << status.tag << ", ";
    os << "cancelled=" << status.cancelled << ", ";
    os << "error=" << status.error << ")";
  }
  else {
    os << "NULL";
  }
  return os;
}

/**
 * Dumpi timestamp.
 */
inline
std::ostream& operator<<(std::ostream &os, const dumpi_clock &tt)
{
  os << "dumpi_clock(" << tt.sec << "." << std::setw(8)
     << std::setfill('0') << tt.nsec << ")";
  return os;
}

/**
 * Dumpi time value.
 */
inline
std::ostream& operator<<(std::ostream &os, const dumpi_time &tt)
{
  os << "dumpi_time(start=" << tt.start << ", stop=" << tt.stop << ")";
  return os;
}

/**
 * Namespace for internal utility print routines.
 */
namespace P {
template <typename T>
std::string ints(int len, const T *arr)
{
  if(arr) {
    std::ostringstream oss;
    oss << "[";
    // This results in ICE in g++-4.0
    //std::copy(arr, arr+len,
    //          typename std::ostream_iterator<T>(oss, ", "));
    std::copy(arr, arr+len, std::ostream_iterator<T>(oss, ", "));
    oss << "]";
    return oss.str();
  }
  else {
    return "NULL";
  }
}

template <typename T>
std::string ints(bool do_output, int len, const T *arr)
{
  if(do_output) {
    return ints(len, arr);
  }
  else {
    return "<invalid>";
  }
}

template <typename T>
std::string ints2d(int xl, int yl, T **arr)
{
  if(arr) {
    std::ostringstream oss;
    oss << "[";
    for(int x = 0; x < xl; ++x) {
      if(arr[x]) {
        oss << ints(yl, arr[x]);
      }
      else {
        oss << "NULL";
      }
      if(x < xl-1) {
        oss << ", ";
      }
    }
    oss << "]";
    return oss.str();
  }
  else {
    return "NULL";
  }
}

inline
std::string statuses(int len, const dumpi_status *statuses)
{
  if(statuses) {
    std::ostringstream oss;
    oss << "[";
    for(int i = 0; i < len; ++i) {
      oss << (statuses+i);
      if(i < len-1) {
        oss << ", ";
      }
    }
    oss << "]";
    return oss.str();
  }
  else {
    return "NULL";
  }
}

inline
std::string stringarray(int len, char **arr)
{
  if(arr) {
    std::ostringstream oss;
    oss << "[";
    for(int i = 0; i < len; ++i) {
      oss << "\"" << arr[i] << "\"";
      if(i < len-1) {
        oss << ", ";
      }
    }
    oss << "]";
    return oss.str();
  }
  else {
    return "NULL";
  }
}

inline
std::string stringarray(bool do_output, int len, char **arr)
{
  if(do_output) {
    return stringarray(len, arr);
  }
  else {
    return "<invalid>";
  }
}

inline
std::string stringarraynull(char **arr)
{
  if(arr) {
    std::ostringstream oss;
    oss << "[";
    for(int i = 0; arr[i] != NULL; ++i) {
      oss << "\"" << arr[i] << "\"";
      if(arr[i+1] != NULL) {
        oss << ", ";
      }
    }
    oss << "]";
    return oss.str();
  }
  else {
    return "NULL";
  }
}

inline
std::string stringarraynull(bool do_output, char **arr)
{
  if(do_output) {
    return stringarraynull(arr);
  }
  else {
    return "<invalid>";
  }
}

inline
std::string stringarraynull2d(bool do_output, int xl, char ***arr)
{
  if(do_output) {
    if(arr) {
      std::ostringstream oss;
      oss << "[";
      for(int x = 0; x < xl; ++x) {
        if(arr[x]) {
          oss << stringarraynull(arr[x]);
        }
        else {
          oss << "NULL";
        }
        if(x < xl-1) {
          oss << ", ";
        }
      }
      oss << "]";
      return oss.str();
    }
    else {
      return "NULL";
    }
  }
  else {
    return "<invalid>";
  }
}
}

/**
 * C++ IO for a function call.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_func_call &dd)
{
  if(&dd) {
    os << "dumpi_func_call(fn=" << dd.fn << ")";
  }
  else {
    os << "dumpi_func_call(NULL)";
  }
  return os;
}

/**
 * C++ IO for a send operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_send &dd)
{
  os << "dumpi_send(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "dest=" << dd.dest << ", ";
    /** Argument value before PMPI call */
    os << "tag=" << dd.tag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a recv operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_recv &dd)
{
  os << "dumpi_recv(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "source=" << dd.source << ", ";
    /** Argument value before PMPI call */
    os << "tag=" << dd.tag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a get_count operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_get_count &dd)
{
  os << "dumpi_get_count(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "status=" << *dd.status << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "count=" << dd.count << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a bsend operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_bsend &dd)
{
  os << "dumpi_bsend(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "dest=" << dd.dest << ", ";
    /** Argument value before PMPI call */
    os << "tag=" << dd.tag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a ssend operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_ssend &dd)
{
  os << "dumpi_ssend(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "dest=" << dd.dest << ", ";
    /** Argument value before PMPI call */
    os << "tag=" << dd.tag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a rsend operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_rsend &dd)
{
  os << "dumpi_rsend(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "dest=" << dd.dest << ", ";
    /** Argument value before PMPI call */
    os << "tag=" << dd.tag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a buffer_attach operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_buffer_attach &dd)
{
  os << "dumpi_buffer_attach(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "size=" << dd.size << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a buffer_detach operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_buffer_detach &dd)
{
  os << "dumpi_buffer_detach(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "size=" << dd.size << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a isend operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_isend &dd)
{
  os << "dumpi_isend(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "dest=" << dd.dest << ", ";
    /** Argument value before PMPI call */
    os << "tag=" << dd.tag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a ibsend operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_ibsend &dd)
{
  os << "dumpi_ibsend(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "dest=" << dd.dest << ", ";
    /** Argument value before PMPI call */
    os << "tag=" << dd.tag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a issend operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_issend &dd)
{
  os << "dumpi_issend(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "dest=" << dd.dest << ", ";
    /** Argument value before PMPI call */
    os << "tag=" << dd.tag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a irsend operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_irsend &dd)
{
  os << "dumpi_irsend(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "dest=" << dd.dest << ", ";
    /** Argument value before PMPI call */
    os << "tag=" << dd.tag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a irecv operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_irecv &dd)
{
  os << "dumpi_irecv(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "source=" << dd.source << ", ";
    /** Argument value before PMPI call */
    os << "tag=" << dd.tag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a wait operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_wait &dd)
{
  os << "dumpi_wait(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "request=" << dd.request << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a test operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_test &dd)
{
  os << "dumpi_test(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "request=" << dd.request << ", ";
    /** Argument value after PMPI call */
    os << "flag=" << dd.flag << ", ";
    /** Argument value before PMPI call.  Only stored if(flag != 0) */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a request_free operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_request_free &dd)
{
  if(&dd) {
    os << "dumpi_request_free(";
    /** Argument value before PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a waitany operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_waitany &dd)
{
  if(&dd) {
    os << "dumpi_waitany(";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "requests=" << P::ints(dd.count, dd.requests) << ", ";
    /** Argument value after PMPI call */
    os << "index=" << dd.index << ", ";
    /** Argument value before PMPI call.  Only stored if(index != MPI_UNDEFINED) */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a testany operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_testany &dd)
{
  os << "dumpi_testany(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "requests=" << P::ints(dd.count, dd.requests) << ", ";
    /** Argument value after PMPI call */
    os << "index=" << dd.index << ", ";
    /** Argument value after PMPI call */
    os << "flag=" << dd.flag << ", ";
    /** Argument value before PMPI call.  Only stored if(flag != 0) */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a waitall operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_waitall &dd)
{
  os << "dumpi_waitall(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "requests=" << P::ints(dd.count, dd.requests) << ", ";
    /** Argument value after PMPI call.  Array of length [count] */
    os << "statuses=" << P::statuses(dd.count, dd.statuses) << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a testall operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_testall &dd)
{
  os << "dumpi_testall(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "requests=" << P::ints(dd.count, dd.requests) << ", ";
    /** Argument value after PMPI call */
    os << "flag=" << dd.flag << ", ";
    /**
     * Argument value after PMPI call.  Array of length [count].
     * Only stored if(flag != 0)
     */
    os << "status=" << P::statuses(dd.count, dd.statuses) << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a waitsome operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_waitsome &dd)
{
  os << "dumpi_waitsome(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "requests=" << P::ints(dd.count, dd.requests) << ", ";
    /** Argument value after PMPI call */
    os << "outcount=" << dd.outcount << ", ";
    /** Argument value after PMPI call.  Array of length [*outcount] */
    os << "indices=" << P::ints(dd.outcount, dd.indices) << ", ";
    /** Argument value after PMPI call.  Array of length [*outcount] */
    os << "statuses=" << P::statuses(dd.outcount, dd.statuses) << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a testsome operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_testsome &dd)
{
  os << "dumpi_testsome(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "requests=" << P::ints(dd.count, dd.requests) << ", ";
    /** Argument value after PMPI call */
    os << "outcount=" << dd.outcount << ", ";
    /** Argument value after PMPI call.  Array of length [*outcount] */
    os << "indices=" << P::ints(dd.outcount, dd.indices) << ", ";
    /** Argument value after PMPI call.  Array of length [*outcount] */
    os << "statuses=" << P::statuses(dd.outcount, dd.statuses) << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a iprobe operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_iprobe &dd)
{
  os << "dumpi_iprobe(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "source=" << dd.source << ", ";
    /** Argument value before PMPI call */
    os << "tag=" << dd.tag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "flag=" << dd.flag << ", ";
    /** Argument value before PMPI call.  Only stored if(flag != 0) */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a probe operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_probe &dd)
{
  os << "dumpi_probe(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "source=" << dd.source << ", ";
    /** Argument value before PMPI call */
    os << "tag=" << dd.tag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a cancel operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_cancel &dd)
{
  os << "dumpi_cancel(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a test_cancelled operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_test_cancelled &dd)
{
  os << "dumpi_test_cancelled(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "status=" << *dd.status << ", ";
    /** Argument value after PMPI call */
    os << "cancelled=" << dd.cancelled << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a send_init operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_send_init &dd)
{
  os << "dumpi_send_init(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "dest=" << dd.dest << ", ";
    /** Argument value before PMPI call */
    os << "tag=" << dd.tag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a bsend_init operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_bsend_init &dd)
{
  os << "dumpi_bsend_init(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "dest=" << dd.dest << ", ";
    /** Argument value before PMPI call */
    os << "tag=" << dd.tag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a ssend_init operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_ssend_init &dd)
{
  os << "dumpi_ssend_init(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "dest=" << dd.dest << ", ";
    /** Argument value before PMPI call */
    os << "tag=" << dd.tag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a rsend_init operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_rsend_init &dd)
{
  os << "dumpi_rsend_init(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "dest=" << dd.dest << ", ";
    /** Argument value before PMPI call */
    os << "tag=" << dd.tag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a recv_init operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_recv_init &dd)
{
  os << "dumpi_recv_init(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "source=" << dd.source << ", ";
    /** Argument value before PMPI call */
    os << "tag=" << dd.tag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a start operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_start &dd)
{
  os << "dumpi_start(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a startall operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_startall &dd)
{
  os << "dumpi_startall(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value after PMPI call.  Array of length [count] */
    os << "requests=" << P::ints(dd.count, dd.requests) << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a sendrecv operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_sendrecv &dd)
{
  os << "dumpi_sendrecv(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "sendcount=" << dd.sendcount << ", ";
    /** Argument value before PMPI call */
    os << "sendtype=" << dd.sendtype << ", ";
    /** Argument value before PMPI call */
    os << "dest=" << dd.dest << ", ";
    /** Argument value before PMPI call */
    os << "sendtag=" << dd.sendtag << ", ";
    /** Argument value before PMPI call */
    os << "recvcount=" << dd.recvcount << ", ";
    /** Argument value before PMPI call */
    os << "recvtype=" << dd.recvtype << ", ";
    /** Argument value before PMPI call */
    os << "source=" << dd.source << ", ";
    /** Argument value before PMPI call */
    os << "recvtag=" << dd.recvtag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a sendrecv_replace operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_sendrecv_replace &dd)
{
  os << "dumpi_sendrecv_replace(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "dest=" << dd.dest << ", ";
    /** Argument value before PMPI call */
    os << "sendtag=" << dd.sendtag << ", ";
    /** Argument value before PMPI call */
    os << "source=" << dd.source << ", ";
    /** Argument value before PMPI call */
    os << "recvtag=" << dd.recvtag << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_contiguous operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_contiguous &dd)
{
  os << "dumpi_type_contiguous(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "oldtype=" << dd.oldtype << ", ";
    /** Argument value after PMPI call */
    os << "newtype=" << dd.newtype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_vector operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_type_vector &dd)
{
  os << "dumpi_type_vector(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "blocklength=" << dd.blocklength << ", ";
    /** Argument value before PMPI call */
    os << "stride=" << dd.stride << ", ";
    /** Argument value before PMPI call */
    os << "oldtype=" << dd.oldtype << ", ";
    /** Argument value after PMPI call */
    os << "newtype=" << dd.newtype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_hvector operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_type_hvector &dd)
{
  os << "dumpi_type_hvector(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "blocklength=" << dd.blocklength << ", ";
    /** Argument value before PMPI call */
    os << "stride=" << dd.stride << ", ";
    /** Argument value before PMPI call */
    os << "oldtype=" << dd.oldtype << ", ";
    /** Argument value after PMPI call */
    os << "newtype=" << dd.newtype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_indexed operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_type_indexed &dd)
{
  os << "dumpi_type_indexed(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "lengths=" << P::ints(dd.count, dd.lengths) << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "indices=" << P::ints(dd.count, dd.indices) << ", ";
    /** Argument value before PMPI call */
    os << "oldtype=" << dd.oldtype << ", ";
    /** Argument value after PMPI call */
    os << "newtype=" << dd.newtype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_hindexed operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_hindexed &dd)
{
  os << "dumpi_type_hindexed(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "lengths=" << P::ints(dd.count, dd.lengths) << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "indices=" << P::ints(dd.count, dd.indices) << ", ";
    /** Argument value before PMPI call */
    os << "oldtype=" << dd.oldtype << ", ";
    /** Argument value after PMPI call */
    os << "newtype=" << dd.newtype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_struct operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_type_struct &dd)
{
  os << "dumpi_type_struct(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "lengths=" << P::ints(dd.count, dd.lengths) << ", ";
    /** Argument value before PMPI call */
    os << "indices=" << dd.indices << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "oldtypes=" << P::ints(dd.count, dd.oldtypes) << ", ";
    /** Argument value after PMPI call */
    os << "newtype=" << dd.newtype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a address operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_address &dd)
{
  os << "dumpi_address(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "address=" << dd.address << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_extent operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_type_extent &dd)
{
  os << "dumpi_type_extent(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "extent=" << dd.extent << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_size operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_type_size &dd)
{
  os << "dumpi_type_size(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "size=" << dd.size << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_lb operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_type_lb &dd)
{
  os << "dumpi_type_lb(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "lb=" << dd.lb << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_ub operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_type_ub &dd)
{
  os << "dumpi_type_ub(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "ub=" << dd.ub << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_commit operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_type_commit &dd)
{
  os << "dumpi_type_commit(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_free operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_type_free &dd)
{
  os << "dumpi_type_free(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a get_elements operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_get_elements &dd)
{
  os << "dumpi_get_elements(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "status=" << *dd.status << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "elements=" << dd.elements << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a pack operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_pack &dd)
{
  os << "dumpi_pack(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "incount=" << dd.incount << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "outcount=" << dd.outcount << ", ";
    /** Argument value before and after PMPI call. */
    os << "position={in=" << dd.position.in << ", out="
       << dd.position.out << "}, ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a unpack operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_unpack &dd)
{
  os << "dumpi_unpack(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "incount=" << dd.incount << ", ";
    /** Argument value before and after PMPI call. */
    os << "position={in=" << dd.position.in << ", out="
       << dd.position.out << "}, ";
    /** Argument value before PMPI call */
    os << "outcount=" << dd.outcount << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a pack_size operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_pack_size &dd)
{
  os << "dumpi_pack_size(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "incount=" << dd.incount << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "size=" << dd.size << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a barrier operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_barrier &dd)
{
  os << "dumpi_barrier(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a bcast operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_bcast &dd)
{
  os << "dumpi_bcast(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "root=" << dd.root << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a gather operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_gather &dd)
{
  os << "dumpi_gather(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "commrank=" << dd.commrank << ", ";
    /** Argument value before PMPI call */
    os << "sendcount=" << dd.sendcount << ", ";
    /** Argument value before PMPI call */
    os << "sendtype=" << dd.sendtype << ", ";
    /** Argument value before PMPI call.  Only stored if(commrank==root) */
    os << "recvcount=" << dd.recvcount << ", ";
    /** Argument value before PMPI call.  Only stored if(commrank==root) */
    os << "recvtype=" << dd.recvtype << ", ";
    /** Argument value before PMPI call */
    os << "root=" << dd.root << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a gatherv operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_gatherv &dd)
{
  os << "dumpi_gatherv(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "commrank=" << dd.commrank << ", ";
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "commsize=" << dd.commsize << ", ";
    /** Argument value before PMPI call */
    os << "sendcount=" << dd.sendcount << ", ";
    /** Argument value before PMPI call */
    os << "sendtype=" << dd.sendtype << ", ";
    /**
     * Argument value before PMPI call.  Array of length [commsize].
     * Only stored if(commrank==root)
     */
    os << "recvcounts="
       << P::ints(dd.commrank == dd.root, dd.commsize, dd.recvcounts)
       << ", ";
    /**
     * Argument value before PMPI call.  Array of length [commsize].
     * Only stored if(commrank==root)
     */
    os << "displs="
       << P::ints(dd.commrank == dd.root, dd.commsize, dd.displs)
       << ", ";
    /** Argument value before PMPI call.  Only stored if(commrank==root) */
    os << "recvtype=" << dd.recvtype << ", ";
    /** Argument value before PMPI call */
    os << "root=" << dd.root << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a scatter operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_scatter &dd)
{
  os << "dumpi_scatter(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "commrank=" << dd.commrank << ", ";
    /** Argument value before PMPI call.  Only stored if(commrank==root) */
    os << "sendcount=" << dd.sendcount << ", ";
    /** Argument value before PMPI call.  Only stored if(commrank==root) */
    os << "sendtype=" << dd.sendtype << ", ";
    /** Argument value before PMPI call */
    os << "recvcount=" << dd.recvcount << ", ";
    /** Argument value before PMPI call */
    os << "recvtype=" << dd.recvtype << ", ";
    /** Argument value before PMPI call */
    os << "root=" << dd.root << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a scatterv operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_scatterv &dd)
{
  os << "dumpi_scatterv(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "commrank=" << dd.commrank << ", ";
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "commsize=" << dd.commsize << ", ";
    /**
     * Argument value before PMPI call.  Array of length [commsize].
     * Only stored if(commrank==root)
     */
    os << "sendcounts="
       << P::ints(dd.commrank == dd.root, dd.commsize, dd.sendcounts)
       << ", ";
    /**
     * Argument value before PMPI call.  Array of length [commsize].
     * Only stored if(commrank==root)
     */
    os << "displs="
       << P::ints(dd.commrank == dd.root, dd.commsize, dd.displs)
       << ", ";
    /** Argument value before PMPI call */
    os << "sendtype=" << dd.sendtype << ", ";
    /** Argument value before PMPI call */
    os << "recvcount=" << dd.recvcount << ", ";
    /** Argument value before PMPI call */
    os << "recvtype=" << dd.recvtype << ", ";
    /** Argument value before PMPI call */
    os << "root=" << dd.root << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a allgather operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_allgather &dd)
{
  os << "dumpi_allgather(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "sendcount=" << dd.sendcount << ", ";
    /** Argument value before PMPI call */
    os << "sendtype=" << dd.sendtype << ", ";
    /** Argument value before PMPI call */
    os << "recvcount=" << dd.recvcount << ", ";
    /** Argument value before PMPI call */
    os << "recvtype=" << dd.recvtype << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a allgatherv operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_allgatherv &dd)
{
  os << "dumpi_allgatherv(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "commsize=" << dd.commsize << ", ";
    /** Argument value before PMPI call */
    os << "sendcount=" << dd.sendcount << ", ";
    /** Argument value before PMPI call */
    os << "sendtype=" << dd.sendtype << ", ";
    /** Argument value before PMPI call.  Array of length [commsize] */
    os << "recvcounts=" << P::ints(dd.commsize, dd.recvcounts) << ", ";
    /** Argument value before PMPI call.  Array of length [commsize] */
    os << "displs=" << P::ints(dd.commsize, dd.displs) << ", ";
    /** Argument value before PMPI call */
    os << "recvtype=" << dd.recvtype << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a alltoall operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_alltoall &dd)
{
  os << "dumpi_alltoall(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "sendcount=" << dd.sendcount << ", ";
    /** Argument value before PMPI call */
    os << "sendtype=" << dd.sendtype << ", ";
    /** Argument value before PMPI call */
    os << "recvcount=" << dd.recvcount << ", ";
    /** Argument value before PMPI call */
    os << "recvtype=" << dd.recvtype << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a alltoallv operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_alltoallv &dd)
{
  os << "dumpi_alltoallv(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "commsize=" << dd.commsize << ", ";
    /** Argument value before PMPI call.  Array of length [commsize] */
    os << "sendcounts=" << P::ints(dd.commsize, dd.sendcounts) << ", ";
    /** Argument value before PMPI call.  Array of length [commsize] */
    os << "senddispls=" << P::ints(dd.commsize, dd.senddispls) << ", ";
    /** Argument value before PMPI call */
    os << "sendtype=" << dd.sendtype << ", ";
    /** Argument value before PMPI call.  Array of length [commsize] */
    os << "recvcounts=" << P::ints(dd.commsize, dd.recvcounts) << ", ";
    /** Argument value before PMPI call.  Array of length [commsize] */
    os << "recvdispls=" << P::ints(dd.commsize, dd.recvdispls) << ", ";
    /** Argument value before PMPI call */
    os << "recvtype=" << dd.recvtype << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a reduce operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_reduce &dd)
{
  os << "dumpi_reduce(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "op=" << dd.op << ", ";
    /** Argument value before PMPI call */
    os << "root=" << dd.root << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a op_create operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_op_create &dd)
{
  os << "dumpi_op_create(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "commute=" << dd.commute << ", ";
    /** Argument value after PMPI call */
    os << "op=" << dd.op << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a op_free operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_op_free &dd)
{
  os << "dumpi_op_free(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "op=" << dd.op << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a allreduce operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_allreduce &dd)
{
  os << "dumpi_allreduce(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "op=" << dd.op << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a reduce_scatter operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_reduce_scatter &dd)
{
  os << "dumpi_reduce_scatter(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "commsize=" << dd.commsize << ", ";
    /** Argument value before PMPI call.  Array of length [commsize] */
    os << "recvcounts=" << P::ints(dd.commsize, dd.recvcounts) << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "op=" << dd.op << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a scan operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_scan &dd)
{
  os << "dumpi_scan(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "op=" << dd.op << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a group_size operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_group_size &dd)
{
  os << "dumpi_group_size(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "group=" << dd.group << ", ";
    /** Argument value after PMPI call */
    os << "size=" << dd.size << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a group_rank operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_group_rank &dd)
{
  os << "dumpi_group_rank(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "group=" << dd.group << ", ";
    /** Argument value after PMPI call */
    os << "rank=" << dd.rank << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a group_translate_ranks operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_group_translate_ranks &dd)
{
  os << "dumpi_group_translate_ranks(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "group1=" << dd.group1 << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "ranks1=" << P::ints(dd.count, dd.ranks1) << ", ";
    /** Argument value before PMPI call */
    os << "group2=" << dd.group2 << ", ";
    /** Argument value after PMPI call.  Array of length [count] */
    os << "ranks2=" << P::ints(dd.count, dd.ranks2) << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a group_compare operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_group_compare &dd)
{
  os << "dumpi_group_compare(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "group1=" << dd.group1 << ", ";
    /** Argument value before PMPI call */
    os << "group2=" << dd.group2 << ", ";
    /** Argument value after PMPI call */
    os << "result=" << dd.result << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_group operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_comm_group &dd)
{
  os << "dumpi_comm_group(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "group=" << dd.group << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a group_union operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_group_union &dd)
{
  os << "dumpi_group_union(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "group1=" << dd.group1 << ", ";
    /** Argument value before PMPI call */
    os << "group2=" << dd.group2 << ", ";
    /** Argument value after PMPI call */
    os << "newgroup=" << dd.newgroup << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a group_intersection operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_group_intersection &dd)
{
  os << "dumpi_group_intersection(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "group1=" << dd.group1 << ", ";
    /** Argument value before PMPI call */
    os << "group2=" << dd.group2 << ", ";
    /** Argument value after PMPI call */
    os << "newgroup=" << dd.newgroup << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a group_difference operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_group_difference &dd)
{
  os << "dumpi_group_difference(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "group1=" << dd.group1 << ", ";
    /** Argument value before PMPI call */
    os << "group2=" << dd.group2 << ", ";
    /** Argument value after PMPI call */
    os << "newgroup=" << dd.newgroup << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a group_incl operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_group_incl &dd)
{
  os << "dumpi_group_incl(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "group=" << dd.group << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "ranks=" << P::ints(dd.count, dd.ranks) << ", ";
    /** Argument value after PMPI call */
    os << "newgroup=" << dd.newgroup << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a group_excl operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_group_excl &dd)
{
  os << "dumpi_group_excl(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "group=" << dd.group << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "ranks=" << P::ints(dd.count, dd.ranks) << ", ";
    /** Argument value after PMPI call */
    os << "newgroup=" << dd.newgroup << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a group_range_incl operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_group_range_incl &dd)
{
  os << "dumpi_group_range_incl(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "group=" << dd.group << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  2D-aray of length [count][3] */
    os << "ranges=" << P::ints2d(dd.count, 3, dd.ranges) << ", ";
    /** Argument value after PMPI call */
    os << "newgroup=" << dd.newgroup << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a group_range_excl operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_group_range_excl &dd)
{
  os << "dumpi_group_range_excl(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "group=" << dd.group << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  2D-array of length [count][3] */
    os << "ranges=" << P::ints2d(dd.count, 3, dd.ranges) << ", ";
    /** Argument value after PMPI call */
    os << "newgroup=" << dd.newgroup << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a group_free operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_group_free &dd)
{
  os << "dumpi_group_free(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "group=" << dd.group << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_size operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_comm_size &dd)
{
  os << "dumpi_comm_size(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "size=" << dd.size << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_rank operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_comm_rank &dd)
{
  os << "dumpi_comm_rank(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "rank=" << dd.rank << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_compare operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_comm_compare &dd)
{
  os << "dumpi_comm_compare(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm1=" << dd.comm1 << ", ";
    /** Argument value before PMPI call */
    os << "comm2=" << dd.comm2 << ", ";
    /** Argument value after PMPI call */
    os << "result=" << dd.result << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_dup operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_comm_dup &dd)
{
  os << "dumpi_comm_dup(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "oldcomm=" << dd.oldcomm << ", ";
    /** Argument value after PMPI call */
    os << "newcomm=" << dd.newcomm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_create operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_comm_create &dd)
{
  os << "dumpi_comm_create(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "oldcomm=" << dd.oldcomm << ", ";
    /** Argument value before PMPI call */
    os << "group=" << dd.group << ", ";
    /** Argument value after PMPI call */
    os << "newcomm=" << dd.newcomm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_split operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_comm_split &dd)
{
  os << "dumpi_comm_split(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "oldcomm=" << dd.oldcomm << ", ";
    /** Argument value before PMPI call */
    os << "color=" << dd.color << ", ";
    /** Argument value before PMPI call */
    os << "key=" << dd.key << ", ";
    /** Argument value after PMPI call */
    os << "newcomm=" << dd.newcomm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_free operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_comm_free &dd)
{
  os << "dumpi_comm_free(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_test_inter operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_comm_test_inter &dd)
{
  os << "dumpi_comm_test_inter(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "inter=" << dd.inter << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_remote_size operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_comm_remote_size &dd)
{
  os << "dumpi_comm_remote_size(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "size=" << dd.size << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_remote_group operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_comm_remote_group &dd)
{
  os << "dumpi_comm_remote_group(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "group=" << dd.group << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a intercomm_create operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_intercomm_create &dd)
{
  os << "dumpi_intercomm_create(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "localcomm=" << dd.localcomm << ", ";
    /** Argument value before PMPI call */
    os << "localleader=" << dd.localleader << ", ";
    /** Argument value before PMPI call */
    os << "remotecomm=" << dd.remotecomm << ", ";
    /** Argument value before PMPI call */
    os << "remoteleader=" << dd.remoteleader << ", ";
    /** Argument value before PMPI call */
    os << "tag=" << dd.tag << ", ";
    /** Argument value after PMPI call */
    os << "newcomm=" << dd.newcomm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a intercomm_merge operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_intercomm_merge &dd)
{
  os << "dumpi_intercomm_merge(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "high=" << dd.high << ", ";
    /** Argument value after PMPI call */
    os << "newcomm=" << dd.newcomm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a keyval_create operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_keyval_create &dd)
{
  os << "dumpi_keyval_create(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "key=" << dd.key << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a keyval_free operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_keyval_free &dd)
{
  os << "dumpi_keyval_free(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "key=" << dd.key << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a attr_put operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_attr_put &dd)
{
  os << "dumpi_attr_put(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "key=" << dd.key << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a attr_get operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_attr_get &dd)
{
  os << "dumpi_attr_get(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "key=" << dd.key << ", ";
    /** Argument value after PMPI call */
    os << "flag=" << dd.flag << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a attr_delete operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_attr_delete &dd)
{
  os << "dumpi_attr_delete(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "key=" << dd.key << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a topo_test operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_topo_test &dd)
{
  os << "dumpi_topo_test(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "topo=" << dd.topo << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a cart_create operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_cart_create &dd)
{
  os << "dumpi_cart_create(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "oldcomm=" << dd.oldcomm << ", ";
    /** Argument value before PMPI call */
    os << "ndim=" << dd.ndim << ", ";
    /** Argument value before PMPI call.  Array of length [ndim] */
    os << "dims=" << P::ints(dd.ndim, dd.dims) << ", ";
    /** Argument value before PMPI call.  Array of length [ndim] */
    os << "periods=" << P::ints(dd.ndim, dd.periods) << ", ";
    /** Argument value before PMPI call */
    os << "reorder=" << dd.reorder << ", ";
    /** Argument value after PMPI call */
    os << "newcomm=" << dd.newcomm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a dims_create operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_dims_create &dd)
{
  os << "dumpi_dims_create(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "nodes=" << dd.nodes << ", ";
    /** Argument value before PMPI call */
    os << "ndim=" << dd.ndim << ", ";
    /** Argument value before and after PMPI call. Array of lenght [ndim] */
    os << "dims={in=" << P::ints(dd.ndim, dd.dims.in) << ", out="
       << P::ints(dd.ndim, dd.dims.out) << "})";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a graph_create operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_graph_create &dd)
{
  os << "dumpi_graph_create(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "numedges=" << dd.numedges << ", ";
    /** Argument value before PMPI call */
    os << "oldcomm=" << dd.oldcomm << ", ";
    /** Argument value before PMPI call */
    os << "nodes=" << dd.nodes << ", ";
    /** Argument value before PMPI call.  Array of length [nodes] */
    os << "index=" << P::ints(dd.nodes, dd.index) << ", ";
    /** Argument value before PMPI call.  Array of length [numedges] */
    os << "edges=" << P::ints(dd.numedges, dd.edges) << ", ";
    /** Argument value before PMPI call */
    os << "reorder=" << dd.reorder << ", ";
    /** Argument value after PMPI call */
    os << "newcomm=" << dd.newcomm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a graphdims_get operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_graphdims_get &dd)
{
  os << "dumpi_graphdims_get(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "nodes=" << dd.nodes << ", ";
    /** Argument value after PMPI call */
    os << "edges=" << dd.edges << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a graph_get operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_graph_get &dd)
{
  os << "dumpi_graph_get(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "totedges=" << dd.totedges << ", ";
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "totnodes=" << dd.totnodes << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "maxindex=" << dd.maxindex << ", ";
    /** Argument value before PMPI call */
    os << "maxedges=" << dd.maxedges << ", ";
    /** Argument value after PMPI call.  Array of length [totnodes] */
    os << "index=" << P::ints(dd.totnodes, dd.index) << ", ";
    /** Argument value after PMPI call.  Array of length [totedges] */
    os << "edges=" << P::ints(dd.totnodes, dd.edges) << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a cartdim_get operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_cartdim_get &dd)
{
  os << "dumpi_cartdim_get(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "ndim=" << dd.ndim << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a cart_get operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_cart_get &dd)
{
  os << "dumpi_cart_get(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "ndim=" << dd.ndim << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "maxdims=" << dd.maxdims << ", ";
    /** Argument value after PMPI call.  Array of length [ndim] */
    os << "dims=" << P::ints(dd.ndim, dd.dims) << ", ";
    /** Argument value after PMPI call.  Array of length [ndim] */
    os << "periods=" << P::ints(dd.ndim, dd.periods) << ", ";
    /** Argument value after PMPI call */
    os << "coords=" << dd.coords << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a cart_rank operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_cart_rank &dd)
{
  os << "dumpi_cart_rank(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "ndim=" << dd.ndim << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call.  Array of length [ndim] */
    os << "coords=" << P::ints(dd.ndim, dd.coords) << ", ";
    /** Argument value after PMPI call */
    os << "rank=" << dd.rank << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a cart_coords operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_cart_coords &dd)
{
  os << "dumpi_cart_coords(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "ndim=" << dd.ndim << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "rank=" << dd.rank << ", ";
    /** Argument value before PMPI call */
    os << "maxdims=" << dd.maxdims << ", ";
    /** Argument value after PMPI call.  Array of length [ndim] */
    os << "coords=" << P::ints(dd.ndim, dd.coords) << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a graph_neighbors_count operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_graph_neighbors_count &dd)
{
  os << "dumpi_graph_neighbors_count(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "rank=" << dd.rank << ", ";
    /** Argument value after PMPI call */
    os << "nneigh=" << dd.nneigh << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a graph_neighbors operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_graph_neighbors &dd)
{
  os << "dumpi_graph_neighbors(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "nneigh=" << dd.nneigh << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "rank=" << dd.rank << ", ";
    /** Argument value before PMPI call */
    os << "maxneighbors=" << dd.maxneighbors << ", ";
    /** Argument value after PMPI call.  Array of length [nneigh] */
    os << "neighbors=" << P::ints(dd.nneigh, dd.neighbors) << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a cart_shift operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_cart_shift &dd)
{
  os << "dumpi_cart_shift(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "direction=" << dd.direction << ", ";
    /** Argument value before PMPI call */
    os << "displ=" << dd.displ << ", ";
    /** Argument value after PMPI call */
    os << "source=" << dd.source << ", ";
    /** Argument value after PMPI call */
    os << "dest=" << dd.dest << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a cart_sub operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_cart_sub &dd)
{
  os << "dumpi_cart_sub(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "ndim=" << dd.ndim << ", ";
    /** Argument value before PMPI call */
    os << "oldcomm=" << dd.oldcomm << ", ";
    /** Argument value before PMPI call.  Array of length [ndim] */
    os << "remain_dims=" << P::ints(dd.ndim, dd.remain_dims) << ", ";
    /** Argument value after PMPI call */
    os << "newcomm=" << dd.newcomm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a cart_map operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_cart_map &dd)
{
  os << "dumpi_cart_map(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "ndim=" << dd.ndim << ", ";
    /** Argument value before PMPI call.  Array of length [ndim] */
    os << "dims=" << P::ints(dd.ndim, dd.dims) << ", ";
    /** Argument value before PMPI call.  Array of length [ndim] */
    os << "period=" << P::ints(dd.ndim, dd.period) << ", ";
    /** Argument value after PMPI call */
    os << "newrank=" << dd.newrank << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a graph_map operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_graph_map &dd)
{
  os << "dumpi_graph_map(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "numedges=" << dd.numedges << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "nodes=" << dd.nodes << ", ";
    /** Argument value before PMPI call.  Array of length [nodes] */
    os << "index=" << P::ints(dd.nodes, dd.index) << ", ";
    /** Argument value before PMPI call.  Array of length [numedges] */
    os << "edges=" << P::ints(dd.numedges, dd.edges) << ", ";
    /** Argument value after PMPI call */
    os << "newrank=" << dd.newrank << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a get_processor_name operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_get_processor_name &dd)
{
  os << "dumpi_get_processor_name(";
  if(&dd) {
    /** Argument value after PMPI call.  Array of length [*resultlen] */
    os << "name=\"" << dd.name << "\", ";
    /** Argument value after PMPI call */
    os << "resultlen=" << dd.resultlen << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a get_version operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_get_version &dd)
{
  os << "dumpi_get_version(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "version=" << dd.version << ", ";
    /** Argument value after PMPI call */
    os << "subversion=" << dd.subversion << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a errhandler_create operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_errhandler_create &dd)
{
  os << "dumpi_errhandler_create(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "errhandler=" << dd.errhandler << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a errhandler_set operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_errhandler_set &dd)
{
  os << "dumpi_errhandler_set(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "errhandler=" << dd.errhandler << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a errhandler_get operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_errhandler_get &dd)
{
  os << "dumpi_errhandler_get(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "errhandler=" << dd.errhandler << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a errhandler_free operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_errhandler_free &dd)
{
  os << "dumpi_errhandler_free(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "errhandler=" << dd.errhandler << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a error_string operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_error_string &dd)
{
  os << "dumpi_error_string(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "errorcode=" << dd.errorcode << ", ";
    /** Argument value after PMPI call.  Array of length [*resultlen] */
    os << "errorstring=\"" << dd.errorstring << "\", ";
    /** Argument value after PMPI call */
    os << "resultlen=" << dd.resultlen << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a error_class operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_error_class &dd)
{
  os << "dumpi_error_class(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "errorcode=" << dd.errorcode << ", ";
    /** Argument value after PMPI call */
    os << "errorclass=" << dd.errorclass << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a wtime operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_wtime &dd)
{
  os << "dumpi_wtime(";
  if(&dd) {
    os << "psec=" << dd.psec << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a wtick operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_wtick &dd)
{
  os << "dumpi_wtick(";
  if(&dd) {
    os << "psec=" << dd.psec << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a init operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_init &dd)
{
  os << "dumpi_init(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "argc=" << dd.argc << ", ";
    /**
     * Argument value before PMPI call.
     * Array of length [argc] containing NUL-terminated std::strings.
     */
    os << "argv=" << P::stringarray(dd.argc, dd.argv) << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a finalize operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_finalize &dd)
{
  os << "dumpi_finalize(";
  if(&dd) {
    os << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a initialized operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_initialized &dd)
{
  os << "dumpi_initialized(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "result=" << dd.result << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a abort operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_abort &dd)
{
  os << "dumpi_abort(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "errorcode=" << dd.errorcode << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a close_port operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_close_port &dd)
{
  os << "dumpi_close_port(";
  if(&dd) {
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "portname=\"" << dd.portname << "\")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_accept operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_comm_accept &dd)
{
  os << "dumpi_comm_accept(";
  if(&dd) {
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "portname=\"" << dd.portname << "\", ";
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ", ";
    /** Argument value before PMPI call */
    os << "root=" << dd.root << ", ";
    /** Argument value before PMPI call */
    os << "oldcomm=" << dd.oldcomm << ", ";
    /** Argument value after PMPI call */
    os << "newcomm=" << dd.newcomm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_connect operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_comm_connect &dd)
{
  os << "dumpi_comm_connect(";
  if(&dd) {
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "portname=\"" << dd.portname << "\", ";
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ", ";
    /** Argument value before PMPI call */
    os << "root=" << dd.root << ", ";
    /** Argument value before PMPI call */
    os << "oldcomm=" << dd.oldcomm << ", ";
    /** Argument value after PMPI call */
    os << "newcomm=" << dd.newcomm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_disconnect operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_comm_disconnect &dd)
{
  os << "dumpi_comm_disconnect(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_get_parent operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_comm_get_parent &dd)
{
  os << "dumpi_comm_get_parent(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "parent=" << dd.parent << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_join operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_comm_join &dd)
{
  os << "dumpi_comm_join(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "fd=" << dd.fd << ", ";
    /** Argument value after PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_spawn operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_comm_spawn &dd)
{
  os << "dumpi_comm_spawn(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "oldcommrank=" << dd.oldcommrank << ", ";
    /**
     * Argument value before PMPI call.  NUL-terminated std::string.
     * Only stored if(oldcommrank==root)
     */
    os << "command=\"" << dd.command << "\", ";
    /**
     * Argument value before PMPI call.
     * Null-terminated array of containing NUL-terminated std::strings.
     * Only stored if(oldcommrank==root)
     */
    os << "argv="
       << P::stringarraynull(dd.oldcommrank == dd.root, dd.argv) << ", ";
    /** Argument value before PMPI call.  Only stored if(oldcommrank==root) */
    os << "maxprocs=" << dd.maxprocs << ", ";
    /** Argument value before PMPI call.  Only stored if(oldcommrank==root) */
    os << "info=" << dd.info << ", ";
    /** Argument value before PMPI call */
    os << "root=" << dd.root << ", ";
    /** Argument value before PMPI call */
    os << "oldcomm=" << dd.oldcomm << ", ";
    /** Argument value after PMPI call */
    os << "newcomm=" << dd.newcomm << ", ";
    /**
     * Argument value after PMPI call.  Array of length [maxprocs].
     * Only stored if(oldcommrank==root)
     */
    os << "errcodes="
       << P::ints(dd.oldcommrank == dd.root, dd.maxprocs, dd.errcodes)
       << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_spawn_multiple operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_comm_spawn_multiple &dd)
{
  os << "dumpi_comm_spawn_multiple(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "totprocs=" << dd.totprocs << ", ";
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "oldcommrank=" << dd.oldcommrank << ", ";
    /** Argument value before PMPI call.  Only stored if(oldcommrank==root) */
    os << "count=" << dd.count << ", ";
    /**
     * Argument value before PMPI call.
     * Array of length [count] containing NUL-terminated std::strings.
     * Only stored if(oldcommrank==root)
     */
    os << "commands="
       << P::stringarray(dd.oldcommrank == dd.root, dd.count, dd.commands)
       << ", ";
    /**
     * Argument value before PMPI call.
     * 3D-array of length [count], with second-dimension NULL-terminated,
     * and third dimension NUL-terminated strings (MPI_Comm_spawn_multiple
     * is completely insane).
     * Only stored if(oldcommrank==root)
     */
    os << "argvs="
       << P::stringarraynull2d(dd.oldcommrank==dd.root,dd.count,dd.argvs)
       << ", ";
    /**
     * Argument value before PMPI call.  Array of length [count].
     * Only stored if(oldcommrank==root)
     */
    os << "maxprocs="
       << P::ints(dd.oldcommrank == dd.root, dd.count, dd.maxprocs)
       << ", ";
    /**
     * Argument value before PMPI call.  Array of length [count].
     * Only stored if(oldcommrank==root)
     */
    os << "info="
       << P::ints(dd.oldcommrank == dd.root, dd.count, dd.info) << ", ";
    /** Argument value before PMPI call */
    os << "root=" << dd.root << ", ";
    /** Argument value before PMPI call */
    os << "oldcomm=" << dd.oldcomm << ", ";
    /** Argument value after PMPI call */
    os << "newcomm=" << dd.newcomm << ", ";
    /**
     * Argument value after PMPI call.  Array of length [totprocs].
     * Only stored if(oldcommrank==root)
     */
    os << "errcodes="
       << P::ints(dd.oldcommrank==dd.root, dd.totprocs, dd.errcodes)
       << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a lookup_name operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_lookup_name &dd)
{
  os << "dumpi_lookup_name(";
  if(&dd) {
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "servicename=\"" << dd.servicename << "\", ";
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ", ";
    /** Argument value after PMPI call.  NUL-terminated std::string. */
    os << "portname=\"" << dd.portname << "\")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a open_port operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_open_port &dd)
{
  os << "dumpi_open_port(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ", ";
    /** Argument value after PMPI call.  NUL-terminated std::string */
    os << "portname=\"" << dd.portname << "\")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a publish_name operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_publish_name &dd)
{
  os << "dumpi_publish_name(";
  if(&dd) {
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "servicename=\"" << dd.servicename << "\", ";
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ", ";
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "portname=\"" << dd.portname << "\")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a unpublish_name operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_unpublish_name &dd)
{
  os << "dumpi_unpublish_name(";
  if(&dd) {
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "servicename=\"" << dd.servicename << "\", ";
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ", ";
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "portname=\"" << dd.portname << "\")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a accumulate operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_accumulate &dd)
{
  os << "dumpi_accumulate(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "origincount=" << dd.origincount << ", ";
    /** Argument value before PMPI call */
    os << "origintype=" << dd.origintype << ", ";
    /** Argument value before PMPI call */
    os << "targetrank=" << dd.targetrank << ", ";
    /** Argument value before PMPI call */
    os << "targetdisp=" << dd.targetdisp << ", ";
    /** Argument value before PMPI call */
    os << "targetcount=" << dd.targetcount << ", ";
    /** Argument value before PMPI call */
    os << "targettype=" << dd.targettype << ", ";
    /** Argument value before PMPI call */
    os << "op=" << dd.op << ", ";
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a get operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_get &dd)
{
  os << "dumpi_get(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "origincount=" << dd.origincount << ", ";
    /** Argument value before PMPI call */
    os << "origintype=" << dd.origintype << ", ";
    /** Argument value before PMPI call */
    os << "targetrank=" << dd.targetrank << ", ";
    /** Argument value before PMPI call */
    os << "targetdisp=" << dd.targetdisp << ", ";
    /** Argument value before PMPI call */
    os << "targetcount=" << dd.targetcount << ", ";
    /** Argument value before PMPI call */
    os << "targettype=" << dd.targettype << ", ";
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a put operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_put &dd)
{
  os << "dumpi_put(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "origincount=" << dd.origincount << ", ";
    /** Argument value before PMPI call */
    os << "origintype=" << dd.origintype << ", ";
    /** Argument value before PMPI call */
    os << "targetrank=" << dd.targetrank << ", ";
    /** Argument value before PMPI call */
    os << "targetdisp=" << dd.targetdisp << ", ";
    /** Argument value before PMPI call */
    os << "targetcount=" << dd.targetcount << ", ";
    /** Argument value before PMPI call */
    os << "targettype=" << dd.targettype << ", ";
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_complete operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_win_complete &dd)
{
  os << "dumpi_win_complete(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_create operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_win_create &dd)
{
  os << "dumpi_win_create(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "size=" << dd.size << ", ";
    /** Argument value before PMPI call */
    os << "dispunit=" << dd.dispunit << ", ";
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "win=" << dd.win << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_fence operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_win_fence &dd)
{
  os << "dumpi_win_fence(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "assertion=" << dd.assertion << ", ";
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_free operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_win_free &dd)
{
  os << "dumpi_win_free(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_get_group operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_win_get_group &dd)
{
  os << "dumpi_win_get_group(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ", ";
    /** Argument value after PMPI call */
    os << "group=" << dd.group << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_lock operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_win_lock &dd)
{
  os << "dumpi_win_lock(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "locktype=" << dd.locktype << ", ";
    /** Argument value before PMPI call */
    os << "winrank=" << dd.winrank << ", ";
    /** Argument value before PMPI call */
    os << "assertion=" << dd.assertion << ", ";
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_post operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_win_post &dd)
{
  os << "dumpi_win_post(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "group=" << dd.group << ", ";
    /** Argument value before PMPI call */
    os << "assertion=" << dd.assertion << ", ";
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_start operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_win_start &dd)
{
  os << "dumpi_win_start(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "group=" << dd.group << ", ";
    /** Argument value before PMPI call */
    os << "assertion=" << dd.assertion << ", ";
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_test operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_win_test &dd)
{
  os << "dumpi_win_test(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ", ";
    /** Argument value after PMPI call */
    os << "flag=" << dd.flag << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_unlock operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_win_unlock &dd)
{
  os << "dumpi_win_unlock(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "winrank=" << dd.winrank << ", ";
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_wait operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_win_wait &dd)
{
  os << "dumpi_win_wait(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a alltoallw operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_alltoallw &dd)
{
  os << "dumpi_alltoallw(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "commsize=" << dd.commsize << ", ";
    /** Argument value before PMPI call.  Array of length [commsize] */
    os << "sendcounts=" << P::ints(dd.commsize, dd.sendcounts) << ", ";
    /** Argument value before PMPI call.  Array of length [commsize] */
    os << "senddispls=" << P::ints(dd.commsize, dd.senddispls) << ", ";
    /** Argument value before PMPI call.  Array of length [commsize] */
    os << "sendtypes=" << P::ints(dd.commsize, dd.sendtypes) << ", ";
    /** Argument value before PMPI call.  Array of length [commsize] */
    os << "recvcounts=" << P::ints(dd.commsize, dd.recvcounts) << ", ";
    /** Argument value before PMPI call.  Array of length [commsize] */
    os << "recvdispls=" << P::ints(dd.commsize, dd.recvdispls) << ", ";
    /** Argument value before PMPI call.  Array of length [commsize] */
    os << "recvtypes=" << P::ints(dd.commsize, dd.recvtypes) << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a exscan operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_exscan &dd)
{
  os << "dumpi_exscan(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "op=" << dd.op << ", ";
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a add_error_class operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_add_error_class &dd)
{
  os << "dumpi_add_error_class(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "errorclass=" << dd.errorclass << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a add_error_code operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_add_error_code &dd)
{
  os << "dumpi_add_error_code(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "errorclass=" << dd.errorclass << ", ";
    /** Argument value after PMPI call */
    os << "errorcode=" << dd.errorcode << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a add_error_string operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_add_error_string &dd)
{
  os << "dumpi_add_error_string(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "errorcode=" << dd.errorcode << ", ";
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "errorstring=\"" << dd.errorstring << "\")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_call_errhandler operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_comm_call_errhandler &dd)
{
  os << "dumpi_comm_call_errhandler(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "errorcode=" << dd.errorcode << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_create_keyval operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_comm_create_keyval &dd)
{
  os << "dumpi_comm_create_keyval(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "keyval=" << dd.keyval << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_delete_attr operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_comm_delete_attr &dd)
{
  os << "dumpi_comm_delete_attr(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "keyval=" << dd.keyval << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_free_keyval operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_comm_free_keyval &dd)
{
  os << "dumpi_comm_free_keyval(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "keyval=" << dd.keyval << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_get_attr operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_comm_get_attr &dd)
{
  os << "dumpi_comm_get_attr(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "keyval=" << dd.keyval << ", ";
    /** Argument value after PMPI call */
    os << "flag=" << dd.flag << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_get_name operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_comm_get_name &dd)
{
  os << "dumpi_comm_get_name(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call.  Array of length [*resultlen] */
    os << "name=\"" << dd.name << "\", ";
    /** Argument value after PMPI call */
    os << "resultlen=" << dd.resultlen << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_set_attr operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_comm_set_attr &dd)
{
  os << "dumpi_comm_set_attr(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "keyval=" << dd.keyval << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_set_name operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_comm_set_name &dd)
{
  os << "dumpi_comm_set_name(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "name=\"" << dd.name << "\")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_call_errhandler operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_call_errhandler &dd)
{
  os << "dumpi_file_call_errhandler(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "errorcode=" << dd.errorcode << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a grequest_complete operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_grequest_complete &dd)
{
  os << "dumpi_grequest_complete(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a grequest_start operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_grequest_start &dd)
{
  os << "dumpi_grequest_start(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a init_thread operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_init_thread &dd)
{
  os << "dumpi_init_thread(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "argc=" << dd.argc << ", ";
    /**
     * Argument value before PMPI call.
     * Array of length [argc] containing NUL-terminated std::strings.
     */
    os << "argv=" << P::stringarray(dd.argc, dd.argv) << ", ";
    /** Argument value before PMPI call */
    os << "required=" << dd.required << ", ";
    /** Argument value after PMPI call */
    os << "provided=" << dd.provided << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a is_thread_main operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_is_thread_main &dd)
{
  os << "dumpi_is_thread_main(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "flag=" << dd.flag << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a query_thread operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_query_thread &dd)
{
  os << "dumpi_query_thread(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "supported=" << dd.supported << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a status_set_cancelled operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_status_set_cancelled &dd)
{
  os << "dumpi_status_set_cancelled(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "status=" << *dd.status << ", ";
    /** Argument value before PMPI call */
    os << "flag=" << dd.flag << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a status_set_elements operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_status_set_elements &dd)
{
  os << "dumpi_status_set_elements(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "status=" << *dd.status << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_create_keyval operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_create_keyval &dd)
{
  os << "dumpi_type_create_keyval(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "keyval=" << dd.keyval << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_delete_attr operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_delete_attr &dd)
{
  os << "dumpi_type_delete_attr(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "keyval=" << dd.keyval << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_dup operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_type_dup &dd)
{
  os << "dumpi_type_dup(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "oldtype=" << dd.oldtype << ", ";
    /** Argument value after PMPI call */
    os << "newtype=" << dd.newtype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_free_keyval operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_free_keyval &dd)
{
  os << "dumpi_type_free_keyval(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "keyval=" << dd.keyval << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_get_attr operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_get_attr &dd)
{
  os << "dumpi_type_get_attr(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "keyval=" << dd.keyval << ", ";
    /** Argument value after PMPI call */
    os << "flag=" << dd.flag << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_get_contents operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_get_contents &dd)
{
  os << "dumpi_type_get_contents(";
  if(&dd) {
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "numdatatypes=" << dd.numdatatypes << ", ";
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "numaddresses=" << dd.numaddresses << ", ";
    /** Not an MPI argument.  Added to index relevant data in the struct. */
    os << "numintegers=" << dd.numintegers << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "maxintegers=" << dd.maxintegers << ", ";
    /** Argument value before PMPI call */
    os << "maxaddresses=" << dd.maxaddresses << ", ";
    /** Argument value before PMPI call */
    os << "maxdatatypes=" << dd.maxdatatypes << ", ";
    /** Argument value after PMPI call.  Array of length [numintegers] */
    os << "arrintegers=" << P::ints(dd.numintegers, dd.arrintegers)
       << ", ";
    /** Argument value after PMPI call.  Array of length [numaddresses] */
    os << "arraddresses=" << P::ints(dd.numaddresses, dd.arraddresses)
       << ", ";
    /** Argument value after PMPI call.  Array of length [numdatatypes] */
    os << "arrdatatypes=" << P::ints(dd.numdatatypes, dd.arrdatatypes)
       << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_get_envelope operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_get_envelope &dd)
{
  os << "dumpi_type_get_envelope(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "numintegers=" << dd.numintegers << ", ";
    /** Argument value after PMPI call */
    os << "numaddresses=" << dd.numaddresses << ", ";
    /** Argument value after PMPI call */
    os << "numdatatypes=" << dd.numdatatypes << ", ";
    /** Argument value after PMPI call */
    os << "combiner=" << dd.combiner << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_get_name operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_get_name &dd)
{
  os << "dumpi_type_get_name(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call.  NUL-terminated std::string. */
    os << "name=\"" << dd.name << "\", ";
    /** Argument value after PMPI call */
    os << "resultlen=" << dd.resultlen << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_set_attr operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_set_attr &dd)
{
  os << "dumpi_type_set_attr(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call */
    os << "keyval=" << dd.keyval << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_set_name operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_set_name &dd)
{
  os << "dumpi_type_set_name(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "name=\"" << dd.name << "\")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_match_size operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_match_size &dd)
{
  os << "dumpi_type_match_size(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "typeclass=" << dd.typeclass << ", ";
    /** Argument value before PMPI call */
    os << "size=" << dd.size << ", ";
    /** Argument value after PMPI call */
    os << "datatype=" << dd.datatype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_call_errhandler operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_win_call_errhandler &dd)
{
  os << "dumpi_win_call_errhandler(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ", ";
    /** Argument value before PMPI call */
    os << "errorcode=" << dd.errorcode << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_create_keyval operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_win_create_keyval &dd)
{
  os << "dumpi_win_create_keyval(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "keyval=" << dd.keyval << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_delete_attr operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_win_delete_attr &dd)
{
  os << "dumpi_win_delete_attr(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ", ";
    /** Argument value before PMPI call */
    os << "keyval=" << dd.keyval << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_free_keyval operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_win_free_keyval &dd)
{
  os << "dumpi_win_free_keyval(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "keyval=" << dd.keyval << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_get_attr operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_win_get_attr &dd)
{
  os << "dumpi_win_get_attr(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ", ";
    /** Argument value before PMPI call */
    os << "keyval=" << dd.keyval << ", ";
    /** Argument value after PMPI call */
    os << "flag=" << dd.flag << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_get_name operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_win_get_name &dd)
{
  os << "dumpi_win_get_name(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ", ";
    /** Argument value after PMPI call.  NUL-terminated std::string. */
    os << "name=\"" << dd.name << "\", ";
    /** Argument value after PMPI call */
    os << "resultlen=" << dd.resultlen << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_set_attr operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_win_set_attr &dd)
{
  os << "dumpi_win_set_attr(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ", ";
    /** Argument value before PMPI call */
    os << "keyval=" << dd.keyval << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_set_name operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_win_set_name &dd)
{
  os << "dumpi_win_set_name(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ", ";
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "name=\"" << dd.name << "\")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a alloc_mem operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_alloc_mem &dd)
{
  os << "dumpi_alloc_mem(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "size=" << dd.size << ", ";
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_create_errhandler operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_comm_create_errhandler &dd)
{
  os << "dumpi_comm_create_errhandler(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "errhandler=" << dd.errhandler << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_get_errhandler operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_comm_get_errhandler &dd)
{
  os << "dumpi_comm_get_errhandler(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value after PMPI call */
    os << "errhandler=" << dd.errhandler << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a comm_set_errhandler operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_comm_set_errhandler &dd)
{
  os << "dumpi_comm_set_errhandler(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call */
    os << "errhandler=" << dd.errhandler << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_create_errhandler operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_create_errhandler &dd)
{
  os << "dumpi_file_create_errhandler(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "errhandler=" << dd.errhandler << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_get_errhandler operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_get_errhandler &dd)
{
  os << "dumpi_file_get_errhandler(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value after PMPI call */
    os << "errhandler=" << dd.errhandler << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_set_errhandler operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_set_errhandler &dd)
{
  os << "dumpi_file_set_errhandler(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "errhandler=" << dd.errhandler << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a finalized operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_finalized &dd)
{
  os << "dumpi_finalized(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "flag=" << dd.flag << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a free_mem operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_free_mem &dd)
{
  os << "dumpi_free_mem(";
  if(&dd) {
    os << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a get_address operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_get_address &dd)
{
  os << "dumpi_get_address(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "address=" << dd.address << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a info_create operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_info_create &dd)
{
  os << "dumpi_info_create(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "info=" << dd.info << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a info_delete operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_info_delete &dd)
{
  os << "dumpi_info_delete(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ", ";
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "key=\"" << dd.key << "\")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a info_dup operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_info_dup &dd)
{
  os << "dumpi_info_dup(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "oldinfo=" << dd.oldinfo << ", ";
    /** Argument value after PMPI call */
    os << "newinfo=" << dd.newinfo << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a info_free operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_info_free &dd)
{
  os << "dumpi_info_free(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a info_get operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_info_get &dd)
{
  os << "dumpi_info_get(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ", ";
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "key=\"" << dd.key << "\", ";
    /** Argument value before PMPI call */
    os << "valuelength=" << dd.valuelength << ", ";
    /** Argument value after PMPI call.  NUL-terminated std::string. */
    os << "value=\"" << dd.value << "\", ";
    /** Argument value after PMPI call */
    os << "flag=" << dd.flag << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a info_get_nkeys operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_info_get_nkeys &dd)
{
  os << "dumpi_info_get_nkeys(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ", ";
    /** Argument value after PMPI call */
    os << "nkeys=" << dd.nkeys << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a info_get_nthkey operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_info_get_nthkey &dd)
{
  os << "dumpi_info_get_nthkey(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ", ";
    /** Argument value before PMPI call */
    os << "n=" << dd.n << ", ";
    /** Argument value after PMPI call.  NUL-terminated std::string */
    os << "key=\"" << dd.key << "\")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a info_get_valuelen operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_info_get_valuelen &dd)
{
  os << "dumpi_info_get_valuelen(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ", ";
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "key=\"" << dd.key << "\", ";
    /** Argument value after PMPI call */
    os << "valuelen=" << dd.valuelen << ", ";
    /** Argument value after PMPI call */
    os << "flag=" << dd.flag << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a info_set operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_info_set &dd)
{
  os << "dumpi_info_set(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ", ";
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "key=\"" << dd.key << "\", ";
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "value=\"" << dd.value << "\")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a pack_external operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_pack_external &dd)
{
  os << "dumpi_pack_external(";
  if(&dd) {
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "datarep=\"" << dd.datarep << "\", ";
    /** Argument value before PMPI call */
    os << "incount=" << dd.incount << ", ";
    /** Argument value before PMPI call */
    os << "intype=" << dd.intype << ", ";
    /** Argument value before PMPI call */
    os << "outcount=" << dd.outcount << ", ";
    /** Argument value before and after PMPI call. */
    os << "position={in=" << dd.position.in << ", out="
       << dd.position.out << "})";
  }
  else {
    os << "NULL)";
  }
  return os;
}

/**
 * C++ IO for a pack_external_size operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_pack_external_size &dd)
{
  os << "dumpi_pack_external_size(";
  if(&dd) {
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "datarep=\"" << dd.datarep << "\", ";
    /** Argument value before PMPI call */
    os << "incount=" << dd.incount << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "size=" << dd.size << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a request_get_status operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_request_get_status &dd)
{
  os << "dumpi_request_get_status(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "request=" << dd.request << ", ";
    /** Argument value after PMPI call */
    os << "flag=" << dd.flag << ", ";
    /** Argument value before PMPI call.  Only stored if(flag!=0) */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_create_darray operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_create_darray &dd)
{
  os << "dumpi_type_create_darray(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "size=" << dd.size << ", ";
    /** Argument value before PMPI call */
    os << "rank=" << dd.rank << ", ";
    /** Argument value before PMPI call */
    os << "ndims=" << dd.ndims << ", ";
    /** Argument value before PMPI call.  Array of length [ndims] */
    os << "gsizes=" << P::ints(dd.ndims, dd.gsizes) << ", ";
    /** Argument value before PMPI call.  Array of length [ndims] */
    os << "distribs=" << P::ints(dd.ndims, dd.distribs) << ", ";
    /** Argument value before PMPI call.  Array of length [ndims] */
    os << "dargs=" << P::ints(dd.ndims, dd.dargs) << ", ";
    /** Argument value before PMPI call.  Array of length [ndims] */
    os << "psizes=" << P::ints(dd.ndims, dd.psizes) << ", ";
    /** Argument value before PMPI call */
    os << "order=" << dd.order << ", ";
    /** Argument value before PMPI call */
    os << "oldtype=" << dd.oldtype << ", ";
    /** Argument value after PMPI call */
    os << "newtype=" << dd.newtype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_create_hindexed operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_create_hindexed &dd)
{
  os << "dumpi_type_create_hindexed(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "blocklengths=" << P::ints(dd.count, dd.blocklengths) << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "displacements=" << P::ints(dd.count, dd.displacements)
       << ", ";
    /** Argument value before PMPI call */
    os << "oldtype=" << dd.oldtype << ", ";
    /** Argument value after PMPI call */
    os << "newtype=" << dd.newtype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_create_hvector operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_create_hvector &dd)
{
  os << "dumpi_type_create_hvector(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "blocklength=" << dd.blocklength << ", ";
    /** Argument value before PMPI call */
    os << "stride=" << dd.stride << ", ";
    /** Argument value before PMPI call */
    os << "oldtype=" << dd.oldtype << ", ";
    /** Argument value after PMPI call */
    os << "newtype=" << dd.newtype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_create_indexed_block operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_create_indexed_block &dd)
{
  os << "dumpi_type_create_indexed_block(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "blocklength=" << dd.blocklength << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "displacments=" << P::ints(dd.count, dd.displacments) << ", ";
    /** Argument value before PMPI call */
    os << "oldtype=" << dd.oldtype << ", ";
    /** Argument value after PMPI call */
    os << "newtype=" << dd.newtype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_create_resized operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_create_resized &dd)
{
  os << "dumpi_type_create_resized(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "oldtype=" << dd.oldtype << ", ";
    /** Argument value before PMPI call */
    os << "lb=" << dd.lb << ", ";
    /** Argument value before PMPI call */
    os << "extent=" << dd.extent << ", ";
    /** Argument value after PMPI call */
    os << "newtype=" << dd.newtype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_create_struct operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_create_struct &dd)
{
  os << "dumpi_type_create_struct(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "blocklengths=" << P::ints(dd.count, dd.blocklengths) << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "displacements=" << P::ints(dd.count, dd.displacements)<< ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "oldtypes=" << P::ints(dd.count, dd.oldtypes) << ", ";
    /** Argument value after PMPI call */
    os << "newtype=" << dd.newtype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_create_subarray operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_create_subarray &dd)
{
  os << "dumpi_type_create_subarray(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "ndims=" << dd.ndims << ", ";
    /** Argument value before PMPI call.  Array of length [ndims] */
    os << "sizes=" << P::ints(dd.ndims, dd.sizes) << ", ";
    /** Argument value before PMPI call.  Array of length [ndims] */
    os << "subsizes=" << P::ints(dd.ndims, dd.subsizes) << ", ";
    /** Argument value before PMPI call.  Array of length [ndims] */
    os << "starts=" << P::ints(dd.ndims, dd.starts) << ", ";
    /** Argument value before PMPI call */
    os << "order=" << dd.order << ", ";
    /** Argument value before PMPI call */
    os << "oldtype=" << dd.oldtype << ", ";
    /** Argument value after PMPI call */
    os << "newtype=" << dd.newtype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_get_extent operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_get_extent &dd)
{
  os << "dumpi_type_get_extent(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "lb=" << dd.lb << ", ";
    /** Argument value after PMPI call */
    os << "extent=" << dd.extent << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a type_get_true_extent operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_type_get_true_extent &dd)
{
  os << "dumpi_type_get_true_extent(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "lb=" << dd.lb << ", ";
    /** Argument value after PMPI call */
    os << "extent=" << dd.extent << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a unpack_external operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_unpack_external &dd)
{
  os << "dumpi_unpack_external(";
  if(&dd) {
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "datarep=\"" << dd.datarep << "\", ";
    /** Argument value before PMPI call */
    os << "insize=" << dd.insize << ", ";
    /** Argument value before and after PMPI call. */
    os << "position={in=" << dd.position.in << ", out="
       << dd.position.out << "}, ";
    /** Argument value before PMPI call */
    os << "outcount=" << dd.outcount << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_create_errhandler operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_win_create_errhandler &dd)
{
  os << "dumpi_win_create_errhandler(";
  if(&dd) {
    /** Argument value after PMPI call */
    os << "errhandler=" << dd.errhandler << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_get_errhandler operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_win_get_errhandler &dd)
{
  os << "dumpi_win_get_errhandler(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ", ";
    /** Argument value after PMPI call */
    os << "errhandler=" << dd.errhandler << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a win_set_errhandler operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_win_set_errhandler &dd)
{
  os << "dumpi_win_set_errhandler(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "win=" << dd.win << ", ";
    /** Argument value after PMPI call */
    os << "errhandler=" << dd.errhandler << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_open operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_file_open &dd)
{
  os << "dumpi_file_open(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "comm=" << dd.comm << ", ";
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "filename=\"" << dd.filename << "\", ";
    /** Argument value before PMPI call */
    os << "amode=" << dd.amode << ", ";
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ", ";
    /** Argument value after PMPI call */
    os << "file=" << dd.file << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_close operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_file_close &dd)
{
  os << "dumpi_file_close(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_delete operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_file_delete &dd)
{
  os << "dumpi_file_delete(";
  if(&dd) {
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "filename=\"" << dd.filename << "\", ";
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_set_size operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_set_size &dd)
{
  os << "dumpi_file_set_size(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "size=" << dd.size << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_preallocate operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_preallocate &dd)
{
  os << "dumpi_file_preallocate(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "size=" << dd.size << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_get_size operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_get_size &dd)
{
  os << "dumpi_file_get_size(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value after PMPI call */
    os << "size=" << dd.size << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_get_group operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_get_group &dd)
{
  os << "dumpi_file_get_group(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value after PMPI call */
    os << "group=" << dd.group << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_get_amode operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_get_amode &dd)
{
  os << "dumpi_file_get_amode(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value after PMPI call */
    os << "amode=" << dd.amode << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_set_info operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_set_info &dd)
{
  os << "dumpi_file_set_info(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_get_info operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_get_info &dd)
{
  os << "dumpi_file_get_info(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value after PMPI call */
    os << "info=" << dd.info << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_set_view operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_set_view &dd)
{
  os << "dumpi_file_set_view(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "offset=" << dd.offset << ", ";
    /** Argument value before PMPI call */
    os << "hosttype=" << dd.hosttype << ", ";
    /** Argument value before PMPI call */
    os << "filetype=" << dd.filetype << ", ";
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "datarep=\"" << dd.datarep << "\", ";
    /** Argument value before PMPI call */
    os << "info=" << dd.info << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_get_view operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_get_view &dd)
{
  os << "dumpi_file_get_view(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value after PMPI call */
    os << "offset=" << dd.offset << ", ";
    /** Argument value after PMPI call */
    os << "hosttype=" << dd.hosttype << ", ";
    /** Argument value after PMPI call */
    os << "filetype=" << dd.filetype << ", ";
    /** Argument value after PMPI call.  NUL-terminated std::string */
    os << "datarep=\"" << dd.datarep << "\")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_read_at operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_file_read_at &dd)
{
  os << "dumpi_file_read_at(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "offset=" << dd.offset << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_read_at_all operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_read_at_all &dd)
{
  os << "dumpi_file_read_at_all(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "offset=" << dd.offset << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_write_at operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_write_at &dd)
{
  os << "dumpi_file_write_at(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "offset=" << dd.offset << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_write_at_all operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_write_at_all &dd)
{
  os << "dumpi_file_write_at_all(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "offset=" << dd.offset << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_iread_at operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_iread_at &dd)
{
  os << "dumpi_file_iread_at(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "offset=" << dd.offset << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_iwrite_at operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_iwrite_at &dd)
{
  os << "dumpi_file_iwrite_at(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "offset=" << dd.offset << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_read operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_file_read &dd)
{
  os << "dumpi_file_read(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_read_all operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_read_all &dd)
{
  os << "dumpi_file_read_all(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_write operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_file_write &dd)
{
  os << "dumpi_file_write(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_write_all operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_write_all &dd)
{
  os << "dumpi_file_write_all(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_iread operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_file_iread &dd)
{
  os << "dumpi_file_iread(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_iwrite operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_file_iwrite &dd)
{
  os << "dumpi_file_iwrite(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_seek operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_file_seek &dd)
{
  os << "dumpi_file_seek(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "offset=" << dd.offset << ", ";
    /** Argument value before PMPI call */
    os << "whence=" << dd.whence << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_get_position operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_get_position &dd)
{
  os << "dumpi_file_get_position(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value after PMPI call */
    os << "offset=" << dd.offset << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_get_byte_offset operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_get_byte_offset &dd)
{
  os << "dumpi_file_get_byte_offset(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "offset=" << dd.offset << ", ";
    /** Argument value after PMPI call */
    os << "bytes=" << dd.bytes << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_read_shared operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_read_shared &dd)
{
  os << "dumpi_file_read_shared(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_write_shared operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_write_shared &dd)
{
  os << "dumpi_file_write_shared(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_iread_shared operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_iread_shared &dd)
{
  os << "dumpi_file_iread_shared(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_iwrite_shared operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_iwrite_shared &dd)
{
  os << "dumpi_file_iwrite_shared(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "request=" << dd.request << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_read_ordered operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_read_ordered &dd)
{
  os << "dumpi_file_read_ordered(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_write_ordered operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_write_ordered &dd)
{
  os << "dumpi_file_write_ordered(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_seek_shared operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_seek_shared &dd)
{
  os << "dumpi_file_seek_shared(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "offset=" << dd.offset << ", ";
    /** Argument value before PMPI call */
    os << "whence=" << dd.whence << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_get_position_shared operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_get_position_shared &dd)
{
  os << "dumpi_file_get_position_shared(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value after PMPI call */
    os << "offset=" << dd.offset << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_read_at_all_begin operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_read_at_all_begin &dd)
{
  os << "dumpi_file_read_at_all_begin(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "offset=" << dd.offset << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_read_at_all_end operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_read_at_all_end &dd)
{
  os << "dumpi_file_read_at_all_end(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_write_at_all_begin operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_write_at_all_begin &dd)
{
  os << "dumpi_file_write_at_all_begin(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "offset=" << dd.offset << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_write_at_all_end operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_write_at_all_end &dd)
{
  os << "dumpi_file_write_at_all_end(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_read_all_begin operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_read_all_begin &dd)
{
  os << "dumpi_file_read_all_begin(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_read_all_end operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_read_all_end &dd)
{
  os << "dumpi_file_read_all_end(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_write_all_begin operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_write_all_begin &dd)
{
  os << "dumpi_file_write_all_begin(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_write_all_end operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_write_all_end &dd)
{
  os << "dumpi_file_write_all_end(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_read_ordered_begin operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_read_ordered_begin &dd)
{
  os << "dumpi_file_read_ordered_begin(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_read_ordered_end operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_read_ordered_end &dd)
{
  os << "dumpi_file_read_ordered_end(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_write_ordered_begin operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_write_ordered_begin &dd)
{
  os << "dumpi_file_write_ordered_begin(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_write_ordered_end operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_write_ordered_end &dd)
{
  os << "dumpi_file_write_ordered_end(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_get_type_extent operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_get_type_extent &dd)
{
  os << "dumpi_file_get_type_extent(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "datatype=" << dd.datatype << ", ";
    /** Argument value after PMPI call */
    os << "extent=" << dd.extent << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a register_datarep operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_register_datarep &dd)
{
  os << "dumpi_register_datarep(";
  if(&dd) {
    /** Argument value before PMPI call.  NUL-terminated std::string */
    os << "name=\"" << dd.name << "\")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_set_atomicity operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_set_atomicity &dd)
{
  os << "dumpi_file_set_atomicity(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value before PMPI call */
    os << "flag=" << dd.flag << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_get_atomicity operation.
 */
inline std::ostream& operator<<(std::ostream &os,
                                const dumpi_file_get_atomicity &dd)
{
  os << "dumpi_file_get_atomicity(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ", ";
    /** Argument value after PMPI call */
    os << "flag=" << dd.flag << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for a file_sync operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpi_file_sync &dd)
{
  os << "dumpi_file_sync(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "file=" << dd.file << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for an IO test operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpio_test &dd)
{
  os << "dumpio_test(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "request=" << dd.request << ", ";
    /** Argument value after PMPI call */
    os << "flag=" << dd.flag << ", ";
    /** Argument value before PMPI call.  Only stored if(flag!=0) */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for an IO wait operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpio_wait &dd)
{
  os << "dumpio_wait(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "request=" << dd.request << ", ";
    /** Argument value after PMPI call */
    os << "status=" << *dd.status << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for an IO testall operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpio_testall &dd)
{
  os << "dumpio_testall(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "requests=" << P::ints(dd.count, dd.requests) << ", ";
    /** Argument value after PMPI call */
    os << "flag=" << dd.flag << ", ";
    /**
     * Argument value after PMPI call.  Array of length [count].
     * Only stored if(flag!=0)
     */
    os << "statuses=" << P::statuses(dd.count, dd.statuses) << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for an IO waitall operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpio_waitall &dd)
{
  os << "dumpio_waitall(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "requests=" << P::ints(dd.count, dd.requests) << ", ";
    /** Argument value after PMPI call.  Array of length [count] */
    os << "statuses=" << P::statuses(dd.count, dd.statuses) << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for an IO testany operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpio_testany &dd)
{
  os << "dumpio_testany(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "requests=" << P::ints(dd.count, dd.requests) << ", ";
    /** Argument value before PMPI call.  Only stored if(flag!=0) */
    os << "index=" << dd.index << ", ";
    /** Argument value after PMPI call */
    os << "flag=" << dd.flag << ", ";
    /**
     * Argument value after PMPI call.  Array of length [count].
     * Only stored if(flag!=0)
     */
    os << "statuses=" << P::statuses(dd.count, dd.statuses) << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for an IO waitany operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpio_waitany &dd)
{
  os << "dumpio_waitany(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "requests=" << P::ints(dd.count, dd.requests) << ", ";
    /** Argument value after PMPI call */
    os << "index=" << dd.index << ", ";
    /**
     * Argument value after PMPI call.  Array of length [count].
     * Only stored if(index != MPI_UNDEFINED)
     */
    os << "statuses=" << P::statuses(dd.count, dd.statuses) << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for an IO waitsome operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpio_waitsome &dd)
{
  os << "dumpio_waitsome(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "requests=" << P::ints(dd.count, dd.requests) << ", ";
    /** Argument value after PMPI call */
    os << "outcount=" << dd.outcount << ", ";
    /** Argument value after PMPI call.  Array of length [*outcount] */
    os << "indices=" << P::ints(dd.outcount, dd.indices) << ", ";
    /** Argument value after PMPI call.  Array of length [*outcount] */
    os << "statuses=" << P::statuses(dd.outcount, dd.statuses) << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}
/**
 * C++ IO for an IO testsome operation.
 */
inline std::ostream& operator<<(std::ostream &os, const dumpio_testsome &dd)
{
  os << "dumpio_testsome(";
  if(&dd) {
    /** Argument value before PMPI call */
    os << "count=" << dd.count << ", ";
    /** Argument value before PMPI call.  Array of length [count] */
    os << "requests=" << P::ints(dd.count, dd.requests) << ", ";
    /** Argument value after PMPI call */
    os << "outcount=" << dd.outcount << ", ";
    /** Argument value after PMPI call.  Array of length [*outcount] */
    os << "indices=" << P::ints(dd.outcount, dd.indices) << ", ";
    /** Argument value after PMPI call.  Array of length [*outcount] */
    os << "statuses=" << P::statuses(dd.outcount, dd.statuses) << ")";
  }
  else {
    os << "NULL)";
  }
  return os;
}

}
}

#endif /* SSTMAC_SOFTWARE_SKELETONS_UNDUMPI_DUMPITYPEIO_H_INCLUDED */
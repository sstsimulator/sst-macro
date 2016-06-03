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

#include <sstmac/libraries/mpi/rma/mpi_rma_message.h>
#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <stdlib.h>
#include <sstream>

namespace sstmac {
namespace sw {

// Pointer types.

//
// Hello.
//
mpi_rma_message::mpi_rma_message(const std::string& libn,
                                 int64_t envelope, int64_t mintrans, int count,
                                 mpi_type_id type, int type_packed_size,
                                 mpi_id source, mpi_id dest,
                                 mpi_comm_id commid, int seqnum, mpi_message::id msgid,
                                 category cat, task_id source_task, task_id dest_task,
                                 app_id aid, content_type_t ctype,
                                 const payload::const_ptr& content,
                                 mpi_protocol* protocol,
                                 rmatype_t rmat, int win, const op_info &info) :
  mpi_message(libn, envelope, mintrans, count, type, type_packed_size, source, dest,
              mpi_tag(-1), commid, seqnum, msgid, false, cat, source_task,
              dest_task, aid, protocol),
  rmatype_(rmat), win_(win), info_(info)
{
  content_ = content;
  content_type(ctype);
}

mpi_rma_message::mpi_rma_message(content_type_t content_type,
                                 rmatype_t rmat, int win, const op_info &info) :
  mpi_message(content_type), rmatype_(rmat), win_(win), info_(info)
{

}

mpi_rma_message::mpi_rma_message(const payload::const_ptr& content,
                                 content_type_t content_type, rmatype_t rmat, int win,
                                 const op_info &info) :
  mpi_message(content, content_type), rmatype_(rmat), win_(win), info_(info)
{

}

mpi_message*
mpi_rma_message::deep_clone() const
{
  mpi_rma_message* ret = new mpi_rma_message(lib_name_,
                               envelope_, mintrans_, count_, type_, type_packed_size_,
                               source_, dest_, commid_,
                               seqnum_, msgid_, cat_, src_task_, dest_task_, aid_, content_type_,
                               content_, protocol(), rmatype_, win_, info_);

  ret->toaddr_ = toaddr_;
  ret->fromaddr_ = fromaddr_;
  ret->ignore_seqnum_ = ignore_seqnum_;
  return ret;
}

std::string
mpi_rma_message::to_string() const
{
  std::string b = mpi_message::to_string();

  return "(rma)" + b;
}

void
mpi_rma_message::build_status(mpi_status* stat) const
{
  stat->set_content(this->content());
}

}
} // end of namespace sstmac


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

#include <sstmac/libraries/mpi/mpi_message.h>
#include <sstmac/libraries/mpi/mpi_status.h>
#include <sstmac/libraries/mpi/mpi_protocol/mpi_protocol.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/common/messages/payload.h>
#include <sstmac/common/sstmac_env.h>
#include <sprockit/debug.h>
#include <sprockit/errors.h>

#include <stdlib.h>
#include <sstream>

namespace sstmac {
namespace sw {

// Pointer types.

//
// Hello.
//
mpi_message::mpi_message(const std::string& libn, 
                         int64_t envelope, int64_t mintrans, int count,
                         mpi_type_id type, int type_packed_size,
                         mpi_id source, mpi_id dest, mpi_tag tag,
                         mpi_comm_id commid, int seqnum, mpi_message::id msgid,
                         bool ssend, category cat, task_id source_task,
                         task_id dest_task, app_id aid,
                         mpi_protocol* protocol,
                         const payload::const_ptr& content) :
  library_interface(libn),
  //I don't know node addr yet
  //and just use zero bytes for the payload size for now
  network_message(node_id(), node_id(), source_task, dest_task, -1),
  envelope_(envelope), mintrans_(mintrans), count_(count),
  type_(type), type_packed_size_(type_packed_size),
  source_(source), dest_(dest), tag_(tag), commid_(commid),
  seqnum_(seqnum), msgid_(msgid), cat_(cat),
  content_type_(null_content),
  ssend_(ssend),
  aid_(aid),
  content_(content),
  ignore_seqnum_(false)
{
  if (envelope_ < 0) {
    envelope_ = 0;
  }
  if (mintrans_ < 0) {
    mintrans_ = 0;
  }
  protocol_ = protocol->get_prot_id();
  //cannot call recompute bytes yet because
  //I have not been assigned a content type
}

void
mpi_message::recompute_bytes()
{
  switch (content_type_)
  {
  case eager_payload:
  case data: {
    long sbytes = envelope_ + count_ * type_packed_size_;
    bytes_ = sbytes > mintrans_ ? sbytes : mintrans_;
    break;
  }
  case header:
  case rendezvous_ack:
  case completion_ack:
  case fake:
    bytes_ = envelope_ > mintrans_ ? envelope_ : mintrans_;
    break;
  default:
    spkt_throw_printf(sprockit::value_error,
        "mpi_message::recompute_bytes: invalid type %s",
        str(content_type_));
  }
}

mpi_message::mpi_message(content_type_t content_type) :
  library_interface(""),
  envelope_(0), mintrans_(0), count_(0), commid_(-1),
  content_type_(content_type), content_(0),
  protocol_(mpi_protocol::EAGER0_SOCKET), ignore_seqnum_(false),
  type_packed_size_(1)
{
  recompute_bytes();
}

mpi_message::mpi_message(const payload::const_ptr& content,
                         content_type_t content_type) :
  library_interface(""),
  envelope_(0), mintrans_(0), count_(0), commid_(-1),
  content_type_(content_type), content_(content),
  protocol_(mpi_protocol::EAGER0_SOCKET),
  ignore_seqnum_(false),
  type_packed_size_(1)
{
  recompute_bytes();
}

mpi_message::mpi_message() :
  library_interface(""),
  commid_(-1), count_(0),
  type_packed_size_(1)
{
}

void
mpi_message::content_type(content_type_t ty)
{
  content_type_ = ty;
  recompute_bytes();
}

mpi_message*
mpi_message::clone() const
{
  mpi_message* cln = new mpi_message;
  clone_into(cln);
  return cln;
}

hw::network_message*
mpi_message::clone_injection_ack() const
{
  mpi_message* cln = clone();
  cln->convert_to_ack();
  return cln;
}

void
mpi_message::serialize_order(serializer& ser)
{
  //sst_message::serialize_order(ser);
  network_message::serialize_order(ser);
  library_interface::serialize_order(ser);

  ser & type_;
  ser & type_packed_size_;
  ser & (envelope_);
  ser & (mintrans_);
  ser & (count_);
  ser & (source_);
  ser & (dest_);
  ser & (tag_);
  ser & (commid_);
  ser & (seqnum_);
  ser & (msgid_);
  ser & (cat_);
  ser & (content_type_);
  ser & (ssend_);
  ser & (aid_);
  ser & (ignore_seqnum_);
  ser & (bytes_);
  ser & (protocol_);

  sstmac::payload* p = const_cast<sstmac::payload*>(content_.get());
  ser & (p);
  content_ = p;

  ser & buffer_;
  if (buffer_.eager && network_message::type() != network_message::rdma_get_request){
    //the whole array
    int size = buffer_.data ? type_packed_size_ * count_ : 0;
    ser & sstmac::array(buffer_.data, size);
  }
}

void
mpi_message::put_on_wire()
{
  if (buffer_.data == 0 || is_metadata()){
    //nothing to do
  } else {
    char* new_buf = new char[bytes_];
    ::memcpy(new_buf, buffer_.data, bytes_);
    buffer_.data = new_buf;
  }
}


void
mpi_message::clone_into(mpi_message* cln) const
{
  library_interface::clone_into(cln);
  network_message::clone_into(cln);
  cln->envelope_ = envelope_;
  cln->mintrans_ = mintrans_;
  cln->count_ = count_;
  cln->type_ = type_;
  cln->tag_ = tag_;
  cln->commid_ = commid_;
  cln->seqnum_ = seqnum_;
  cln->msgid_ = msgid_;
  cln->ssend_ = ssend_;
  cln->cat_ = cat_;
  cln->aid_ = aid_;
  cln->source_ = source_;
  cln->dest_ = dest_;
  cln->content_type_ = content_type_;
  cln->content_ = content_;
  cln->protocol_ = protocol_;
  cln->ignore_seqnum_ = ignore_seqnum_;
}

void
mpi_message::reverse()
{
  mpi_id tmp = source_;
  source_ = dest_;
  dest_ = tmp;
  network_message::reverse();
}

//
// Goodbye.
//
mpi_message::~mpi_message() throw ()
{
}

//
// Create a status object.
//
void
mpi_message::build_status(mpi_status* stat) const
{
  if (content_type_ != fake) {
    stat->set_commid(commid_);
    stat->set_source(source_);
    stat->set_dest(dest_);
    stat->set_tag(tag_);
    stat->set_send_type(type_);
    stat->set_count(count_);
    stat->add_bytes_received(count_ * type_packed_size_);
  }
  stat->set_content(content_);
}

#define enumcase(x) case x: return #x;
const char*
mpi_message::str(category cat)
{
  switch (cat) {
      enumcase(user);
      enumcase(collective);
      enumcase(onesided);
  }
  spkt_throw_printf(sprockit::value_error, "unknown mpi category %d", cat);
}

const char*
mpi_message::str(content_type_t content_type)
{
  switch (content_type) {
      enumcase(null_content);
      enumcase(data);
      enumcase(eager_payload);
      enumcase(header);
      enumcase(rendezvous_ack);
      enumcase(completion_ack);
      enumcase(fake);
  }
  spkt_throw_printf(sprockit::value_error,
      "unknown mpi content type %d", content_type);
}

void
mpi_message::set_content(const payload::const_ptr& p)
{
  content_ = p;
}

std::string
mpi_message::to_string() const
{
  std::stringstream ss;
  ss << "mpimessage("
     << ", nettype=" << network_message::tostr(network_message::type())
     << ", env=" << envelope_
     << ", mintrans=" << mintrans_
     << ", count=" << count_
     << ", type=" << type_
     << ", src=" << source_
     << ", dst=" << dest_
     << ", toaddr=" << toaddr_
     << ", fromaddr=" << fromaddr_
     << ", tag=" << tag_
     << ", commid" << commid_;

  if (ignore_seqnum_) {
    ss << ", seq=(ignored)" << seqnum_;
  }
  else {
    ss << ", seq=" << seqnum_;
  }

  ss << ", ssend=" << ssend_
     << ", category=" << str(cat_)
     << ", content=" << str(content_type_)
     << ", srctsk=" << src_task_
     << ", dsttsk=" << dest_task_
     << ", protocol=" << protocol()->to_string()
     << ", load=" << ((content_ == 0) ? "null" : content_->to_string()) << ")";
  return ss.str();
}

long
mpi_message::payload_bytes() const
{
  return abs(count_) * type_packed_size_;
}

void
mpi_message::convert(mpi_conversion_t conv)
{
  switch (conv)
  {
  case HEADER_TO_RDMA_GET_REQ: {
    // this is a control message, but it's part of a "data" action
    // this will be converted to an actual payload in the NIC
    content_type_ = mpi_message::data;
    network_message::type_ = network_message::rdma_get_request;
    recompute_bytes();
    reverse();
    break;
  }
  case HEADER_TO_RDMA_PUT_REQ: {
    spkt_throw(sprockit::unimplemented_error,
        "mpi_message::convert: put request");
  }
  }

#if 0
  case RDMA_PUT_REQ_TO_RSP {
    if (count_ < 0) {
      spkt_throw(sprockit::illformed_error,
                  "mpimessage: invalid rdma put request generation");
    }
    count_ = -count_;
    msgtype_ = RDMA_PUT_RSP;
    content_type_ = mpi_message::completion_ack;
    //set_needs_ack(false);
    reverse();
    //no MPI overhead on this - just the payload
    recompute_bytes();
    break;
  }
#endif
}

void
mpi_message::header_to_rendezvous_ack()
{
  reverse();
  content_type_ = mpi_message::rendezvous_ack;
  hw::network_message::type_ = hw::network_message::payload;
}

void
mpi_message::rendezvous_ack_to_payload()
{
  reverse();
  content_type_ = mpi_message::data;
  hw::network_message::type_ = hw::network_message::payload;
  recompute_bytes();
}

void
mpi_message::payload_to_completion_ack()
{
  reverse();
  //needs_ack_ = false;
  content_type_ = mpi_message::completion_ack;
  hw::network_message::type_ = hw::network_message::payload;
  recompute_bytes();
}

mpi_protocol*
mpi_message::protocol() const
{
  return mpi_protocol::get_protocol_object((mpi_protocol::PROTOCOL_ID)protocol_);
}

void
mpi_message::set_protocol(mpi_protocol* protocol)
{
  protocol_ = protocol->get_prot_id();
}

void
mpi_message::set_ignore_seqnum(bool flag)
{
  ignore_seqnum_ = flag;
}

bool
mpi_message::ignore_seqnum() const
{
  return ignore_seqnum_;
}

}
} // end of namespace sstmac


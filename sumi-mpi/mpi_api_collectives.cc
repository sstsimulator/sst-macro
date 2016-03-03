#include <sumi-mpi/mpi_api.h>
#include <sumi-mpi/mpi_queue/mpi_queue.h>
#include <sstmac/software/process/backtrace.h>

namespace sumi {


void
mpi_api::validate_mpi_collective(const char* name, MPI_Datatype sendtype, MPI_Datatype recvtype)
{
  if (sendtype != recvtype){
    spkt_throw_printf(sprockit::unimplemented_error,
      "%s: cannot handle different types for send/recv",
      name);
  }

  mpi_type* sendtypePtr = type_from_id(sendtype);
  mpi_type* recvtypePtr = type_from_id(recvtype);
  if (!sendtypePtr->contiguous() || !recvtypePtr->contiguous()){
    spkt_throw_printf(sprockit::unimplemented_error,
      "%s: cannot handle non-contiguous types for send/recv",
      name);
  }

}

void
mpi_api::collective_progress_loop(collective::type_t ty, int tag, bool tmp_domain)
{
  std::list<collective_done_message::ptr> pending;
  while (1){
    sumi::message::ptr msg = blocking_poll();
    if (msg->class_type() == message::collective_done){
      //this is a collective done message
      collective_done_message::ptr cmsg = ptr_safe_cast(collective_done_message, msg);
      mpi_api_debug(sprockit::dbg::mpi_collective,
                    "found collective done message of type=%s tag=%d: need %s,%d",
                    collective::tostr(cmsg->type()), cmsg->tag(), collective::tostr(ty), tag);
      if (tag == cmsg->tag() && ty == cmsg->type()){  //done!
        if (tmp_domain){
          delete cmsg->dom();
        }
        break;
      } else {
        //a different collective completed
        pending.push_back(cmsg);
      }
    } else {
      mpi_message::ptr mpiMsg = ptr_safe_cast(mpi_message, msg);
      queue_->incoming_message(mpiMsg);
    }
  }

  
  std::list<collective_done_message::ptr>::iterator it, end = pending.end();
  for (it=pending.begin(); it != end; ++it){
    completion_queue_.push_back(*it);
  }
}

int
mpi_api::start_allgather(const void *sendbuf, void *recvbuf, int count, MPI_Datatype type, MPI_Comm comm)
{
  int typeSize = type_size(type);
  mpi_comm* commPtr = get_comm(comm);
  int tag = commPtr->next_collective_tag();
  transport::allgather(recvbuf, const_cast<void*>(sendbuf), count, typeSize, tag,
    false, options::initial_context,
    (comm == MPI_COMM_WORLD ? 0 : commPtr)); //comm world is a "null" domain
  return tag;
}

int
mpi_api::allgather(int sendcount, MPI_Datatype sendtype, int recvcount, MPI_Datatype recvtype, MPI_Comm comm)
{
  return allgather(NULL, sendcount, sendtype, NULL, recvcount, recvtype, comm);
}

int
mpi_api::allgather(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Allgather");
  validate_mpi_collective("allgather", sendtype, recvtype);
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Allgather(%d,%s,%d,%s,%s)",
    sendcount, type_str(sendtype).c_str(),
    recvcount, type_str(recvtype).c_str(),
    comm_str(comm).c_str());
  int tag = start_allgather(sendbuf, recvbuf, sendcount, sendtype, comm);
  collective_progress_loop(collective::allgather, tag);
  return MPI_SUCCESS;
}

int
mpi_api::start_alltoall(const void *sendbuf, void *recvbuf, int count, MPI_Datatype type, MPI_Comm comm)
{
  int typeSize = type_size(type);
  mpi_comm* commPtr = get_comm(comm);
  int tag = commPtr->next_collective_tag();
  spkt_throw(sprockit::unimplemented_error,
    "sumi::alltoall");
  return tag;
}

int
mpi_api::alltoall(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Alltoall");
  validate_mpi_collective("alltoall", sendtype, recvtype);
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Alltoall(%d,%s,%d,%s,%s)",
    sendcount, type_str(sendtype).c_str(),
    recvcount, type_str(recvtype).c_str(),
    comm_str(comm).c_str());
  int tag = start_alltoall(sendbuf, recvbuf, sendcount, sendtype, comm);
  collective_progress_loop(collective::alltoall, tag);
  return MPI_SUCCESS;
}

int
mpi_api::start_allreduce(const void *src, void *dst, int count, MPI_Datatype type, MPI_Op op, MPI_Comm comm)
{
  if (src == MPI_IN_PLACE) src = dst;

  mpi_type* typePtr = type_from_id(type);
  reduce_fxn fxn = typePtr->op(op);
  int typeSize = typePtr->packed_size();
  mpi_comm* commPtr = get_comm(comm);
  int tag = commPtr->next_collective_tag();
  transport::allreduce(dst, const_cast<void*>(src), count, typeSize, tag, fxn,
    false, options::initial_context,
    (comm == MPI_COMM_WORLD ? 0 : commPtr)); //comm world is a "null" domain
  return tag;
}

int
mpi_api::allreduce(const void *src, void *dst, int count, MPI_Datatype type, MPI_Op op, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Allreduce");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Allreduce(%d,%s,%s,%s)",
    count, type_str(type).c_str(), op_str(op), comm_str(comm).c_str());
  int tag = start_allreduce(src, dst, count, type, op, comm);
  collective_progress_loop(collective::allreduce, tag);
  return MPI_SUCCESS;
}

int
mpi_api::allreduce(int count, MPI_Datatype type, MPI_Op op, MPI_Comm comm)
{
  return allreduce(NULL, NULL, count, type, op, comm);
}

int
mpi_api::start_barrier(MPI_Comm comm)
{
  mpi_comm* commPtr = get_comm(comm);
  int tag = commPtr->next_collective_tag();
  transport::barrier(tag, false,
    (comm == MPI_COMM_WORLD ? 0 : commPtr)); //comm world is a "null" domain
  return tag;
}

int
mpi_api::barrier(MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Barrier");
  int tag = start_barrier(comm);
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Barrier(%s) on tag %d",
    comm_str(comm).c_str(), int(tag));
  collective_progress_loop(collective::barrier, tag);
  return MPI_SUCCESS;
}

int
mpi_api::start_bcast(void *buffer, int count, MPI_Datatype type, int root, MPI_Comm comm)
{
  int typeSize = type_size(type);
  mpi_comm* commPtr = get_comm(comm);
  int tag = commPtr->next_collective_tag();

  sumi::domain* dom = comm == MPI_COMM_WORLD ? global_dom() : commPtr;
  if (root != 0){
    //shift everyone in the domain
    dom = new sumi::shifted_domain(dom, root);
  }

  transport::bcast(buffer, count, typeSize, tag,
    false, options::initial_context, dom);

  return tag;
}

int
mpi_api::bcast(void *buffer, int count, MPI_Datatype type, int root, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Bcast");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Bcast(%d,%s,%d,%s)",
    count, type_str(type).c_str(), int(root), comm_str(comm).c_str());
  int tag = start_bcast(buffer, count, type, root, comm);

  collective_progress_loop(collective::bcast, tag, root!=0/*whether a tmp domain*/);
  return MPI_SUCCESS;
}

int
mpi_api::bcast(int count, MPI_Datatype datatype, int root, MPI_Comm comm)
{
  return bcast(NULL, count, datatype, root, comm);
}

int
mpi_api::start_gather(const void *sendbuf, void *recvbuf, int count, MPI_Datatype type, int root, MPI_Comm comm)
{
  int typeSize = type_size(type);
  mpi_comm* commPtr = get_comm(comm);
  int tag = commPtr->next_collective_tag();
  spkt_throw(sprockit::unimplemented_error,
    "sumi::gather");
  return tag;
}

int
mpi_api::gather(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Gather");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Gather(%d,%s,%d,%s,%d,%s)",
    sendcount, type_str(sendtype).c_str(),
    recvcount, type_str(recvtype).c_str(),
    int(root), comm_str(comm).c_str());
  validate_mpi_collective("scatter", sendtype, recvtype);
  int tag = start_gather(sendbuf, recvbuf, sendcount, sendtype, root, comm);
  collective_progress_loop(collective::reduce, tag);
  return MPI_SUCCESS;
}

int
mpi_api::gather(int sendcount, MPI_Datatype sendtype, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  return gather(NULL, sendcount, sendtype, NULL, recvcount, recvtype, root, comm);
}

int
mpi_api::start_reduce(const void *src, void *dst, int count, MPI_Datatype type, MPI_Op op, int root, MPI_Comm comm)
{
  mpi_type* typePtr = type_from_id(type);
  reduce_fxn fxn = typePtr->op(op);
  int typeSize = typePtr->packed_size();
  mpi_comm* commPtr = get_comm(comm);
  int tag = commPtr->next_collective_tag();
  //transport::reduce(src, dst, count, typeSize, tag, fxn, root,
  //  false, options::initial_context,
  //  (comm == MPI_COMM_WORLD ? 0, commPtr));
  spkt_throw(sprockit::unimplemented_error,
    "sumi::reduce");
  return tag;
}

int
mpi_api::reduce(const void *src, void *dst, int count, MPI_Datatype type, MPI_Op op, int root, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Reduce");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Reduce(%d,%s,%s,%d,%s)", count, type_str(type).c_str(),
    op_str(op), int(root), comm_str(comm).c_str());
  int tag = start_reduce(src, dst, count, type, op, root, comm);
  collective_progress_loop(collective::reduce, tag);
  return MPI_SUCCESS;
}

int
mpi_api::reduce(int count, MPI_Datatype type, MPI_Op op, int root, MPI_Comm comm)
{
  return reduce(NULL, NULL, count, type, op, root, comm);
}

int
mpi_api::start_reduce_scatter(const void *src, void *dst, int *recvcnts, MPI_Datatype type, MPI_Op op, MPI_Comm comm)
{
  mpi_type* typePtr = type_from_id(type);
  reduce_fxn fxn = typePtr->op(op);
  int typeSize = typePtr->packed_size();
  mpi_comm* commPtr = get_comm(comm);
  int tag = commPtr->next_collective_tag();
  //transport::reduce_scatter(src, dst, recvcnts, typeSize, tag, fxn,
  //  false, options::initial_context,
  //  (comm == MPI_COMM_WORLD ? 0, commPtr));
  spkt_throw(sprockit::unimplemented_error,
    "sumi::reduce_scatter");
  return tag;
}

int
mpi_api::reduce_scatter(const void *src, void *dst, int *recvcnts, MPI_Datatype type, MPI_Op op, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Reducescatter");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Reduce_scatter(<...>,%s,%s,%s)",
    type_str(type).c_str(), op_str(op), comm_str(comm).c_str());
  int tag = start_reduce_scatter(src, dst, recvcnts, type, op, comm);
  collective_progress_loop(collective::reduce_scatter, tag);
  return MPI_SUCCESS;
}

int
mpi_api::reduce_scatter(int *recvcnts, MPI_Datatype type, MPI_Op op, MPI_Comm comm)
{
  return reduce_scatter(NULL, NULL, recvcnts, type, op, comm);
}

int
mpi_api::start_scan(const void *src, void *dst, int count, MPI_Datatype type, MPI_Op op, MPI_Comm comm)
{
  mpi_type* typePtr = type_from_id(type);
  reduce_fxn fxn = typePtr->op(op);
  int typeSize = typePtr->packed_size();
  mpi_comm* commPtr = get_comm(comm);
  int tag = commPtr->next_collective_tag();
  //transport::reduce_scatter(src, dst, recvcnts, typeSize, tag, fxn,
  //  false, options::initial_context,
  //  (comm == MPI_COMM_WORLD ? 0, commPtr));
  spkt_throw(sprockit::unimplemented_error,
    "sumi::scan");
  return tag;
}

int
mpi_api::scan(const void *src, void *dst, int count, MPI_Datatype type, MPI_Op op, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Scan");
  mpi_api_debug(sprockit::dbg::mpi | sprockit::dbg::mpi_collective,
    "MPI_Scan(%d,%s,%s,%s)",
    count, type_str(type).c_str(), op_str(op), comm_str(comm).c_str());
  int tag = start_scan(src, dst, count, type, op, comm);
  collective_progress_loop(collective::scan, tag);
  return MPI_SUCCESS;
}

int
mpi_api::scan(int count, MPI_Datatype type, MPI_Op op, MPI_Comm comm)
{
  return scan(NULL, NULL, count, type, op, comm);
}

int
mpi_api::start_scatter(const void *sendbuf, void *recvbuf, int count, MPI_Datatype type, int root, MPI_Comm comm)
{
  int typeSize = type_size(type);
  mpi_comm* commPtr = get_comm(comm);
  int tag = commPtr->next_collective_tag();
  spkt_throw(sprockit::unimplemented_error,
    "sumi::scatter");
  return tag;
}

int
mpi_api::scatter(const void *sendbuf, int sendcount, MPI_Datatype sendtype, void *recvbuf, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  SSTMACBacktrace("MPI_Scatter");
  mpi_api_debug(sprockit::dbg::mpi, "MPI_Scatter(%d,%s,%d,%s,%d,%s)",
    sendcount, type_str(sendtype).c_str(),
    recvcount, type_str(recvtype).c_str(),
    int(root), comm_str(comm).c_str());
  validate_mpi_collective("scatter", sendtype, recvtype);
  int tag =start_scatter(sendbuf, recvbuf, sendcount, sendtype, root, comm);
  collective_progress_loop(collective::scatter, tag);
  return MPI_SUCCESS;
}

int
mpi_api::scatter(int sendcount, MPI_Datatype sendtype, int recvcount, MPI_Datatype recvtype, int root, MPI_Comm comm)
{
  return scatter(sendcount, sendtype, recvcount, recvtype, root, comm);
}

}

#include <gni/gni_transport.h>
#include <cstdlib>
#include <stdlib.h>
#include <unistd.h>
#include <cstring>

namespace sumi {

template <class peer_data_t>
peer_data_t* 
gather_data(int rank, int nproc, peer_data_t* mydata)
{
  mydata->rank = rank;
  peer_data_t* tmp = new peer_data_t[nproc];
  peer_data_t* output = new peer_data_t[nproc];
  ::memcpy(&tmp[rank], mydata, sizeof(peer_data_t));
  PMI_Allgather(&tmp[rank], tmp, sizeof(peer_data_t));
  for (int i=0; i < nproc; ++i){
     int wrong_order_i = tmp[i].rank;        
     peer_data_t* dst = output + wrong_order_i;
     peer_data_t* src = tmp + i;
     ::memcpy(dst, src, sizeof(peer_data_t));
  }
  delete[] tmp;
  return output;
}

void
gni_transport::gather_nic_data()
{
  nic_data_t mydata;
  mydata.nic_addr = my_global_nic_addr_;
  nics_ = gather_data(rank_, nproc_, &mydata);
}

void
gni_transport::gather_peer_data()
{
  peer_segment_data_t mydata;
  gethostname(mydata.hostname, MAX_HOSTNAME_LENGTH);
  mydata.smsg_attr = my_smsg_attr_;
  mydata.ping_buffer = ping_buffer_;
  mydata.ping_mem_handle = ping_mem_handle_;
#if 0
  printf("init smsg on node %d with msg_type=%d maxcredit=%d maxsize=%d, buffsize=%d msqid=%d hostname=%s\n",
    rank_,
    mydata.smsg_attr.msg_type,
    mydata.smsg_attr.mbox_maxcredit,
    mydata.smsg_attr.msg_maxsize,
    mydata.smsg_attr.buff_size,
    mydata.msqid,
    mydata.hostname);
#endif
  peers_ = gather_data(rank_, nproc_, &mydata);
}

}

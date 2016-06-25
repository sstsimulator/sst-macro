#include <sumi/alltoallv.h>
#include <sumi/partner_timeout.h>
#include <sumi/transport.h>
#include <sumi/domain.h>
#include <sprockit/output.h>
#include <cstring>

#define divide_by_2_round_up(x) \
  ((x/2) + (x%2))

#define divide_by_2_round_down(x) \
  (x/2)

using namespace sprockit::dbg;

#define SEND_SHUFFLE 0
#define RECV_SHUFFLE 1

namespace sumi
{

SpktRegister("bruck_alltoall", dag_collective, bruck_alltoallv_collective);

void
bruck_alltoallv_actor::init_buffers(void* dst, void* src)
{
}

void
bruck_alltoallv_actor::finalize_buffers()
{
}

void
bruck_alltoallv_actor::shuffle(action *ac, void* tmpBuf, void* mainBuf, bool copyToTemp)
{
}

void
bruck_alltoallv_actor::start_shuffle(action *ac)
{
}

void
bruck_alltoallv_actor::init_dag()
{
}

void
bruck_alltoallv_actor::buffer_action(void *dst_buffer, void *msg_buffer, action* ac)
{
  std::memcpy(dst_buffer, msg_buffer, ac->nelems * type_size_);
}

void
bruck_alltoallv_actor::finalize()
{
}


}


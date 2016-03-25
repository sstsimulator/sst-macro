#include <sumi/domain.h>
#include <sumi/transport.h>
#include <sprockit/errors.h>

namespace sumi {

global_domain::global_domain(transport *tport) :
  transport_(tport)
{
  my_domain_rank_ = tport->rank();
}

int
global_domain::nproc() const
{
  return transport_->nproc();
}

int
global_domain::domain_to_global_rank(int domain_rank) const
{
  return domain_rank;
}

int
global_domain::global_to_domain_rank(int global_rank) const
{
  return global_rank;
}

int
index_domain::global_to_domain_rank(int global_rank) const
{
  spkt_throw(sprockit::unimplemented_error,
    "index_domain::global_to_domain_rank: this should only be involved in failures");
}

}


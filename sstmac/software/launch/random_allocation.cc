#include <sstmac/software/launch/random_allocation.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sprockit/sim_parameters.h>

#include <algorithm>

namespace sstmac {
namespace sw {

SpktRegister("random", node_allocator, random_allocation,
            "Allocate a random set of nodes from the list of available nodes. This will give a non-contiguous allocation.");

random_allocation::~random_allocation() throw ()
{
}

void
random_allocation::init_factory_params(sprockit::sim_parameters *params)
{
  node_allocator::init_factory_params(params);
  int seed = params->get_optional_int_param("random_allocation_seed", -1);
  if (seed == -1){
    seed = time(NULL);
  }
  rng_ = RNG::SimpleCombo::construct(seed);
}

void
random_allocation::allocate(
  int nnode_requested,
  const ordered_node_set& available,
  ordered_node_set& allocation) const
{
  if (available.size() < nnode_requested){
    spkt_throw_printf(sprockit::value_error,
      "random allocation cannot succeed: need %d nodes, but have %d",
      nnode_requested, available.size());
  }

  std::vector<node_id> availvec(available.size());
  std::copy(available.begin(), available.end(), availvec.begin());
  RNG::UniformInteger_functor rngf(rng_);
  std::random_shuffle(availvec.begin(), availvec.end(), rngf);
  for (int i = 0; i < nnode_requested; i++) {
    node_id node = availvec[i];
    allocation.insert(node);
  }
}

} //end namespace sw
}


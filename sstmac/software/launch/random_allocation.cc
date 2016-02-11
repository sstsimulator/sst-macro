#include <sstmac/software/launch/random_allocation.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sprockit/sim_parameters.h>

#include <algorithm>

namespace sstmac {
namespace sw {

SpktRegister("random", allocation_strategy, random_allocation,
            "Allocate a random set of nodes from the list of available nodes. This will give a non-contiguous allocation.");

random_allocation::~random_allocation() throw ()
{
}

void
random_allocation::init_factory_params(sprockit::sim_parameters *params)
{
  allocation_strategy::init_factory_params(params);
  int seed = params->get_optional_int_param("random_allocation_seed", -1);
  if (seed == -1){
    seed = time(NULL);
  }
  rng_ = RNG::SimpleCombo::construct(seed);
}

void
random_allocation::allocate(int nnode_requested, node_set &allocation)
{
  validate_num_nodes(nnode_requested, "random_allocation");
  node_set& available = interconn_->available();
  node_set& allocated = interconn_->allocated();

  std::vector<node_id> availvec(available.size());
  std::copy(available.begin(), available.end(), availvec.begin());
  RNG::UniformInteger_functor rngf(rng_);
  std::random_shuffle(availvec.begin(), availvec.end(), rngf);
  for (int i = 0; i < nnode_requested; i++) {
    node_id node = availvec[i];
    allocation.insert(node);
    allocated.insert(node);
    available.erase(node);
  }
}

} //end namespace sw
}


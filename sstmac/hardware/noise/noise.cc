#include <sstmac/hardware/noise/noise.h>
#include <sprockit/sim_parameters.h>

ImplementFactory(sstmac::hw::noise_model);

namespace sstmac {
  namespace hw {

SpktRegister("gaussian", noise_model, gaussian_noise_model,
    "implements a normally distributed noise model with mean, stdev, and optional max parameter defining cutoff");

void
noise_model::init_factory_params(sprockit::sim_parameters *params)
{
}

gaussian_noise_model::~gaussian_noise_model()
{
  if (rng_) delete rng_;
}

double
gaussian_noise_model::value()
{
  return rng_->value();
}

gaussian_noise_model::gaussian_noise_model(double mean, double stdev,
                                           double maxz, int seed)
  : rng_(nullptr)
{
  rng_ = new RNG::NormalDistribution(mean, stdev, maxz, seed);
}

void
gaussian_noise_model::init_factory_params(sprockit::sim_parameters *params)
{
  double mean = params->get_quantity("mean");
  double stdev = params->get_quantity("stdev");
  int seed = params->get_optional_int_param("seed", 0);
  double maxz = params->get_optional_double_param("maxz", 100);
  rng_ = new RNG::NormalDistribution(mean, stdev, maxz, seed);
}

  }
}

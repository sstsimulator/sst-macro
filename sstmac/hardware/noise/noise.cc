#include <sstmac/hardware/noise/noise.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>

ImplementFactory(sstmac::hw::noise_model);


RegisterKeywords(
"mean",
"seed",
"stdev",
"maxz",
);

namespace sstmac {
  namespace hw {

SpktRegister("gaussian", noise_model, gaussian_noise_model,
    "implements a normally distributed noise model with mean, stdev, and optional max parameter defining cutoff");

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

gaussian_noise_model::gaussian_noise_model(sprockit::sim_parameters *params)
  : noise_model(params)
{
  double mean = params->get_quantity("mean");
  double stdev = params->get_quantity("stdev");
  int seed = params->get_optional_int_param("seed", 0);
  double maxz = params->get_optional_double_param("maxz", 100);
  rng_ = new RNG::NormalDistribution(mean, stdev, maxz, seed);
}

  }
}

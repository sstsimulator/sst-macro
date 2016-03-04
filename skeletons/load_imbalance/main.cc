#include <sstmac/sstmacro.h>
#include <sstmac/compute.h>
#include <sstmac/common/rng.h>
#include <sprockit/keyword_registration.h>

sstmac_register_app(load_imbalance);

using namespace sstmac;

RegisterKeywords(
  "num_iterations",
  "min_comp_time",
  "max_comp_time",
  "reshuffle_work",
  "comp_time_seed"
);

int 
load_imbalance_main(int argc, char **argv) 
{
  MPI_Init(&argc, &argv);
  int me, nproc;
  MPI_Comm_rank(MPI_COMM_WORLD, &me);
  MPI_Comm_size(MPI_COMM_WORLD, &nproc);

  int seed;

  int niter = env::params->get_optional_int_param("num_iterations", 10);
  double min_comp_time = env::params->get_time_param("min_comp_time");
  double max_comp_time = env::params->get_time_param("max_comp_time");
  double comp_time_range = max_comp_time - min_comp_time;
  bool reshuffle_work = env::params->get_optional_bool_param("reshuffle_work", true);
  seed = (env::params->get_optional_int_param("comp_time_seed", 911) << (me + 5)) << (me + 3) + me;

  //std::cout << seed << std::endl;
  RNG::UniformInteger* comp_rng = RNG::SHR3::construct(seed);
  comp_rng->value(); comp_rng->value(); comp_rng->value();

  double val = comp_rng->realvalue();
  //std::cout << val << std::endl;
  double comp_time = val * comp_time_range + min_comp_time;
  for (int i=0; i < niter; ++i){
    sstmac_compute(comp_time);
    MPI_Barrier(MPI_COMM_WORLD);
    if (reshuffle_work)
        comp_time = comp_rng->realvalue() * comp_time_range + min_comp_time;
  }
  MPI_Finalize();
  return 0;
}


#include <sstmac/main/driver.h>

using namespace sstmac;

int main(int argc, char** argv)
{
  SimulationQueue queue;
  queue.init(argc, argv);

  double bandwidths[] = { 0.1, 0.4, 0.8 };
  double results[] = {0, 0, 0};
  int nPoints = sizeof(bandwidths) / sizeof(double);

  sprockit::sim_parameters params;
  params["congestion_model"] = "pisces";
  params["amm_model"] = "amm3";
  params["sleep_time"] = 0;
  params["message_size"].setByteLength(16, "KB");
  
  for (int i=0; i < nPoints; ++i){
    //params["injection_bandwidth"].setBandwidth(bandwidths[i], "GB/s");
    Simulation* sim = queue.fork(params);
    sim->setLabel(i);
  }
  
  for (int i=0; i < nPoints; ++i){
    Simulation* sim = queue.waitForForked();
    int idx = sim->label();
    results[idx] = sim->simulatedTime();
  }

  for (int i=0; i < nPoints; ++i){
    printf("BW=%4.2fGB/s T=%8.4fms\n", 
      bandwidths[i], results[i]*1e3);
  }

  queue.finalize();
  return 0;
}


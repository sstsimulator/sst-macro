#include <sst/elements/ariel/ariel_shmem.h>

int main(int argc, char** argv)
{
  auto* tunnel = new SST::ArielComponent::ArielTunnel("ariel_test");
  return 0;
}


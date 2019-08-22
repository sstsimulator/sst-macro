#include <sst/elements/ariel/ariel_shmem.h>
#include <iostream>
#include <unistd.h>

using namespace SST::ArielComponent;

int main(int argc, char** argv)
{
  for (int i=1; i < argc; ++i){
    std::cout << "Got arg: " << argv[i] << std::endl;
  }
  std::string name(argv[1]);
  int initial_pause = 3;
  std::cout << "Starting app in";
  for (int i=initial_pause; i > 0; --i){
    std::cout << " ..." << i;
    std::cout.flush();
    sleep(1);
  }
  std::cout << std::endl;
  auto* tunnel = new ArielTunnel(name);

  ArielCommand ac;
  ac.command = ARIEL_START_INSTRUCTION;
  tunnel->writeMessage(0, ac);

  ac.command = ARIEL_PERFORM_READ;
  ac.instPtr = 0;
  ac.inst.addr = 0x4000000;
  ac.inst.size = 4096;
  ac.inst.instClass = 0;
  ac.inst.simdElemCount = 0;
  std::cout << "Writing test address" << std::endl;
  tunnel->writeMessage(0, ac);

  ac.command = ARIEL_END_INSTRUCTION;
  tunnel->writeMessage(0, ac);

  ac.command = ARIEL_PERFORM_EXIT;
  tunnel->writeMessage(0, ac);


  return 0;
}


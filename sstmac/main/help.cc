#include <sprockit/output.h>
#include <cstring>

void
print_help(int argc, char **argv)
{
  char *name = argv[0];
  char *tail = name + strlen(name);
  while (tail != name && *tail != '/') {
    --tail;
  }
  if (tail != name) {
    ++tail;
  }

  cout0 << "usage: " << tail << " -f <config file> [options] \n" << " \n"
            << "Options are: \n" << "\t[(--help|-h)]                          \n"
            << "\t[(--print-apps|-P)]                    \n"
            << "\t[(--debug|-d)             <value> ]    \n"
            << "\t[(--configfile|-f)        <value> ]    \n"
            << "\t[(--param|-p)       <key>=<value> ]    \n"
            << "\t[(--stoptime|-t)          <value> ]    \n"
            << "\t[(--mode|-m)              <value> ]    \n"
            << "\t[(--include|-i)           <value> ]    \n"
            << "\t[(--runnumber|-r)         <value> ]    \n"
            << "\t[(--cpu-affinity|-c)      <value>,<value>,... ]    \n"
            << "\n"

            << "Configuration file is not optional. See parameters.ini for \n"
            << "a list of parameters. \n" << "\n"
            << "--param (-p) is used to override any parameters in the \n"
            << "configuration file, for example: \n" << "\t-p network_name=buffer \n"
            << "\n--include (-i) is used to include a pre-made parameter file \n"
            << "from the configurations folder into your parameter set\n" << "\n"
            << "--stoptime (-t) stops the simulation at the given time, \n"
            << "which is useful for debugging \n" << "\n"
            << "--mode (-m) can be used to open the debugger by saying -m debug, \n"
            << "and can turn off printing the simulation at the end with -m notime \n"
            << "\n--cpu-affinity takes a comma separated list of processor affinities\n"
            << "with size equal to the number of PDES tasks per node\n"
            << "\n" << "Valid arguments to --debug (-d) are strings of the form \n"
            << "\"<(debug|stats)> (name1) | (name2) | ... \" \n"
            << "\t- examples: \n"
            << "\t\t- \"<debug> mpi\" turns on all mpi output \n"
            << "\t\t- \"<debug> app | mpicollective\" turns on all \n"
            << "\t\tskeleton and mpicollective output \n"
            << "\t\t- \"<stats> all\" turns on all stat collection \n"
            << "\t- some current useful debug tags are: \n"
            << "\t\tsoftware: app mpi mpiapi sstmac_mpi mpicollective mpi_queue \n"
            << "\t\thardware: node network nic  \n"
            << "\t\tsystem: launch allocation indexing \n"
            << "\t- current stat tags are: \n"
            << "\t\tall spyplot congestion network \n";

}

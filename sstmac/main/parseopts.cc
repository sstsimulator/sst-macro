#include <errno.h>
#include <getopt.h>
#include <sstmac/main/sstmac.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/debug.h>
#include <sprockit/basic_string_tokenizer.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <cstdlib>

void
activate_debugs(const std::string& debug_list)
{
  std::deque<std::string> tok;
  std::string space = ",";
  pst::BasicStringTokenizer::tokenize(debug_list, tok, space);
  for (auto& item : tok){
    sprockit::debug::turn_on(item);
  }
}

std::ostream&
operator<<(std::ostream &os, const opts &oo)
{
  if (oo.help) {
    os << "  --help \\\n";
  }
  os << "  --debug=\"" << oo.debug << "\" \\\n";
  os << "  --configfile=\"" << oo.configfile << "\" \\\n";
  return os;
}

int
parse_opts(int argc, char **argv, opts &oo)
{
  int no_congestion = 0;
  int pisces_debug = 0;
  int macrels_debug = 0;
  int dompitest = 0;
  int printnodes = 0;
  int no_wall_time = 0;
  int print_params = 0;
  int debugflags = 0;
  int dodumpi = 0;
  int lowrestimer = 0;
  int run_ping_all = 0;
  option gopt[] = {
    { "help", no_argument, NULL, 'h' },
    { "include", required_argument, NULL, 'i' },
    { "debug", required_argument, NULL, 'd' },
    { "configfile", required_argument, NULL, 'f' },
    { "auto", no_argument, NULL, 'a' },
    { "nproc", required_argument, NULL, 'n' },
    { "param", required_argument, NULL, 'p' },
    { "srun", required_argument, NULL, 's' },
    { "dumpi", no_argument, &dodumpi, 1 },
    { "debug-flags", no_argument, &debugflags, 1},
    { "mpitest", no_argument, &dompitest, 1 },
    { "print-nodes", no_argument, &printnodes, 1 },
    { "pisces", no_argument, &pisces_debug, 1 },
    { "macrels", no_argument, &macrels_debug, 1 },
    { "ping-all", no_argument, &run_ping_all, 1 },
    { "no-congestion", no_argument, &no_congestion, 1 },
    { "low-res-timer", no_argument, &lowrestimer, 1 },
    { "print-params", no_argument, &print_params, 1 },
    { "no-wall-time", no_argument, &no_wall_time, 1 },
    { "cpu-affinity", required_argument, NULL, 'c' },
    { NULL, 0, NULL, '\0' }
  };
  int ch;
  bool errorflag = false;
  std::list<std::pair<std::string, std::string> > paramlist;
  oo.params = new sprockit::sim_parameters;
  optind = 1;
  while ((ch = getopt_long(argc, argv, "Phad:f:t:p:m:n:u:i:c:", gopt, NULL))
         != -1) {
    switch (ch) {
      case 0:
        //this set an input flag
        break;
      case 'h':
        oo.help = 1;
        break;
      case 'd':
        activate_debugs(optarg);
        break;
      case 'a': {
        oo.configfile = "debug.ini";
        oo.params->add_param_override("app1.name", "user_app_cxx");
        oo.params->add_param_override("app1.launch_cmd", "aprun -n 1");
        break;
      }
      case 'n' : {
        oo.params->add_param_override("app1.launch_cmd", sprockit::printf("aprun -n %s -N 1", optarg));
        break;
      }
      case 'f':
        oo.configfile = optarg;
        break;
      case 'i': {
        sprockit::sim_parameters* params = new sprockit::sim_parameters(optarg);
        params->combine_into(oo.params);
        delete params;
      }
      break;
      case 'p': {
        //overwrite anything existing
        oo.params->parse_line(optarg, false, true);
        break;
      }
      case 'c': {
        //overwrite anything existing
        std::string param_line = std::string("cpu_affinity = ") + std::string(optarg);
        oo.params->parse_line(param_line, false, true);
        break;
      }
      default:
        cerr0 << "Unhandled input flag" << std::endl;
        errorflag = true;
        break;
    }
    if (oo.help) {
      print_help(argc, argv);
      return PARSE_OPT_EXIT_SUCCESS;
    }
    if (errorflag) {
      return PARSE_OPT_EXIT_FAIL;
    }
  }

  if (print_params) {
    oo.print_params = true;
  }

  if (no_congestion) {
    oo.params->add_param("switch.arbitrator", "null");
  }

  if (dodumpi) {
    oo.configfile = "debug.ini";
    oo.params->add_param("app1.name", "parsedumpi");
  }

  if (pisces_debug) {
    oo.configfile = "pisces.ini";
  }

  if (macrels_debug) {
    oo.configfile = "macrels.ini";
  }

  if (run_ping_all){
    oo.params->add_param("app1.name", "mpi_ping_all");
  }

  if (debugflags){
    sprockit::debug::print_all_debug_slots(std::cout);
    return PARSE_OPT_EXIT_SUCCESS;
  }
  /** check to see if we should do an mpitestall */
  if (dompitest) {
    if (oo.configfile != "") {
      cerr0 << "Cannot set config file with --configfile and also request --mpitest" << std::endl;
      return PARSE_OPT_EXIT_FAIL;
    }
    oo.configfile = "mpi_test_all.ini";
  }
  if (printnodes) {
    oo.params->add_param_override("app1.name", "mpi_printnodes");
  }

  if (lowrestimer){
    oo.low_res_timer = true;
  }

  /** double negative, sorry about that */
  oo.print_walltime = !no_wall_time;


  return PARSE_OPT_SUCCESS;
}

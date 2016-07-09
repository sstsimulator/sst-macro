#include <sstmac/common/runtime.h>
#include <sstmac/hardware/interconnect/interconnect.h>
#include <sstmac/hardware/topology/structured_topology.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/app_manager.h>
#include <sstmac/software/launch/launch_info.h>
#include <sstmac/dumpi_util/dumpi_meta.h>
#include <sprockit/output.h>
#include <sprockit/sim_parameters.h>
#include <sprockit/keyword_registration.h>
#include <sprockit/util.h>
#include <sprockit/stl_string.h>
#include <sprockit/basic_string_tokenizer.h>
#include <unistd.h>
#include <getopt.h>

ImplementFactory(sstmac::sw::app_manager);
RegisterNamespaces("app_manager");

namespace sstmac {
namespace sw {

std::map<int, app_manager*> app_manager::static_app_managers_;

SpktRegister("default", app_manager, app_manager);

app_manager::~app_manager()
{
  delete app_template_;
  delete allocator_;
  delete indexer_;
  delete linfo_;
}

void
app_manager::set_topology(hw::topology *top)
{
  top_ = top;
  indexer_->set_topology(top_);
  allocator_->set_topology(top_);
}

void
app_manager::init_factory_params(sprockit::sim_parameters* params)
{
  appname_ = params->get_param("name");

  if (params->has_param("core_affinities")) {
    params->get_vector_param("core_affinities", core_affinities_);
  }

  app_template_ = sw::app_factory::get_value(appname_, params);

  if (params->has_param("launch_cmd")){
    parse_launch_cmd(params);
  } else if (params->has_param("launch_dumpi_metaname")){
    std::string metafile = params->get_param("launch_dumpi_metaname");
    sw::dumpi_meta dm(metafile);
    nproc_ = dm.num_procs();
  } else {
    nproc_ = params->get_int_param("size");
    procs_per_node_ = params->get_optional_int_param("tasks_per_node", 1);
  }

  allocator_ = sw::node_allocator_factory::get_optional_param("launch_allocation",
               "first_available", params, rt_);

  indexer_ = sw::task_mapper_factory::get_optional_param("launch_indexing",
               "block", params, rt_);

  linfo_ = new sw::launch_info(
             app_template_,
             aid_,
             nproc_,
             core_affinities_);

  STATIC_INIT_INTERCONNECT(params)
}

app_manager*
app_manager::static_app_manager(int aid, sprockit::sim_parameters* params)
{
  spkt_throw(sprockit::unimplemented_error, "static_app_manager");
  if (!static_app_managers_[aid]){
    std::string app_namespace = sprockit::printf("app%d", aid);
    sprockit::sim_parameters* app_params = params->top_parent()->get_namespace(app_namespace);
    app_manager* mgr = app_manager_factory::get_optional_param(
          "launch_type", "skeleton", app_params, app_id(aid), 0/*no parallel runtime*/);
    static_app_managers_[aid] = mgr;
  }
  return static_app_managers_[aid];
}

void
app_manager::index_allocation(const ordered_node_set &allocation)
{
  indexer_->map_ranks(aid_, allocation,
               procs_per_node_, rank_to_node_indexing_,
               nproc_);

  if (sprockit::debug::slot_active(sprockit::dbg::indexing)){
    cout0 << sprockit::printf("Allocated and indexed %d nodes\n",
                rank_to_node_indexing_.size());
    int num_nodes = rank_to_node_indexing_.size();

    hw::structured_topology* regtop =
        safe_cast(hw::structured_topology, top_);

    for (int i=0; i < num_nodes; ++i){
      node_id nid = rank_to_node_indexing_[i];
      if (top_){
        hw::coordinates coords = regtop->node_coords(nid);
        cout0 << sprockit::printf("Rank %d -> nid%d %s\n",
            i, int(nid), stl_string(coords).c_str());
      } else {
         cout0 << sprockit::printf("Rank %d -> nid%d\n", i, int(nid));
      }
    }
  }

  int num_nodes = top_->num_nodes();
  node_to_rank_indexing_.resize(num_nodes);
  int num_ranks = rank_to_node_indexing_.size();
  for (int i=0; i < num_ranks; ++i){
    node_id nid = rank_to_node_indexing_[i];
    node_to_rank_indexing_[nid].push_back(i);
  }
}

void
app_manager::request_allocation(
  const sw::ordered_node_set& available,
  sw::ordered_node_set& allocation)
{
  int num_nodes = nproc_ / procs_per_node_;
  int remainder = nproc_ % procs_per_node_;
  if (remainder) {
    ++num_nodes;
  }
  allocator_->allocate(num_nodes, available, allocation);
}

void
app_manager::parse_launch_cmd(
  sprockit::sim_parameters* params,
  int& nproc,
  int& procs_per_node,
  std::vector<int>& affinities)
{
  if (params->has_param("launch_cmd")) {
    /** Check for an aprun launch */
    std::string launch_cmd = params->get_param("launch_cmd");
    size_t pos = launch_cmd.find_first_of(' ');
    std::string launcher;
    if (pos != std::string::npos) {
      launcher = launch_cmd.substr(0, pos);
    }
    else {
      launcher = launch_cmd;
    }

    if (launcher == "aprun") {
      parse_aprun(launch_cmd, nproc, procs_per_node, affinities);
      if (procs_per_node == -1) { //nothing given
        int ncores = params->get_optional_int_param("node_cores", 1);
        procs_per_node = ncores > nproc ? nproc : ncores;
      }
    }
    else {
      spkt_throw_printf(sprockit::value_error,
                        "invalid launcher %s given", launcher.c_str());
    }
  }
  else { //standard launch
    try {
      nproc = params->get_long_param("size");
      procs_per_node = params->get_optional_long_param("ntask_per_node", 1);
    }
    catch (sprockit::input_error& e) {
      cerr0 << "Problem reading app size parameter in skeleton app manager.\n"
               "If this is a DUMPI trace, set app name to dumpi.\n";
      throw e;
    }
  }
}

void
app_manager::parse_launch_cmd(sprockit::sim_parameters* params)
{
  parse_launch_cmd(params, nproc_, procs_per_node_, core_affinities_);
}

void
app_manager::parse_aprun(
  const std::string &cmd,
  int &nproc, int &nproc_per_node,
  std::vector<int>& core_affinities)
{
  int aprun_numa_containment = 0;
  option aprun_gopt[] = {
    { "n", required_argument, NULL, 'n' },
    { "N", required_argument, NULL, 'N' },
    { "ss", no_argument, &aprun_numa_containment, 1 },
    { "cc", required_argument, NULL, 'C' },
    { "S", required_argument, NULL, 'S' },
    { "d", required_argument, NULL, 'd' },
    { NULL, 0, NULL, '\0' }
  };

  char cmdline_str[200];
  ::strcpy(cmdline_str, cmd.c_str());
  char *argv[50];
  int argc = 0;
  char* pch = strtok(cmdline_str, " ");
  while (pch != 0) {
    argv[argc] = pch;
    ++argc;
    pch = strtok(0, " ");
  }

  int _nproc = -1;
  int _nproc_per_node = -1;
  int ch;
  int option_index = 1;
  std::string core_aff_str;
  optind = 1;
  while ((ch = getopt_long_only(argc, argv, "n:N:S:d:", aprun_gopt,
                                &option_index)) != -1) {
    switch (ch) {
      case 0: //this set an input flag
        break;
      case 'n':
        _nproc = atoi(optarg);
        break;
      case 'N':
        _nproc_per_node = atoi(optarg);
        break;
      case 'S':
        break;
      case 'C':
        core_aff_str = optarg;
        break;
      default:
        break;
    }
  }
  if (_nproc <= 0)
    throw sprockit::input_error(
      "aprun allocator did not receive a valid -n specification");

  if (_nproc_per_node == 0 || _nproc_per_node > _nproc)
    throw sprockit::input_error(
      "aprun allocator did not receive a valid -N specification");



  if (core_aff_str.size() > 0) { //we got a core affinity spec
    if (!core_affinities.empty()) {
      spkt_throw_printf(sprockit::illformed_error,
                       "skeleton_appmanager::parse_aprun: core affinities already assigned, cannot use -cc option");
    }

    std::string tosep = core_aff_str;
    std::deque<std::string> tok;
    std::string space = ",";
    pst::BasicStringTokenizer::tokenize(tosep, tok, space);
    std::deque<std::string>::const_iterator it, end = tok.end();
    for (it = tok.begin(); it != end; ++it) {
      std::string core = *it;
      core_affinities.push_back(atoi(core.c_str()));
    }
  }

  nproc = _nproc;
  nproc_per_node = _nproc_per_node;
}



}
}


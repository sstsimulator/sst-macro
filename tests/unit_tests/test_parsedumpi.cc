#include <sstmac/replacements/mpi.h>
#include <sstmac/util.h>
#include <sstmac/skeletons/undumpi/parsedumpi_callbacks.h>
#include <sstmac/software/process/app.h>
#include <sstmac/common/sstmac_env.h>
#include <sprockit/serializer.h>
#include <sprockit/output.h>
#include <sprockit/util.h>
#include <sprockit/test/test.h>
#include <sprockit/sim_parameters.h>


namespace sstmac {
namespace sw {

class parsedumpi_unit_test: public parsedumpi
{
 private:
  void run_test(parsedumpi_callbacks& cb, int num_dumpi, int* dumpi_reqs,
    std::vector<mpi_request*>& input_reqs, std::map<mpi_request*,int>& output_reqs);

 public:
  void
  consume_params(sprockit::sim_parameters *params){
    parsedumpi::consume_params(params);
  }

  virtual ~parsedumpi_unit_test() throw() {}

  app*
  clone_type(){
    return new parsedumpi_unit_test;
  }

  void run_test(UnitTest& unit);

  void skeleton_main();
};

SpktRegisterApp("parsedumpi_unit_test", parsedumpi_unit_test);

#define num_requests 10

void
parsedumpi_unit_test::skeleton_main()
{
  parsedumpi::skeleton_main();

  std::cout << "me!" << std::endl;
  UnitTest unit;

  run_test(unit);
  //sstmac::sw::parsedumpi_unit_test t;
  //t.run_test(unit);

  unit.validate();
}

void
parsedumpi_unit_test::run_test(parsedumpi_callbacks& cb, int num_dumpi, int *dumpi_reqs,
    std::vector<mpi_request*>& input_reqs, std::map<mpi_request*,int>& output_reqs)
{

  dumpi_request req_list[num_requests];
  for (int i=0; i < num_requests; ++i){
    mpi_request* req = new mpi_request(mpi_api::default_key_category);
    cb.store_request(i, req);
    req_list[i] = i;
    input_reqs[i] = req;
  }

  static const int sst_done[] = { 0, 1, 5, 8 };
  int sst_num_done = sizeof(sst_done) / sizeof(int);
  std::vector<int> sst_inds(sst_num_done);
  for (int i=0; i < sst_num_done; ++i){
    sst_inds[i] = sst_done[i];
    cb.nullify_request(sst_done[i]);
  }

  cb.remap_requests(num_requests, num_dumpi, dumpi_reqs, sst_inds, req_list);

  for (int i=0; i < num_requests; ++i){
    output_reqs[cb.get_request(i)] = i;
  }
  output_reqs[0] = 0;
}

void
parsedumpi_unit_test::run_test(UnitTest& unit)
{
  parsedumpi_callbacks cb(this);

  std::vector<mpi_request*> input_reqs(10);

  {
  int test_inds[] = {0, 1, 3, 6};
  std::map<mpi_request*,int> output_reqs;
  run_test(cb, 4, test_inds, input_reqs, output_reqs);
  //SST notdone[0] = 2, DUMPI notdone[0] = 2
  //SST notdone[1] = 3, DUMPI notdone[1] = 4
  //SST notdone[2] = 4, DUMPI notdone[2] = 5
  //SST notdone[3] = 6, DUMPI notdone[3] = 7
  //SST notdone[4] = 7, DUMPI notdone[4] = 8
  //SST notdone[5] = 9, DUMPI notdone[5] = 9
  assertEqual(unit, "input  0", output_reqs[input_reqs[0]], 0);
  assertEqual(unit, "input  1", output_reqs[input_reqs[1]], 0);
  assertEqual(unit, "input  2", output_reqs[input_reqs[2]], 2);
  assertEqual(unit, "input  3", output_reqs[input_reqs[3]], 0);
  assertEqual(unit, "input  4", output_reqs[input_reqs[4]], 3);
  assertEqual(unit, "input  5", output_reqs[input_reqs[5]], 4);
  assertEqual(unit, "input  6", output_reqs[input_reqs[6]], 0);
  assertEqual(unit, "input  7", output_reqs[input_reqs[7]], 6);
  assertEqual(unit, "input  8", output_reqs[input_reqs[8]], 7);
  assertEqual(unit, "input  9", output_reqs[input_reqs[9]], 9);
  }
}

} } //end namespaces

//int
//main()
//{
//  sstmac::sw::app* test = new sstmac::sw::parsedumpi_unit_test;
//  test->skeleton_main();
// return 0;
//}




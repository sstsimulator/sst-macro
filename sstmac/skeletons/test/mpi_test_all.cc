/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */

#include <sstmac/skeletons/test/mpi_test_all.h>
#include <sstmac/software/process/backtrace.h>
#include <sstmac/common/messages/value_payload.h>
#include <sstmac/common/messages/vector_payload.h>
#include <sstmac/common/thread_safe_int.h>
#include <sstmac/libraries/mpi/mpi_api.h>
#include <sprockit/sim_parameters.h>
#include <math.h>


namespace sstmac {
namespace sw {

static thread_safe_int errors_(0);

SpktRegister("mpi_test_all | mpi_test_all | mpi_test", app, mpi_test_all);

mpi_test_all::~mpi_test_all()
{

}

void
mpi_test_all::consume_params(sprockit::sim_parameters* params)
{
  if (params->has_param("testmpi_stop_at_errors")) {
    stop_at_errors_ = params->get_int_param("testmpi_stop_at_errors");
  }
  else {
    stop_at_errors_ = false;
  }

  sleep_time_ = params->get_optional_time_param("sleep_time", 1);
  compute_time_ = params->get_optional_time_param("compute_time", 2);
  print_all_ = params->get_optional_bool_param("mpi_test_print_all", true);
}

void
mpi_test_all::skeleton_main()
{
  SSTMACBacktrace("main");
  timestamp start = mpi()->init();
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();
  //dbg << "App " << rank << " initialized" << "\n";

  if (int(size) < 8) {
    spkt_throw_printf(sprockit::range_error,
                     "mpi_test_all needs at least 8 ranks to test on, only %d given",
                     int(size));
  }


  //----------first, lets do a ping pong with payload
  test_sendrecv();

  // ---- test nonblocking recv
  test_asynch();

  // ---- alright, let's try a broadcast
  test_bcast();

  // --- try a reduce
  test_reduce();

  // ---- sync up with barrier
  test_barrier();

  // ---- test allreduce
  test_allreduce();

  // ------ test scatter
  test_scatter();

  // ------ test gather
  test_gather();

  // ------ test scan
  test_scan();

  //------ test communicator functions
  test_comms();

  //------ test cartesian communicator functions
  test_cartcomms();

  // ------ test wait functions
  test_wait();

  // ------ test allgather
  test_allgather();

  // ------ test alltoall
  test_alltoall();

  // ------ test probing
  test_probe();

  // ------- test persistent
  test_persistent();

  // ------ test reduce_scatter
  // test_reducescatter();


  // ----- finalize
  mpi()->barrier(world);

  if (int(rank)== 0 && errors_ == 0) {
    if (print_all_) std::cout << "- Finished testing! test successful \n";
  }
  else if (int(rank)== 0 && errors_ != 0) {
    if (print_all_) std::cout << "- Finished testing! " << errors_
                 << " ERRORS - test not successful \n";
  }

  timestamp done = mpi()->finalize();

  timestamp t_total = done - start;
  if (rank == 0){
    ::printf("Total runtime %8.4fms\n", t_total.msec());
  }

}

void
mpi_test_all::test_sendrecv()
{
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();

  if (print_all_) std::cout << "testall[" << int(rank)
               << "]-- Testing payload send/recv \n";

  int count = 100; //arbitrary

  mpi_tag tag(1145);
  bool participant = true;
  if ((size.id_ % 2) && (int(rank)+ 1 >= int(size))) {
    // This is the odd-node-out -- communicating with no-one.
    participant = false;
  }
  if (participant) {
    mpi_id buddy(int(rank)^ 1); // 0<=>1, 2<=>3, etc.

    if ((rank.id_) & 1) {
      // even values of half-cycle plus rank.

      value_payload<int>::const_ptr senddata =
        value_payload<int>::construct(int(rank)* 1000);
      timestamp st = mpi()->send(count, mpi_type::mpi_double->id, buddy,
                                 tag, world, senddata);

    }
    else {
      mpi_status stat;
      mpi()->recv(count, mpi_type::mpi_double->id, buddy, tag, world, &stat);

      const value_payload<int>::const_ptr recvdata =
        ptr_test_cast(const value_payload<int>, stat.content());

      if (!recvdata) {
        errors_++;
        if (print_all_) std::cout << "ERROR at rank " << int(rank)
                     << ": SEND-RECV: NULL payload recv'd \n";

        if (stop_at_errors_) {
          spkt_throw(sprockit::spkt_error, "an error occurred in the application");
        }
      }
      else {
        if (recvdata->typed_data() != (buddy.id_ * 1000)) {
          errors_++;
          if (print_all_) std::cout << "ERROR at rank " << int(rank)
                       << ": SEND-RECV: received value "
                       << recvdata->data() << " and we should have got "
                       << int(buddy) * 1000 << "\n";

          if (stop_at_errors_) {
            spkt_throw(sprockit::spkt_error, "an error occurred in the application");
          }
        }

      }
    }

  }
}

void
mpi_test_all::test_scan()
{
  mpi_comm* world = mpi()->comm_world();
  mpi_id rank = world->rank();
  mpi_id size = world->size();

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing scan " << " \n";
  int count = 100;
  mpi()->scan(count, mpi_type::mpi_double->id, mpi_op::sum, world);
  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Through scan " << " \n";
}

void
mpi_test_all::test_barrier()
{
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing barrier " << " \n";

  if (int(rank)% 2 == 0) {
    timestamp t(2e-3);
    compute(t);
  }

  mpi()->barrier(world);

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Through barrier " << " \n";
}

void
mpi_test_all::test_reduce()
{
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();
  int count = 100; //arbitrary

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing reduce " << " \n";

  payload::const_ptr result;

  payload::const_ptr load = value_payload<int>::construct((1 << rank.id_));
  mpi()->reduce(count, mpi_type::mpi_double->id, mpi_op::sum, mpi_id(0), world,
                load, result);

  if (int(rank)== 0) {
    const value_payload<int>::const_ptr recvdata = ptr_safe_cast(const value_payload<int>, result);

    if (!recvdata) {
      errors_++;
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": REDUCE: NULL payload recv'd \n";
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error,
                  "an error occurred in the application");
      }
    }
    else {
      int expected = (int) pow(2.0, world->size()) - 1;
      if (recvdata->typed_data() != expected) {
        errors_++;
        if (print_all_) std::cout << "ERROR at rank " << int(rank)
                     << ": REDUCE: received value " << recvdata->data()
                     << " and we should have got " << expected << "\n";
        if (stop_at_errors_) {
          spkt_throw(sprockit::spkt_error, "an error occurred in the application");
        }
      }

    }
  }
}

void
mpi_test_all::test_asynch()
{
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();
  int count = 100; //arbitrary

  if (int(rank)== 0) {
    if (print_all_) std::cout << "testall[" << int(rank)
                 << "] -- Testing nonblocking recv/wait \n";
    mpi_id r(1);
    mpi_tag t(1);
    mpi()->send(count, mpi_type::mpi_double->id, r, t, world);

    if (print_all_) std::cout << "testall[" << int(rank)
                 << "]---- This should come SECOND -- \n\n\n";
  }
  else if (int(rank)== 1) {
    mpi_request* req;
    mpi_id r(0);
    mpi_tag t(1);
    mpi()->irecv(count, mpi_type::mpi_double->id, r, t, world, req);
    if (print_all_) std::cout << "testall[" << int(rank)
                 << "]---- This should come FIRST -- \n";
    mpi()->wait(&req);
    if (print_all_) std::cout << "testall[" << int(rank)
                 << "]---- This should come THIRD -- \n";

  }
}

void
mpi_test_all::test_allreduce()
{
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();
  int count = 100; //arbitrary

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing allreduce "
               << " \n";

  payload::const_ptr load = value_payload<int>::construct((1 << int(rank)));
  payload::const_ptr result;

  mpi()->allreduce(count, mpi_type::mpi_double->id, mpi_op::sum, world, load,
                   result);

  const value_payload<int>::const_ptr allrecvdata = ptr_test_cast(const value_payload<int>, result);

  if (!allrecvdata) {
    errors_++;
    if (print_all_) std::cout << "ERROR at rank " << int(rank)
                 << ": ALLREDUCE: NULL payload recv'd \n";
    if (stop_at_errors_) {
      spkt_throw(sprockit::spkt_error, "an error occurred in the application");
    }
  }
  else {
    int expected = (int) pow(2.0, world->size()) - 1;
    if (allrecvdata->typed_data() != (expected)) {
      errors_++;
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": ALLREDUCE: received value " << allrecvdata->data()
                   << " and we should have got " << expected << "\n";
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

  }
}

void
mpi_test_all::test_bcast()
{
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();
  int count = 100; //arbitrary

  if (print_all_) std::cout << "testall[" << int(rank)
               << "] -- Testing payload broadcast \n";

  payload::const_ptr load;

  if (int(rank)== 0) {
    load = value_payload<int>::construct(1234);
  }
  mpi_id root(0);
  mpi()->bcast(count, mpi_type::mpi_double->id, root, world, load);

  const value_payload<int>::const_ptr recvdata = ptr_safe_cast(const value_payload<int>, load);

  if (!recvdata) {
    errors_++;
    if (print_all_) std::cout << "ERROR at rank " << int(rank)
                 << ": NULL payload recv'd for BCAST \n";
    if (stop_at_errors_) {
      spkt_throw(sprockit::spkt_error, "an error occurred in the application");
    }
  }
  else {
    if (recvdata->typed_data() != (1234)) {
      errors_++;
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": BCAST: received value " << recvdata->typed_data()
                   << " and we should have got " << 1234 << "\n";
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

  }

  if (print_all_) std::cout << "testall[" << int(rank)
               << "] -- Testing vector payload broadcast \n";

  count = 4;
  load = payload::null();
  if (int(rank)== 0) {
    std::vector<int> pay;
    pay.push_back(1);
    pay.push_back(2);
    pay.push_back(3);
    pay.push_back(4);
    load = vector1_payload<int>::construct(pay, 4);
  }

  mpi()->bcast(count, mpi_type::mpi_int->id, root, world, load);

  const vector1_payload<int>::const_ptr recvdata2 = ptr_test_cast(const vector1_payload<int>, load);

  if (!recvdata2) {
    errors_++;
    if (print_all_) std::cout << "ERROR at rank " << int(rank)
                 << ": NULL payload recv'd for BCAST \n";
    if (stop_at_errors_) {
      spkt_throw(sprockit::spkt_error, "an error occurred in the application");
    }
  }
  else {
    const int* pay = recvdata2->typed_data();
    if (pay[0] != 1 || pay[1] != 2 || pay[2] != 3 || pay[3] != 4) {
      errors_++;
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": BCAST: received value " << pay[0] << ", " << pay[1]
                   << "," << pay[2] << ", " << pay[3]
                   << " and we should have got 1,2,3,4 " << "\n";
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

  }

  if (print_all_) std::cout << "testall[" << int(rank)
               << "] -- Testing buffer pointer payload broadcast \n";

  count = 4;
  load = payload::null();
  if (int(rank)== 0) {
    int pay[4];
    pay[0] = 1;
    pay[1] = 2;
    pay[2] = 3;
    pay[3] = 4;
    load = vector1_payload<int, int*>::construct(pay, 4);
  }

  mpi()->bcast(count, mpi_type::mpi_int->id, root, world, load);

  typedef vector1_payload<int, int*> Vector1Payload;
  const Vector1Payload::const_ptr recvdata3 = ptr_safe_cast(const Vector1Payload, load);

  if (!recvdata3) {
    errors_++;
    if (print_all_) std::cout << "ERROR at rank " << int(rank)
                 << ": NULL payload recv'd for BCAST \n";
    if (stop_at_errors_) {
      spkt_throw(sprockit::spkt_error, "an error occurred in the application");
    }
  }
  else {
    const int* pay = recvdata3->typed_data();
    if (pay[0] != 1 || pay[1] != 2 || pay[2] != 3 || pay[3] != 4) {
      errors_++;
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": BCAST: received value " << pay[0] << ", " << pay[1]
                   << "," << pay[2] << ", " << pay[3]
                   << " and we should have got 1,2,3,4 " << "\n";
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      };
    }

  }
}

void
mpi_test_all::test_scatter()
{
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();
  int count = 100; //arbitrary

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing scatter " << " \n";

  std::vector<payload::const_ptr> vals;
  std::vector<int> vcounts;


  for (int i = 0; i < size; i++) {
    if (int(rank)== 0) {
      int val = 1 << i;
      vals.push_back(value_payload<int>::construct(val));
      vcounts.push_back(count);
    }
    else {
      vals.push_back(payload::const_ptr());
    }
  }

  payload::const_ptr result;

  mpi()->scatter(count, mpi_type::mpi_double->id, count, mpi_type::mpi_double->id,
                 mpi_id(0), world, vals, result);

  const value_payload<int>::const_ptr scatterdata = ptr_safe_cast(const value_payload<int>, result);

  if (!scatterdata) {
    errors_++;
    if (print_all_) std::cout << "ERROR at rank " << int(rank)
                 << ": NULL payload recv'd for SCATTER \n";
    if (stop_at_errors_) {
      spkt_throw(sprockit::spkt_error, "an error occurred in the application");
    }
  }
  else {
    int expected = (int) (1 << int(rank));
    if (scatterdata->typed_data() != (expected)) {
      errors_++;
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": SCATTER: received value " << scatterdata->data()
                   << " and we should have got " << expected << "\n";
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }
  }

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing scatterv "
               << " \n";
  mpi()->scatterv(vcounts, mpi_type::mpi_double->id, count, mpi_type::mpi_double->id,
                  mpi_id(0), world, vals, result);

  const value_payload<int>::const_ptr scatterdata2 = ptr_safe_cast(const value_payload<int>, result);

  if (!scatterdata2) {
    errors_++;
    if (print_all_) std::cout << "ERROR at rank " << int(rank)
                 << ": NULL payload recv'd for SCATTERV \n";
    if (stop_at_errors_) {
      spkt_throw(sprockit::spkt_error, "an error occurred in the application");
    }
  }
  else {
    int expected = (int) (1 << int(rank));
    if (scatterdata2->typed_data() != (expected)) {
      errors_++;
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": SCATTERV: received value " << scatterdata2->typed_data()
                   << " and we should have got " << expected << "\n";
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }
  }

}

void
mpi_test_all::test_gather()
{
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();
  int count = 100; //arbitrary

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing gather " << " \n";

  std::vector<payload::const_ptr> vals;

  payload::const_ptr load = value_payload<int>::construct(1 << int(rank));

  mpi()->gather(count, mpi_type::mpi_double->id, count, mpi_type::mpi_double->id,
                mpi_id(0), world, load, vals);

  if (int(rank)== 0) {
    std::vector<payload::const_ptr>::iterator it, end = vals.end();

    if ((int)vals.size() != int(world->size())) {
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": GATHER: result vector size (" << vals.size()
                   << ") does not match world comm size (" << int(world->size())
                   << ") \n";
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

    int i = 0;
    for (it = vals.begin(); it != end; it++) {
      if (*it) {
        int expected = (int) (1 << i);
        const value_payload<int>::const_ptr
        scatterdata = ptr_safe_cast(const value_payload<int>, *it);

        if (scatterdata->typed_data() != (expected)) {
          errors_++;
          if (print_all_) std::cout << "ERROR at rank " << int(rank)
                       << ": GATHER: received value "
                       << scatterdata->typed_data()
                       << " and we should have got " << expected << "\n";
          if (stop_at_errors_) {
            spkt_throw(sprockit::spkt_error, "an error occurred in the application");
          }
        }
      }
      else {
        errors_++;
        if (print_all_) std::cout << "ERROR at rank " << int(rank)
                     << ": NULL payload recv'd for GATHER \n";
        if (stop_at_errors_) {
          spkt_throw(sprockit::spkt_error, "an error occurred in the application");
        }
      }

      i++;

    }

  }

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing gatherv " << " \n";

  std::vector<payload::const_ptr> vals2;

  payload::const_ptr load2 = value_payload<int>::construct(1 << int(rank));
  std::vector<int> vcounts;

  for (int i = 0; i < int(size); i++) {
    vcounts.push_back(count);
  }

  mpi()->gatherv(count, mpi_type::mpi_double->id, vcounts, mpi_type::mpi_double->id,
                 mpi_id(0), world, load2, vals2);

  if (int(rank)== 0) {
    std::vector<payload::const_ptr>::iterator it, end = vals2.end();

    if ((int)vals2.size() != int(world->size())) {
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": GATHERV: result vector size (" << vals.size()
                   << ") does not match world comm size (" << int(world->size())
                   << ") \n";
    }

    int i = 0;
    for (it = vals2.begin(); it != end; it++) {
      if (*it) {
        int expected = (int) (1 << i);
        const value_payload<int>::const_ptr
        scatterdata = ptr_safe_cast(const value_payload<int>, *it);

        if (scatterdata->typed_data() != (expected)) {
          errors_++;
          if (print_all_) std::cout << "ERROR at rank " << int(rank)
                       << ": GATHERV: received value "
                       << scatterdata->typed_data()
                       << " and we should have got " << expected << "\n";
          if (stop_at_errors_) {
            spkt_throw(sprockit::spkt_error, "an error occurred in the application");
          }
        }
      }
      else {
        errors_++;
        if (print_all_) std::cout << "ERROR at rank " << int(rank)
                     << ": NULL payload recv'd for GATHERV \n";
        if (stop_at_errors_) {
          spkt_throw(sprockit::spkt_error, "an error occurred in the application");
        }
      }

      i++;

    }

  }
}

void
mpi_test_all::test_allgather()
{
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();
  int count = 100; //arbitrary

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing allgather "
               << " \n";

  std::vector<payload::const_ptr> vals;

  payload::const_ptr load = value_payload<int>::construct(1 << int(rank));

  mpi()->allgather(count, mpi_type::mpi_double->id, count, mpi_type::mpi_double->id,
                   world, load, vals);

  std::vector<payload::const_ptr>::iterator it, end = vals.end();

  if ((int)vals.size() != int(world->size())) {
    errors_++;
    if (print_all_) std::cout << "ERROR at rank " << int(rank)
                 << ": ALLGATHER: result vector size (" << vals.size()
                 << ") does not match world comm size (" << int(world->size())
                 << ") \n";
    if (stop_at_errors_) {
      spkt_throw(sprockit::spkt_error, "an error occurred in the application");
    }
  }

  int i = 0;
  for (it = vals.begin(); it != end; it++) {
    if (*it) {
      int expected = (int) (1 << i);
      const value_payload<int>::const_ptr scatterdata = ptr_safe_cast(const value_payload<int>, *it);

      if (scatterdata->typed_data() != (expected)) {
        errors_++;
        if (print_all_) std::cout << "ERROR at rank " << int(rank)
                     << ": ALLGATHER: received value "
                     << scatterdata->typed_data() << " and we should have got "
                     << expected << "\n";
        if (stop_at_errors_) {
          spkt_throw(sprockit::spkt_error, "an error occurred in the application");
        }
      }
    }
    else {
      errors_++;
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": NULL payload recv'd for ALLGATHER \n";
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

    i++;

  }

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing allgatherv "
               << " \n";

  std::vector<payload::const_ptr> vals2;

  payload::const_ptr load2 = value_payload<int>::construct(1 << int(rank));
  std::vector<int> vcounts;

  for (int i = 0; i < int(size); i++) {
    vcounts.push_back(count);
  }

  mpi()->allgatherv(count, mpi_type::mpi_double->id, vcounts,
                    mpi_type::mpi_double->id, world, load2, vals2);

  std::vector<payload::const_ptr>::iterator it2, end2 = vals2.end();

  if ((int)vals2.size() != int(world->size())) {
    errors_++;
    if (print_all_) std::cout << "ERROR at rank " << int(rank)
                 << ": ALLGATHERV: result vector size (" << vals.size()
                 << ") does not match world comm size (" << int(world->size())
                 << ") \n";
    if (stop_at_errors_) {
      spkt_throw(sprockit::spkt_error, "an error occurred in the application");
    }
  }

  i = 0;
  for (it2 = vals2.begin(); it2 != end2; it2++) {
    if (*it2) {
      int expected = (int) (1 << i);
      const value_payload<int>::const_ptr scatterdata = ptr_safe_cast(const value_payload<int>, *it2);

      if (scatterdata->typed_data() != (expected)) {
        errors_++;
        if (print_all_) std::cout << "ERROR at rank " << int(rank)
                     << ": ALLGATHERV: received value "
                     << scatterdata->typed_data() << " and we should have got "
                     << expected << "\n";
        if (stop_at_errors_) {
          spkt_throw(sprockit::spkt_error, "an error occurred in the application");
        }
      }
    }
    else {
      errors_++;
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": NULL payload recv'd for ALLGATHERV \n";
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

    i++;

  }

}

void
mpi_test_all::test_alltoall()
{
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();
  int count = 100; //arbitrary

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing alltoall "
               << " \n";

  std::vector<payload::const_ptr> vals;

  for (int i = 0; i < int(size); i++) {
    int val = 1 << i;
    vals.push_back(value_payload<int>::construct(val));
  }

  std::vector<payload::const_ptr> result;

  mpi()->alltoall(count, mpi_type::mpi_double->id, count, mpi_type::mpi_double->id,
                  world, vals, result);

  std::vector<payload::const_ptr>::iterator it, end = result.end();

  if ((int)result.size() != int(world->size())) {
    errors_++;
    if (print_all_) std::cout << "ERROR at rank " << int(rank)
                 << ": ALLTOALL: result vector size (" << result.size()
                 << ") does not match world comm size (" << int(world->size())
                 << ") \n";
    if (stop_at_errors_) {
      spkt_throw(sprockit::spkt_error, "an error occurred in the application");
    }
  }

  int i = 0;
  for (it = result.begin(); it != end; it++) {
    if (*it) {
      int expected = (int) (1 << int(rank));
      const value_payload<int>::const_ptr scatterdata = ptr_safe_cast(const value_payload<int>, *it);

      if (scatterdata->typed_data() != (expected)) {
        errors_++;
        if (print_all_) std::cout << "ERROR at rank " << int(rank)
                     << ": ALLTOALL: received value " << scatterdata->typed_data()
                     << " and we should have got " << expected << "\n";
        if (stop_at_errors_) {
          spkt_throw(sprockit::spkt_error, "an error occurred in the application");
        }
      }
    }
    else {
      errors_++;
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": NULL payload recv'd for ALLTOALL for rank " << i
                   << " \n";
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

    i++;

  }

}

void
mpi_test_all::test_comms()
{
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing comm create \n";

  std::vector<task_id> ids;
  std::vector<task_id> idodd;
  for (int i = 0; i < int(size); i++) {
    if (i % 2 == 0) {
      ids.push_back(task_id(i));
    }
    else {
      idodd.push_back(task_id(i));
    }
  }

  mpi_group* group = new mpi_group(ids);

  mpi_comm* evens;
  mpi()->comm_create(world, group, evens);

  if (int(rank)% 2 == 0) {
    if (evens == mpi_comm::comm_null) {
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": create_comm returned null comm when it should be a real thing \n";
      errors_++;
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }
    else {

      if (int(evens->rank()) != int(rank)/ 2) {
        errors_++;
        if (print_all_) std::cout << "testall[" << int(rank)
                     << "] -- my new comm rank is " << int(evens->rank())
                     << " and it should be " << (int(rank)/ 2) << "\n";
        if (stop_at_errors_) {
          spkt_throw(sprockit::spkt_error, "an error occurred in the application");
        }
      }

      if (int(evens->id()) != 1) {
        if (print_all_) std::cout << "ERROR at rank " << int(rank)
                     << ": create produced id " << int(evens->id())
                     << " and it should have been " << 1 << " \n";
        errors_++;
        if (stop_at_errors_) {
          spkt_throw(sprockit::spkt_error, "an error occurred in the application");
        }
      }

    }

    if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing comm dup \n";

    mpi_comm* duped;
    mpi()->comm_dup(evens, duped);
    mpi_comm_id shouldbe(
      (int(rank)% 2 == 0) ? int(evens->id()) + 1 : int(evens->id()) + 2);

    if (duped->id() != shouldbe) {
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": create_dup produced id " << int(duped->id())
                   << " and it should have been " << int(shouldbe) << " \n";
      errors_++;
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

    if (duped->size() != evens->size()) {
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": create_dup produced comm with size "
                   << int(duped->size()) << " and it should have been "
                   << int(evens->size()) << " \n";
      errors_++;
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

    if (duped->rank() != evens->rank()) {
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": create_dup produced comm where my rank is "
                   << int(duped->rank()) << " and it should have been "
                   << int(evens->rank()) << " \n";
      errors_++;
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

    if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing comm split \n";

    mpi_comm* mynewsplit;
    int newkey = duped->rank();
    mpi()->comm_split(duped, ((int(rank)== 2) ? 0 : 1), newkey, mynewsplit);
    int sizecheck = (int(rank)== 2) ? 1 : ((int(duped->size())) - 1);

    if (mynewsplit == mpi_comm::comm_null) {
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": create_split returned null comm \n";
      errors_++;
    }
    else {

      if (int(mynewsplit->size()) != sizecheck) {
        if (print_all_) std::cout << "ERROR at rank " << int(rank)
                     << ": create_split produced comm with size "
                     << int(mynewsplit->size()) << " and it should have been "
                     << sizecheck << " \n";
        errors_++;
        if (stop_at_errors_) {
          spkt_throw(sprockit::spkt_error, "an error occurred in the application");
        }
      }
    }

    mpi()->comm_free(evens);
    mpi()->comm_free(duped);
    mpi()->comm_free(mynewsplit);
  }
  else {
    if (evens != mpi_comm::comm_null) {
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": create_comm returned a real comm when it should have returned the null comm \n";
      errors_++;
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

  }
}

void
mpi_test_all::test_cartcomms()
{
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing cart comms \n";

  mpi_comm* cart;
  int dims[2];
  int per[2];
  dims[0] = 2;
  dims[1] = int(size) / 2;
  per[0] = 1;
  per[1] = 0;
  mpi()->mpi_cart_create(world, 2, dims, per, 0, cart);

  int coords[2];
  mpi()->mpi_cart_coords(cart, rank, 2, coords);

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- my coords are (" << coords[0] <<
               "," << coords[1] << ")\n";

  int shiftr;
  int shiftl;

  mpi()->mpi_cart_shift(cart, 1, 2, &shiftr, &shiftl);

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- shift: source = " << shiftr <<
               ", dest = " << shiftl << "\n";

}

void
mpi_test_all::test_wait()
{
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();

  int count = 100;
  mpi_tag tag(698);

  mpi_id sender(5);

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing wait any \n";

  if (int(rank)== 0) {

    std::vector<mpi_request*> reqs;

    for (int i = 1; i < int(size); i++) {
      mpi_request* req;
      mpi()->irecv(count, mpi_type::mpi_double->id, mpi_id(i), tag, world, req);
      reqs.push_back(req);
    }

    int index;
    mpi()->waitany(reqs, index);

    if (index != int(sender) - 1) {
      //subtract 1 because I didn't push a mpirequest in the vector for myself
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": waitany unblocked with index " << index
                   << " and it should have been sent from " << int(sender)
                   << "\n";
      errors_++;
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }
  }
  else if (rank == sender) {
    mpi()->send(count, mpi_type::mpi_double->id, mpi_id(0), tag, world);
  }

  mpi()->barrier(world);

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing wait some \n";

  mpi_tag tag2(699);

  if (int(rank)== 0) {
    sleep(sleep_time_); //lag me, so the others have a chance to send

    std::vector<mpi_request*> reqs;

    for (int i = 1; i < int(size); i++) {
      mpi_request* req;
      mpi()->irecv(count, mpi_type::mpi_double->id, mpi_id(i), tag2, world,
                   req);
      reqs.push_back(req);
    }

    std::vector<int> index;
    int nrecved = 0;
    int nexpected = int(size) / 2 - 1;
    while (nrecved < nexpected) {
      mpi()->waitsome(reqs, index);
      nrecved += index.size();
    }

    if (nrecved != nexpected) {
      //subtract 1 for myself
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": waitsome unblocked with number of indices "
                   << nrecved << " and it should have been " << nexpected << "\n";
      errors_++;
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

  }
  else if (int(rank)% 2 == 0) {
    mpi()->send(count, mpi_type::mpi_double->id, mpi_id(0), tag2, world);
  }

  mpi()->barrier(world);

}

void
mpi_test_all::test_reducescatter()
{
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing reduce_scatter \n";

  std::vector<int> recvcounts;

  for (int i = 0; i < int(size); i++) {
    recvcounts.push_back(10 * int(rank));
  }

  mpi()->reduce_scatter(recvcounts, mpi_type::mpi_double->id, mpi_op::sum, world);

  if (print_all_) std::cout << "testall[" << int(rank)
               << "]: reduce_scatter: recv counts: ";
  for (int i = 0; i < recvcounts.size(); i++) {
    if (print_all_) std::cout << recvcounts[i] << ", ";
  }

  if (print_all_) std::cout << "\n";

}

void
mpi_test_all::test_probe()
{
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();

  int count = 100;
  mpi_tag tag(713);
  mpi_tag tag2(714);

  mpi()->barrier(world);

  mpi_id sender(5);

  if (print_all_) std::cout << "testall[" << int(rank)<< "] -- Testing probe \n";

  if (int(rank)== 0) {
    std::vector<mpi_request*> reqs;
    for (int i = 1; i < int(size); i++) {
      mpi_request* req;
      mpi()->irecv(count, mpi_type::mpi_double->id, mpi_id(i), tag, world, req);
      reqs.push_back(req);
    }

    int index;
    bool flag;
    std::vector<int> indices;

    mpi()->test(&reqs[sender], flag);

    if (flag) {
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": test returned completed, and it probably should not have \n";
      errors_++;
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

    mpi()->testsome(reqs, indices);

    if (indices.size() > 0) {
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": testsome returned completed, and it probably should not have \n";
      errors_++;
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

    mpi()->testany(reqs, index, flag);

    if (flag) {
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": testany returned completed, and it probably should not have \n";
      errors_++;
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

    mpi_status ignore;
    mpi()->iprobe(sender, tag, world, flag, &ignore);

    if (flag) {
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": iprobe returned completed, and it probably should not have \n";
      errors_++;
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

    // -------  here is the blocking probe ----------- //
    mpi()->probe(sender, tag, world, &ignore);

    int reqidx = sender - 1;
    mpi()->test(&reqs[reqidx], flag);

    if (!flag) {
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": test returned NOT completed, and it should not have \n";
      errors_++;
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

    mpi()->testsome(reqs, indices);

    if (indices.size() != 0) {
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": testsome returned indices with size "
                   << indices.size() << ", and it should have been 0\n";
      errors_++;
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

    sleep(sleep_time_);

    //make sure second message comes in
    mpi()->probe(sender, tag2, world, &ignore);
    mpi()->iprobe(sender, tag2, world, flag, &ignore);

    if (!flag) {
      if (print_all_) std::cout << "ERROR at rank " << int(rank)
                   << ": iprobe returned NOT completed, and it should not have \n";
      errors_++;
      if (stop_at_errors_) {
        spkt_throw(sprockit::spkt_error, "an error occurred in the application");
      }
    }

    mpi()->recv(count, mpi_type::mpi_double->id, sender, tag2, world, mpi_status::ignore);
  }
  else if (rank == sender) {
    compute(compute_time_);
    mpi()->send(count, mpi_type::mpi_double->id, mpi_id(0), tag, world);
    mpi()->send(count, mpi_type::mpi_double->id, mpi_id(0), tag2, world);
  }

  mpi()->barrier(world);

}

void
mpi_test_all::test_persistent()
{
  mpi_comm* world = mpi()->comm_world();

  mpi_id rank = world->rank();
  mpi_id size = world->size();

  int count = 100;
  mpi_tag tag(715);

  if (print_all_) std::cout << "testall[" << int(rank)
               << "] -- Testing persistent actions \n";

  mpi_id sender(6);
  mpi_id recver(1);

  mpi_request* asend;
  mpi_request* arecv;

  if (rank == sender) {
    mpi()->send_init(count, mpi_type::mpi_double->id, recver, tag, world,
                     asend);
  }

  if (rank == recver) {
    mpi()->recv_init(count, mpi_type::mpi_double->id, sender, tag, world,
                     arecv);
  }

  if (print_all_) std::cout << "testall[" << int(rank)
               << "] -- Persistent: Haven't started them yet \n";

  if (rank == sender) {
    mpi()->start(asend);
  }

  if (rank == recver) {
    mpi()->start(arecv);
  }

  if (print_all_) std::cout << "testall[" << int(rank)
               << "] -- Persistent: OK, I started them \n";

  if (rank == sender) {
    mpi()->wait(&asend);
  }

  if (rank == recver) {
    mpi()->wait(&arecv);
  }

  if (print_all_) std::cout << "testall[" << int(rank)
               << "] -- Persistent: I'm through \n";

}

}
} //end of namespace sstmac


/**
Copyright 2009-2018 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2018, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#include <sstmac/skeletons/otf2_trace_replay/callqueue.h>
#include <sstmac/skeletons/otf2_trace_replay/otf2_trace_replay.h>

using namespace std;

#if 1
    #define TRIGGER_PRINT(...) cerr << "TRIGGERED CALL (#" << app->rank() << "): " << __VA_ARGS__ << endl;
#else
    #define TRIGGER_PRINT(...)
#endif

/******************************************************************************
 *  CallQueue functions
 */

int
CallQueue::CallReady(MpiCall* call) {
  int triggered = 0;
  call->isready = true;

  // when a call at the front of the queue is ready, there may be a
  // cascade of other ready calls behind it.
  while (call_queue.size() > 0 && call_queue.front().IsReady()) {
    auto& front = call_queue.front();
    front.Trigger();

    if (app->PrintMpiCalls()) {
       TRIGGER_PRINT(front.ToString());
    }
    call_queue.pop();
    triggered++;
  }

  return triggered;
}

MpiCall*
CallQueue::FindRequest(MPI_Request req) const
{
  auto iter = request_map.find(req);
  if (iter == request_map.end()){
    spkt_abort_printf("Rank %d cannot find request %ld\n",
                      app->GetMpi()->rank(), req);
  }
  return iter->second;
}


void
MpiCall::Trigger() {
  app->StartMpi(GetStart());
  if (on_trigger){
    on_trigger();
  }
  app->EndMpi(GetEnd());
}

sstmac::timestamp
MpiCall::convert_time(const OTF2_TimeStamp ts) const
{
  const auto start_offset = app->otf2_clock_properties.globalOffset;
  const auto ticks_per_second = app->otf2_clock_properties.timerResolution;
  return sstmac::timestamp(((double(ts) - start_offset)/ticks_per_second));
}

const char*
MpiCall::name(MPI_CALL_ID id)
{
#define enum_case(x) case ID_##x: return #x
  switch(id){
enum_case(NULL);
enum_case(MPIX_Comm_agree);
enum_case(MPI_File_set_errhandler);
enum_case(MPI_Rsend);
enum_case(MPIX_Comm_failure_ack);
enum_case(MPI_File_set_info);
enum_case(MPI_Rsend_init);
enum_case(MPIX_Comm_failure_get_acked);
enum_case(MPI_File_set_size);
enum_case(MPI_Scan);
enum_case(MPIX_Comm_revoke);
enum_case(MPI_File_set_view);
enum_case(MPI_Scatter);
enum_case(MPIX_Comm_shrink);
enum_case(MPI_File_sync);
enum_case(MPI_Scatterv);
enum_case(MPI_Abort);
enum_case(MPI_File_write);
enum_case(MPI_Send);
enum_case(MPI_Accumulate);
enum_case(MPI_File_write_all);
enum_case(MPI_Send_init);
enum_case(MPI_Add_error_class);
enum_case(MPI_File_write_all_begin);
enum_case(MPI_Sendrecv);
enum_case(MPI_Add_error_code);
enum_case(MPI_File_write_all_end);
enum_case(MPI_Sendrecv_replace);
enum_case(MPI_Add_error_string);
enum_case(MPI_File_write_at);
enum_case(MPI_Ssend);
enum_case(MPI_Address);
enum_case(MPI_File_write_at_all);
enum_case(MPI_Ssend_init);
enum_case(MPI_Aint_add);
enum_case(MPI_File_write_at_all_begin);
enum_case(MPI_Start);
enum_case(MPI_Aint_diff);
enum_case(MPI_File_write_at_all_end);
enum_case(MPI_Startall);
enum_case(MPI_Allgather);
enum_case(MPI_File_write_ordered);
enum_case(MPI_Status_set_cancelled);
enum_case(MPI_Allgatherv);
enum_case(MPI_File_write_ordered_begin);
enum_case(MPI_Status_set_elements);
enum_case(MPI_Alloc_mem);
enum_case(MPI_File_write_ordered_end);
enum_case(MPI_Status_set_elements_x);
enum_case(MPI_Allreduce);
enum_case(MPI_File_write_shared);
enum_case(MPI_T_category_changed);
enum_case(MPI_Alltoall);
enum_case(MPI_Finalize);
enum_case(MPI_T_category_get_categories);
enum_case(MPI_Alltoallv);
enum_case(MPI_Finalized);
enum_case(MPI_T_category_get_cvars);
enum_case(MPI_Alltoallw);
enum_case(MPI_Free_mem);
enum_case(MPI_T_category_get_info);
enum_case(MPI_Attr_delete);
enum_case(MPI_Gather);
enum_case(MPI_T_category_get_num);
enum_case(MPI_Attr_get);
enum_case(MPI_Gatherv);
enum_case(MPI_T_category_get_pvars);
enum_case(MPI_Attr_put);
enum_case(MPI_Get);
enum_case(MPI_T_cvar_get_info);
enum_case(MPI_Barrier);
enum_case(MPI_Get_accumulate);
enum_case(MPI_T_cvar_get_num);
enum_case(MPI_Bcast);
enum_case(MPI_Get_address);
enum_case(MPI_T_cvar_handle_alloc);
enum_case(MPI_Bsend);
enum_case(MPI_Get_count);
enum_case(MPI_T_cvar_handle_free);
enum_case(MPI_Bsend_init);
enum_case(MPI_Get_elements);
enum_case(MPI_T_cvar_read);
enum_case(MPI_Buffer_attach);
enum_case(MPI_Get_elements_x);
enum_case(MPI_T_cvar_write);
enum_case(MPI_Buffer_detach);
enum_case(MPI_Get_library_version);
enum_case(MPI_T_enum_get_info);
enum_case(MPI_Cancel);
enum_case(MPI_Get_processor_name);
enum_case(MPI_T_enum_get_item);
enum_case(MPI_Cart_coords);
enum_case(MPI_Get_version);
enum_case(MPI_T_finalize);
enum_case(MPI_Cart_create);
enum_case(MPI_Graph_create);
enum_case(MPI_T_init_thread);
enum_case(MPI_Cart_get);
enum_case(MPI_Graph_get);
enum_case(MPI_T_pvar_get_info);
enum_case(MPI_Cart_map);
enum_case(MPI_Graph_map);
enum_case(MPI_T_pvar_get_num);
enum_case(MPI_Cart_rank);
enum_case(MPI_Graph_neighbors);
enum_case(MPI_T_pvar_handle_alloc);
enum_case(MPI_Cart_shift);
enum_case(MPI_Graph_neighbors_count);
enum_case(MPI_T_pvar_handle_free);
enum_case(MPI_Cart_sub);
enum_case(MPI_Graphdims_get);
enum_case(MPI_T_pvar_read);
enum_case(MPI_Cartdim_get);
enum_case(MPI_Grequest_complete);
enum_case(MPI_T_pvar_readreset);
enum_case(MPI_Close_port);
enum_case(MPI_Grequest_start);
enum_case(MPI_T_pvar_reset);
enum_case(MPI_Comm_accept);
enum_case(MPI_Group_compare);
enum_case(MPI_T_pvar_session_create);
enum_case(MPI_Comm_call_errhandler);
enum_case(MPI_Group_difference);
enum_case(MPI_T_pvar_session_free);
enum_case(MPI_Comm_compare);
enum_case(MPI_Group_excl);
enum_case(MPI_T_pvar_start);
enum_case(MPI_Comm_connect);
enum_case(MPI_Group_free);
enum_case(MPI_T_pvar_stop);
enum_case(MPI_Comm_create);
enum_case(MPI_Group_incl);
enum_case(MPI_T_pvar_write);
enum_case(MPI_Comm_create_errhandler);
enum_case(MPI_Group_intersection);
enum_case(MPI_Test);
enum_case(MPI_Comm_create_group);
enum_case(MPI_Group_range_excl);
enum_case(MPI_Test_cancelled);
enum_case(MPI_Comm_create_keyval);
enum_case(MPI_Group_range_incl);
enum_case(MPI_Testall);
enum_case(MPI_Comm_delete_attr);
enum_case(MPI_Group_rank);
enum_case(MPI_Testany);
enum_case(MPI_Comm_disconnect);
enum_case(MPI_Group_size);
enum_case(MPI_Testsome);
enum_case(MPI_Comm_dup);
enum_case(MPI_Group_translate_ranks);
enum_case(MPI_Topo_test);
enum_case(MPI_Comm_dup_with_info);
enum_case(MPI_Group_union);
enum_case(MPI_Type_commit);
enum_case(MPI_Comm_free);
enum_case(MPI_Iallgather);
enum_case(MPI_Type_contiguous);
enum_case(MPI_Comm_free_keyval);
enum_case(MPI_Iallgatherv);
enum_case(MPI_Type_create_darray);
enum_case(MPI_Comm_get_attr);
enum_case(MPI_Iallreduce);
enum_case(MPI_Type_create_hindexed);
enum_case(MPI_Comm_get_errhandler);
enum_case(MPI_Ialltoall);
enum_case(MPI_Type_create_hindexed_block);
enum_case(MPI_Comm_get_info);
enum_case(MPI_Ialltoallv);
enum_case(MPI_Type_create_hvector);
enum_case(MPI_Comm_get_name);
enum_case(MPI_Ialltoallw);
enum_case(MPI_Type_create_indexed_block);
enum_case(MPI_Comm_get_parent);
enum_case(MPI_Ibarrier);
enum_case(MPI_Type_create_keyval);
enum_case(MPI_Comm_group);
enum_case(MPI_Ibcast);
enum_case(MPI_Type_create_resized);
enum_case(MPI_Comm_idup);
enum_case(MPI_Ibsend);
enum_case(MPI_Type_create_struct);
enum_case(MPI_Comm_join);
enum_case(MPI_Iexscan);
enum_case(MPI_Type_create_subarray);
enum_case(MPI_Comm_rank);
enum_case(MPI_Igather);
enum_case(MPI_Type_delete_attr);
enum_case(MPI_Comm_remote_group);
enum_case(MPI_Igatherv);
enum_case(MPI_Type_dup);
enum_case(MPI_Comm_remote_size);
enum_case(MPI_Improbe);
enum_case(MPI_Type_extent);
enum_case(MPI_Comm_set_attr);
enum_case(MPI_Imrecv);
enum_case(MPI_Type_free);
enum_case(MPI_Comm_set_errhandler);
enum_case(MPI_Ineighbor_allgather);
enum_case(MPI_Type_free_keyval);
enum_case(MPI_Comm_set_info);
enum_case(MPI_Ineighbor_allgatherv);
enum_case(MPI_Type_get_attr);
enum_case(MPI_Comm_set_name);
enum_case(MPI_Ineighbor_alltoall);
enum_case(MPI_Type_get_contents);
enum_case(MPI_Comm_size);
enum_case(MPI_Ineighbor_alltoallv);
enum_case(MPI_Type_get_envelope);
enum_case(MPI_Comm_spawn);
enum_case(MPI_Ineighbor_alltoallw);
enum_case(MPI_Type_get_extent);
enum_case(MPI_Comm_spawn_multiple);
enum_case(MPI_Info_create);
enum_case(MPI_Type_get_extent_x);
enum_case(MPI_Comm_split);
enum_case(MPI_Info_delete);
enum_case(MPI_Type_get_name);
enum_case(MPI_Comm_split_type);
enum_case(MPI_Info_dup);
enum_case(MPI_Type_get_true_extent);
enum_case(MPI_Comm_test_inter);
enum_case(MPI_Info_free);
enum_case(MPI_Type_get_true_extent_x);
enum_case(MPI_Compare_and_swap);
enum_case(MPI_Info_get);
enum_case(MPI_Type_hindexed);
enum_case(MPI_Dims_create);
enum_case(MPI_Info_get_nkeys);
enum_case(MPI_Type_hvector);
enum_case(MPI_Dist_graph_create);
enum_case(MPI_Info_get_nthkey);
enum_case(MPI_Type_indexed);
enum_case(MPI_Dist_graph_create_adjacent);
enum_case(MPI_Info_get_valuelen);
enum_case(MPI_Type_lb);
enum_case(MPI_Dist_graph_neighbors);
enum_case(MPI_Info_set);
enum_case(MPI_Type_match_size);
enum_case(MPI_Dist_graph_neighbors_count);
enum_case(MPI_Init);
enum_case(MPI_Type_set_attr);
enum_case(MPI_Errhandler_create);
enum_case(MPI_Init_thread);
enum_case(MPI_Type_set_name);
enum_case(MPI_Errhandler_free);
enum_case(MPI_Initialized);
enum_case(MPI_Type_size);
enum_case(MPI_Errhandler_get);
enum_case(MPI_Intercomm_create);
enum_case(MPI_Type_size_x);
enum_case(MPI_Errhandler_set);
enum_case(MPI_Intercomm_merge);
enum_case(MPI_Type_struct);
enum_case(MPI_Error_class);
enum_case(MPI_Iprobe);
enum_case(MPI_Type_ub);
enum_case(MPI_Error_string);
enum_case(MPI_Irecv);
enum_case(MPI_Type_vector);
enum_case(MPI_Exscan);
enum_case(MPI_Ireduce);
enum_case(MPI_Unpack);
enum_case(MPI_Fetch_and_op);
enum_case(MPI_Ireduce_scatter);
enum_case(MPI_Unpack_external);
enum_case(MPI_File_c2f);
enum_case(MPI_Ireduce_scatter_block);
enum_case(MPI_Unpublish_name);
enum_case(MPI_File_call_errhandler);
enum_case(MPI_Irsend);
enum_case(MPI_Wait);
enum_case(MPI_File_close);
enum_case(MPI_Is_thread_main);
enum_case(MPI_Waitall);
enum_case(MPI_File_create_errhandler);
enum_case(MPI_Iscan);
enum_case(MPI_Waitany);
enum_case(MPI_File_delete);
enum_case(MPI_Iscatter);
enum_case(MPI_Waitsome);
enum_case(MPI_File_f2c);
enum_case(MPI_Iscatterv);
enum_case(MPI_Win_allocate);
enum_case(MPI_File_get_amode);
enum_case(MPI_Isend);
enum_case(MPI_Win_allocate_shared);
enum_case(MPI_File_get_atomicity);
enum_case(MPI_Issend);
enum_case(MPI_Win_attach);
enum_case(MPI_File_get_byte_offset);
enum_case(MPI_Keyval_create);
enum_case(MPI_Win_call_errhandler);
enum_case(MPI_File_get_errhandler);
enum_case(MPI_Keyval_free);
enum_case(MPI_Win_complete);
enum_case(MPI_File_get_group);
enum_case(MPI_Lookup_name);
enum_case(MPI_Win_create);
enum_case(MPI_File_get_info);
enum_case(MPI_Mprobe);
enum_case(MPI_Win_create_dynamic);
enum_case(MPI_File_get_position);
enum_case(MPI_Mrecv);
enum_case(MPI_Win_create_errhandler);
enum_case(MPI_File_get_position_shared);
enum_case(MPI_Neighbor_allgather);
enum_case(MPI_Win_create_keyval);
enum_case(MPI_File_get_size);
enum_case(MPI_Neighbor_allgatherv);
enum_case(MPI_Win_delete_attr);
enum_case(MPI_File_get_type_extent);
enum_case(MPI_Neighbor_alltoall);
enum_case(MPI_Win_detach);
enum_case(MPI_File_get_view);
enum_case(MPI_Neighbor_alltoallv);
enum_case(MPI_Win_fence);
enum_case(MPI_File_iread);
enum_case(MPI_Neighbor_alltoallw);
enum_case(MPI_Win_flush);
enum_case(MPI_File_iread_all);
enum_case(MPI_Op_commute);
enum_case(MPI_Win_flush_all);
enum_case(MPI_File_iread_at);
enum_case(MPI_Op_create);
enum_case(MPI_Win_flush_local);
enum_case(MPI_File_iread_at_all);
enum_case(MPI_Op_free);
enum_case(MPI_Win_flush_local_all);
enum_case(MPI_File_iread_shared);
enum_case(MPI_Open_port);
enum_case(MPI_Win_free);
enum_case(MPI_File_iwrite);
enum_case(MPI_Pack);
enum_case(MPI_Win_free_keyval);
enum_case(MPI_File_iwrite_all);
enum_case(MPI_Pack_external);
enum_case(MPI_Win_get_attr);
enum_case(MPI_File_iwrite_at);
enum_case(MPI_Pack_external_size);
enum_case(MPI_Win_get_errhandler);
enum_case(MPI_File_iwrite_at_all);
enum_case(MPI_Pack_size);
enum_case(MPI_Win_get_group);
enum_case(MPI_File_iwrite_shared);
enum_case(MPI_Pcontrol);
enum_case(MPI_Win_get_info);
enum_case(MPI_File_open);
enum_case(MPI_Probe);
enum_case(MPI_Win_get_name);
enum_case(MPI_File_preallocate);
enum_case(MPI_Publish_name);
enum_case(MPI_Win_lock);
enum_case(MPI_File_read);
enum_case(MPI_Put);
enum_case(MPI_Win_lock_all);
enum_case(MPI_File_read_all);
enum_case(MPI_Query_thread);
enum_case(MPI_Win_post);
enum_case(MPI_File_read_all_begin);
enum_case(MPI_Raccumulate);
enum_case(MPI_Win_set_attr);
enum_case(MPI_File_read_all_end);
enum_case(MPI_Recv);
enum_case(MPI_Win_set_errhandler);
enum_case(MPI_File_read_at);
enum_case(MPI_Recv_init);
enum_case(MPI_Win_set_info);
enum_case(MPI_File_read_at_all);
enum_case(MPI_Reduce);
enum_case(MPI_Win_set_name);
enum_case(MPI_File_read_at_all_begin);
enum_case(MPI_Reduce_local);
enum_case(MPI_Win_shared_query);
enum_case(MPI_File_read_at_all_end);
enum_case(MPI_Reduce_scatter);
enum_case(MPI_Win_start);
enum_case(MPI_File_read_ordered);
enum_case(MPI_Reduce_scatter_block);
enum_case(MPI_Win_sync);
enum_case(MPI_File_read_ordered_begin);
enum_case(MPI_Register_datarep);
enum_case(MPI_Win_test);
enum_case(MPI_File_read_ordered_end);
enum_case(MPI_Request_free);
enum_case(MPI_Win_unlock);
enum_case(MPI_File_read_shared);
enum_case(MPI_Request_get_status);
enum_case(MPI_Win_unlock_all);
enum_case(MPI_File_seek);
enum_case(MPI_Rget);
enum_case(MPI_Win_wait);
enum_case(MPI_File_seek_shared);
enum_case(MPI_Rget_accumulate);
enum_case(MPI_Wtick);
enum_case(MPI_File_set_atomicity);
enum_case(MPI_Rput);
enum_case(MPI_Wtime);
  }
}

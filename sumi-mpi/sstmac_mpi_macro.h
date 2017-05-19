/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
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

#define MPI_Address(in,out) *out = ((MPI_Aint) in) + 8
#define MPI_Get_address(in,out) MPI_Address(in,out)

#ifdef __cplusplus
#define MPI_Cart_create sumi::sstmac_mpi()->cart_create
#define MPI_Cart_get sumi::sstmac_mpi()->cart_get
#define MPI_Cart_rank sumi::sstmac_mpi()->cart_rank
#define MPI_Cart_shift sumi::sstmac_mpi()->cart_shift
#define MPI_Cart_coords sumi::sstmac_mpi()->cart_coords
#define MPI_NodeAddress sumi::sstmac_mpi()->nodeaddress
#define MPI_Taskid sumi::sstmac_mpi()->taskid
#define MPI_Errhandler_set sumi::sstmac_mpi()->errhandler_set
#define MPI_Print sumi::sstmac_mpi()->print
#define MPI_Init sumi::sstmac_mpi()->init
#define MPI_Initialized sumi::sstmac_mpi()->initialized
#define MPI_Abort sumi::sstmac_mpi()->abort
#define MPI_Finalize sumi::sstmac_mpi()->finalize
#define MPI_Wait sumi::sstmac_mpi()->wait
#define MPI_Waitall sumi::sstmac_mpi()->waitall
#define MPI_Waitany sumi::sstmac_mpi()->waitany
#define MPI_Waitsome sumi::sstmac_mpi()->waitsome
#define MPI_Get_count sumi::sstmac_mpi()->get_count
#define MPI_Test sumi::sstmac_mpi()->test
#define MPI_Probe sumi::sstmac_mpi()->probe
#define MPI_Cancel sumi::sstmac_mpi()->cancel
#define MPI_Request_free sumi::sstmac_mpi()->request_free
#define MPI_Comm_dup sumi::sstmac_mpi()->comm_dup
#define MPI_Comm_rank sumi::sstmac_mpi()->comm_rank
#define MPI_Comm_split sumi::sstmac_mpi()->comm_split
#define MPI_Comm_size sumi::sstmac_mpi()->comm_size
#define MPI_Comm_free sumi::sstmac_mpi()->comm_free
#define MPI_Comm_create sumi::sstmac_mpi()->comm_create
#define MPI_Comm_group sumi::sstmac_mpi()->comm_group
#define MPI_Group_incl sumi::sstmac_mpi()->group_incl
#define MPI_Group_free sumi::sstmac_mpi()->group_free
#define MPI_Sendrecv sumi::sstmac_mpi()->sendrecv
#define MPI_Send sumi::sstmac_mpi()->send
#define MPI_Isend sumi::sstmac_mpi()->isend
#define MPI_Recv sumi::sstmac_mpi()->recv
#define MPI_Irecv sumi::sstmac_mpi()->irecv
#define MPI_Bsend sumi::sstmac_mpi()->send
#define MPI_Rsend sumi::sstmac_mpi()->send
#define MPI_Ssend sumi::sstmac_mpi()->send
#define MPI_Ibsend sumi::sstmac_mpi()->isend
#define MPI_Issend sumi::sstmac_mpi()->isend
#define MPI_Irsend sumi::sstmac_mpi()->isend
#define MPI_Allreduce sumi::sstmac_mpi()->allreduce
#define MPI_Reduce sumi::sstmac_mpi()->reduce
#define MPI_Barrier sumi::sstmac_mpi()->barrier
#define MPI_Bcast sumi::sstmac_mpi()->bcast
#define MPI_Scan sumi::sstmac_mpi()->scan
#define MPI_Gather sumi::sstmac_mpi()->gather
#define MPI_Gatherv sumi::sstmac_mpi()->gatherv
#define MPI_Allgather sumi::sstmac_mpi()->allgather
#define MPI_Allgatherv sumi::sstmac_mpi()->allgatherv
#define MPI_Scatter sumi::sstmac_mpi()->scatter
#define MPI_Scatterv sumi::sstmac_mpi()->scatterv
#define MPI_Alltoall sumi::sstmac_mpi()->alltoall
#define MPI_Iallreduce sumi::sstmac_mpi()->iallreduce
#define MPI_Ireduce sumi::sstmac_mpi()->ireduce
#define MPI_Ibarrier sumi::sstmac_mpi()->ibarrier
#define MPI_Ibcast sumi::sstmac_mpi()->ibcast
#define MPI_Iscan sumi::sstmac_mpi()->iscan
#define MPI_Igather sumi::sstmac_mpi()->igather
#define MPI_Igatherv sumi::sstmac_mpi()->igatherv
#define MPI_Iallgather sumi::sstmac_mpi()->iallgather
#define MPI_Iallgatherv sumi::sstmac_mpi()->iallgatherv
#define MPI_Iscatter sumi::sstmac_mpi()->iscatter
#define MPI_Iscatterv sumi::sstmac_mpi()->iscatterv
#define MPI_Ialltoall sumi::sstmac_mpi()->ialltoall
#define MPI_Ialltoallv sumi::sstmac_mpi()->ialltoallv
#define MPI_Wtime sumi::sstmac_mpi()->wtime
#define MPI_Disable_Payloads sumi::sstmac_mpi()->disable_Payloads
#define MPI_Enable_Payloads sumi::sstmac_mpi()->enable_Payloads
#define MPI_Recv_init sumi::sstmac_mpi()->recv_init
#define MPI_Startall sumi::sstmac_mpi()->startall
#define MPI_Start sumi::sstmac_mpi()->start
#define MPI_Testall sumi::sstmac_mpi()->testall
#define MPI_Testany sumi::sstmac_mpi()->testany
#define MPI_Testsome sumi::sstmac_mpi()->testsome
#define MPI_Test_cancelled sumi::sstmac_mpi()->test_cancelled
#define MPI_Iprobe sumi::sstmac_mpi()->iprobe
#define MPI_Gatherv sumi::sstmac_mpi()->gatherv
#define MPI_Alltoallv sumi::sstmac_mpi()->alltoallv
#define MPI_Type_contiguous sumi::sstmac_mpi()->type_contiguous
#define MPI_Type_commit sumi::sstmac_mpi()->type_commit
#define MPI_Type_free sumi::sstmac_mpi()->type_free
#define MPI_Wtick sumi::sstmac_mpi()->wtick
#define MPI_Type_hvector sumi::sstmac_mpi()->type_hvector
#define MPI_Type_vector sumi::sstmac_mpi()->type_vector
#define MPI_Type_indexed sumi::sstmac_mpi()->type_indexed
#define MPI_Type_extent sumi::sstmac_mpi()->type_extent
#define MPI_Type_dup sumi::sstmac_mpi()->type_dup
#define MPI_Type_set_name sumi::sstmac_mpi()->type_set_name
#define MPI_Type_indexed sumi::sstmac_mpi()->type_indexed
#define MPI_Type_size sumi::sstmac_mpi()->type_size
#define MPI_Type_get_name sumi::sstmac_mpi()->type_get_name
#define MPI_Group_excl sumi::sstmac_mpi()->group_excl
#define MPI_Group_range_incl sumi::sstmac_mpi()->group_range_incl
#define MPI_Keyval_free sumi::sstmac_mpi()->keyval_free
#define MPI_Comm_compare sumi::sstmac_mpi()->comm_compare
#define MPI_Type_extent sumi::sstmac_mpi()->type_extent
#define MPI_Type_dup sumi::sstmac_mpi()->type_dup
#define MPI_Type_set_name sumi::sstmac_mpi()->type_set_name
#define MPI_Type_indexed sumi::sstmac_mpi()->type_indexed
#define MPI_Type_size sumi::sstmac_mpi()->type_size
#define MPI_Type_get_name sumi::sstmac_mpi()->type_get_name
#define MPI_Group_excl sumi::sstmac_mpi()->group_excl
#define MPI_Testany sumi::sstmac_mpi()->testany
#define MPI_Testsome sumi::sstmac_mpi()->testsome
#define MPI_Test_cancelled sumi::sstmac_mpi()->test_cancelled
#define MPI_Type_indexed sumi::sstmac_mpi()->type_indexed
#define MPI_Type_hindexed sumi::sstmac_mpi()->type_hindexed
#define MPI_Type_extent sumi::sstmac_mpi()->type_extent
#define MPI_Type_size sumi::sstmac_mpi()->type_size
#define MPI_Type_lb sumi::sstmac_mpi()->type_lb
#define MPI_Type_ub sumi::sstmac_mpi()->type_ub
#define MPI_Pack sumi::sstmac_mpi()->pack
#define MPI_Unpack sumi::sstmac_mpi()->unpack
#define MPI_Pack_size sumi::sstmac_mpi()->pack_size
#define MPI_Group_size sumi::sstmac_mpi()->group_size
#define MPI_Group_rank sumi::sstmac_mpi()->group_rank
#define MPI_Group_range_incl sumi::sstmac_mpi()->group_range_incl
#define MPI_Group_compare sumi::sstmac_mpi()->group_compare
#define MPI_Group_union sumi::sstmac_mpi()->group_union
#define MPI_Group_intersection sumi::sstmac_mpi()->group_intersection
#define MPI_Group_difference sumi::sstmac_mpi()->group_difference
#define MPI_Group_range_excl sumi::sstmac_mpi()->group_range_excl
#define MPI_Keyval_free sumi::sstmac_mpi()->keyval_free
#define MPI_Reduce_scatter       sumi::sstmac_mpi()->reduce_scatter
#define MPI_Finalized sumi::sstmac_mpi()->finalized
#define MPI_Type_get_extent sumi::sstmac_mpi()->type_get_extent
#define MPI_Type_get_true_extent sumi::sstmac_mpi()->type_get_true_extent
#define MPI_Alltoallw sumi::sstmac_mpi()->alltoallw
#define MPI_Exscan sumi::sstmac_mpi()->exscan
#define MPI_Comm_set_errhandler sumi::sstmac_mpi()->comm_set_errhandler
#define MPI_Error_class sumi::sstmac_mpi()->error_class
#define MPI_Error_string sumi::sstmac_mpi()->error_string
#define MPI_Type_create_struct sumi::sstmac_mpi()->type_create_struct
#define MPI_Type_struct sumi::sstmac_mpi()->type_create_struct
#define MPI_Buffer_attach sumi::sstmac_mpi()->buffer_attach
#define MPI_Buffer_detach sumi::sstmac_mpi()->buffer_detach
#define MPI_Init_thread sumi::sstmac_mpi()->init_thread
#define MPI_Op_create sumi::sstmac_mpi()->op_create
#define MPI_Op_free sumi::sstmac_mpi()->op_free
#define MPI_Reduce_scatter_block   sumi::sstmac_mpi()->reduce_scatter_block
#define MPI_Ireduce_scatter_block   sumi::sstmac_mpi()->ireduce_scatter_block
#define MPI_Send_init sumi::sstmac_mpi()->send_init
#define MPI_Bsend_init sumi::sstmac_mpi()->send_init
#define MPI_Rsend_init sumi::sstmac_mpi()->send_init
#define MPI_Ssend_init sumi::sstmac_mpi()->send_init
#else
#define MPI_Cart_create sstmac_cart_create
#define MPI_Cart_get sstmac_cart_get
#define MPI_Cart_rank sstmac_cart_rank
#define MPI_Cart_shift sstmac_cart_shift
#define MPI_Cart_coords sstmac_cart_coords
#define MPI_NodeAddress sstmac_nodeaddress
#define MPI_Taskid sstmac_taskid
#define MPI_Errhandler_set sstmac_errhandler_set
#define MPI_Print sstmac_print
#define MPI_Init sstmac_init
#define MPI_Initialized sstmac_initialized
#define MPI_Abort sstmac_abort
#define MPI_Finalize sstmac_finalize
#define MPI_Wait sstmac_wait
#define MPI_Waitall sstmac_waitall
#define MPI_Waitany sstmac_waitany
#define MPI_Waitsome sstmac_waitsome
#define MPI_Get_count sstmac_get_count
#define MPI_Test sstmac_test
#define MPI_Probe sstmac_probe
#define MPI_Cancel sstmac_cancel
#define MPI_Request_free sstmac_request_free
#define MPI_Comm_dup sstmac_comm_dup
#define MPI_Comm_rank sstmac_comm_rank
#define MPI_Comm_split sstmac_comm_split
#define MPI_Comm_size sstmac_comm_size
#define MPI_Comm_free sstmac_comm_free
#define MPI_Comm_create sstmac_comm_create
#define MPI_Comm_group sstmac_comm_group
#define MPI_Group_incl sstmac_group_incl
#define MPI_Group_free sstmac_group_free
#define MPI_Sendrecv sstmac_sendrecv
#define MPI_Send sstmac_send
#define MPI_Isend sstmac_isend
#define MPI_Recv sstmac_recv
#define MPI_Irecv sstmac_irecv
#define MPI_Bsend sstmac_send
#define MPI_Rsend sstmac_send
#define MPI_Ssend sstmac_send
#define MPI_Ibsend sstmac_isend
#define MPI_Issend sstmac_isend
#define MPI_Irsend sstmac_isend
#define MPI_Allreduce sstmac_allreduce
#define MPI_Reduce sstmac_reduce
#define MPI_Barrier sstmac_barrier
#define MPI_Bcast sstmac_bcast
#define MPI_Scan sstmac_scan
#define MPI_Gather sstmac_gather
#define MPI_Gatherv sstmac_gatherv
#define MPI_Allgather sstmac_allgather
#define MPI_Allgatherv sstmac_allgatherv
#define MPI_Scatter sstmac_scatter
#define MPI_Scatterv sstmac_scatterv
#define MPI_Alltoall sstmac_alltoall
#define MPI_Iallreduce sstmac_iallreduce
#define MPI_Ireduce sstmac_ireduce
#define MPI_Ibarrier sstmac_ibarrier
#define MPI_Ibcast sstmac_ibcast
#define MPI_Iscan sstmac_iscan
#define MPI_Igather sstmac_igather
#define MPI_Igatherv sstmac_igatherv
#define MPI_Iallgather sstmac_iallgather
#define MPI_Iallgatherv sstmac_iallgatherv
#define MPI_Iscatter sstmac_iscatter
#define MPI_Iscatterv sstmac_iscatterv
#define MPI_Ialltoall sstmac_ialltoall
#define MPI_Ialltoallv sstmac_ialltoallv
#define MPI_Wtime sstmac_wtime
#define MPI_Disable_Payloads sstmac_disable_Payloads
#define MPI_Enable_Payloads sstmac_enable_Payloads
#define MPI_Recv_init sstmac_recv_init
#define MPI_Startall sstmac_startall
#define MPI_Start sstmac_start
#define MPI_Testall sstmac_testall
#define MPI_Testany sstmac_testany
#define MPI_Testsome sstmac_testsome
#define MPI_Test_cancelled sstmac_test_cancelled
#define MPI_Iprobe sstmac_iprobe
#define MPI_Gatherv sstmac_gatherv
#define MPI_Alltoallv sstmac_alltoallv
#define MPI_Type_contiguous sstmac_type_contiguous
#define MPI_Type_commit sstmac_type_commit
#define MPI_Type_free sstmac_type_free
#define MPI_Wtick sstmac_wtick
#define MPI_Type_hvector sstmac_type_hvector
#define MPI_Type_vector sstmac_type_vector
#define MPI_Type_indexed sstmac_type_indexed
#define MPI_Type_extent sstmac_type_extent
#define MPI_Type_dup sstmac_type_dup
#define MPI_Type_set_name sstmac_type_set_name
#define MPI_Type_indexed sstmac_type_indexed
#define MPI_Type_size sstmac_type_size
#define MPI_Type_get_name sstmac_type_get_name
#define MPI_Group_excl sstmac_group_excl
#define MPI_Group_range_incl sstmac_group_range_incl
#define MPI_Keyval_free sstmac_keyval_free
#define MPI_Comm_compare sstmac_comm_compare
#define MPI_Type_extent sstmac_type_extent
#define MPI_Type_dup sstmac_type_dup
#define MPI_Type_set_name sstmac_type_set_name
#define MPI_Type_indexed sstmac_type_indexed
#define MPI_Type_size sstmac_type_size
#define MPI_Type_get_name sstmac_type_get_name
#define MPI_Group_excl sstmac_group_excl
#define MPI_Testany sstmac_testany
#define MPI_Testsome sstmac_testsome
#define MPI_Test_cancelled sstmac_test_cancelled
#define MPI_Type_indexed sstmac_type_indexed
#define MPI_Type_hindexed sstmac_type_hindexed
#define MPI_Type_extent sstmac_type_extent
#define MPI_Type_size sstmac_type_size
#define MPI_Type_lb sstmac_type_lb
#define MPI_Type_ub sstmac_type_ub
#define MPI_Pack sstmac_pack
#define MPI_Unpack sstmac_unpack
#define MPI_Pack_size sstmac_pack_size
#define MPI_Group_size sstmac_group_size
#define MPI_Group_rank sstmac_group_rank
#define MPI_Group_range_incl sstmac_group_range_incl
#define MPI_Group_compare sstmac_group_compare
#define MPI_Group_union sstmac_group_union
#define MPI_Group_intersection sstmac_group_intersection
#define MPI_Group_difference sstmac_group_difference
#define MPI_Group_range_excl sstmac_group_range_excl
#define MPI_Keyval_free sstmac_keyval_free
#define MPI_Reduce_scatter sstmac_reduce_scatter
#define MPI_Finalized sstmac_finalized
#define MPI_Type_get_extent sstmac_type_get_extent
#define MPI_Type_get_true_extent sstmac_type_get_true_extent
#define MPI_Alltoallw sstmac_alltoallw
#define MPI_Exscan sstmac_exscan
#define MPI_Comm_set_errhandler sstmac_comm_set_errhandler
#define MPI_Error_class sstmac_error_class
#define MPI_Error_string sstmac_error_string
#define MPI_Type_create_struct sstmac_type_create_struct
#define MPI_Type_struct sstmac_type_create_struct
#define MPI_Buffer_attach sstmac_buffer_attach
#define MPI_Buffer_detach sstmac_buffer_detach
#define MPI_Init_thread sstmac_init_thread
#define MPI_Op_create sstmac_op_create
#define MPI_Op_free sstmac_op_free
#define MPI_Reduce_scatter_block sstmac_reduce_scatter_block
#define MPI_Ireduce_scatter_block sstmac_ireduce_scatter_block
#define MPI_Send_init sstmac_send_init
#define MPI_Bsend_init sstmac_send_init
#define MPI_Rsend_init sstmac_send_init
#define MPI_Ssend_init sstmac_send_init
#endif


#define MPI_Intercomm_create error not yet implemented
#define MPI_Comm_remote_size error not yet implemented
#define MPI_Comm_test_inter error not yet implemented
#define MPI_Comm_remote_group error not yet implemented
#define MPI_Intercomm_merge error not yet implemented
#define MPI_Attr_delete error not yet implemented
#define MPI_Dims_create error not yet implemented
#define MPI_Graph_create error not yet implemented
#define MPI_Graphdims_get error not yet implemented
#define MPI_Graph_get error not yet implemented
#define MPI_Cart_sub error not yet implemented
#define MPI_Cart_map error not yet implemented
#define MPI_Graph_map error not yet implemented
#define MPI_Get_processor_name error not yet implemented
#define MPI_Get_version error not yet implemented
#define MPI_Errhandler_create error not yet implemented
#define MPI_Errhandler_get error not yet implemented
#define MPI_Errhandler_free error not yet implemented
#define MPI_Pcontrol error not yet implemented
#define MPI_Close_port error not yet implemented
#define MPI_Comm_accept error not yet implemented
#define MPI_Comm_connect error not yet implemented
#define MPI_Comm_disconnect error not yet implemented
#define MPI_Comm_get_parent error not yet implemented
#define MPI_Comm_join error not yet implemented
#define MPI_Comm_spawn error not yet implemented
#define MPI_Comm_spawn_multiple error not yet implemented
#define MPI_Lookup_name error not yet implemented
#define MPI_Open_port error not yet implemented
#define MPI_Publish_name error not yet implemented
#define MPI_Unpublish_name error not yet implemented
#define MPI_Accumulate error not yet implemented
#define MPI_Win_complete error not yet implemented
#define MPI_Win_create error not yet implemented
#define MPI_Win_fence error not yet implemented
#define MPI_Win_free error not yet implemented
#define MPI_Win_get_group error not yet implemented
#define MPI_Win_lock error not yet implemented
#define MPI_Win_post error not yet implemented
#define MPI_Win_start error not yet implemented
#define MPI_Win_test error not yet implemented
#define MPI_Win_unlock error not yet implemented
#define MPI_Win_wait error not yet implemented
#define MPI_Add_error_class error not yet implemented
#define MPI_Add_error_code error not yet implemented
#define MPI_Add_error_string error not yet implemented
#define MPI_Comm_call_errhandler error not yet implemented
#define MPI_Comm_create_keyval error not yet implemented
#define MPI_Comm_delete_attr error not yet implemented
#define MPI_Comm_free_keyval error not yet implemented
#define MPI_Comm_get_attr error not yet implemented
#define MPI_Comm_get_name error not yet implemented
#define MPI_Comm_set_attr error not yet implemented
#define MPI_Comm_set_name error not yet implemented
#define MPI_File_call_errhandler error not yet implemented
#define MPI_Grequest_complete error not yet implemented
#define MPI_Grequest_start error not yet implemented
#define MPI_Is_thread_main error not yet implemented
#define MPI_Query_thread error not yet implemented
#define MPI_Status_set_cancelled error not yet implemented
#define MPI_Status_set_elements error not yet implemented
#define MPI_Type_create_keyval error not yet implemented
#define MPI_Type_delete_attr error not yet implemented
#define MPI_Type_free_keyval error not yet implemented
#define MPI_Type_get_attr error not yet implemented
#define MPI_Type_get_contents error not yet implemented
#define MPI_Type_get_envelope error not yet implemented
#define MPI_Type_set_attr error not yet implemented
#define MPI_Type_match_size error not yet implemented
#define MPI_Win_call_errhandler error not yet implemented
#define MPI_Win_create_keyval error not yet implemented
#define MPI_Win_delete_attr error not yet implemented
#define MPI_Win_free_keyval error not yet implemented
#define MPI_Win_get_attr error not yet implemented
#define MPI_Win_get_name error not yet implemented
#define MPI_Win_set_attr error not yet implemented
#define MPI_Win_set_name error not yet implemented
#define MPI_File_c2f error not yet implemented
#define MPI_Alloc_mem error not yet implemented
#define MPI_Comm_create_errhandler error not yet implemented
#define MPI_Comm_get_errhandler error not yet implemented

#define MPI_File_create_errhandler error not yet implemented
#define MPI_File_get_errhandler error not yet implemented
#define MPI_File_set_errhandler error not yet implemented
#define MPI_Free_mem error not yet implemented
#define MPI_Info_create error not yet implemented
#define MPI_Info_delete error not yet implemented
#define MPI_Info_dup error not yet implemented
#define MPI_Info_free error not yet implemented
#define MPI_Info_get error not yet implemented
#define MPI_Info_get_nkeys error not yet implemented
#define MPI_Info_get_nthkey error not yet implemented
#define MPI_Info_get_valuelen error not yet implemented
#define MPI_Info_set error not yet implemented
#define MPI_Pack_external error not yet implemented
#define MPI_Pack_external_size error not yet implemented
#define MPI_Request_get_status error not yet implemented
#define MPI_Type_create_darray error not yet implemented
#define MPI_Type_create_hindexed error not yet implemented
#define MPI_Type_create_hvector error not yet implemented
#define MPI_Type_create_indexed_block error not yet implemented
#define MPI_Type_create_resized error not yet implemented
#define MPI_Type_create_subarray error not yet implemented
#define MPI_Unpack_external error not yet implemented
#define MPI_Win_create_errhandler error not yet implemented
#define MPI_Win_get_errhandler error not yet implemented
#define MPI_Win_set_errhandler error not yet implemented
#define MPI_Get error not yet implemented
#define MPI_Put error not yet implemented
#define MPI_Dist_graph_create_adjacent error not yet implemented
#define MPI_Dist_graph_create          error not yet implemented
#define MPI_Dist_graph_neighbors_count error not yet implemented
#define MPI_Dist_graph_neighbors       error not yet implemented
#define MPI_Intercomm_create error not yet implemented
#define MPI_Sendrecv_replace error not yet implemented
#define MPI_Group_translate_ranks error not yet implemented
#define MPI_Get_elements error not yet implemented
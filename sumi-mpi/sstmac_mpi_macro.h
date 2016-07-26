#ifdef __cplusplus
#define MPI_Cart_create current_mpi()->cart_create
#define MPI_Cart_get current_mpi()->cart_get
#define MPI_Cart_rank current_mpi()->cart_rank
#define MPI_Cart_shift current_mpi()->cart_shift
#define MPI_Cart_coords current_mpi()->cart_coords
#define MPI_Topo_test _SSTMAC_Topo_test
#define MPI_NodeAddress current_mpi()->nodeaddress
#define MPI_Taskid current_mpi()->taskid
#define MPI_Errhandler_set current_mpi()->errhandler_set
#define MPI_Print current_mpi()->print
#define MPI_Init current_mpi()->do_init
#define MPI_Initialized current_mpi()->initialized
#define MPI_Abort current_mpi()->abort
#define MPI_Finalize current_mpi()->do_finalize
#define MPI_Wait current_mpi()->wait
#define MPI_Waitall current_mpi()->waitall
#define MPI_Waitany current_mpi()->waitany
#define MPI_Waitsome current_mpi()->waitsome
#define MPI_Get_count current_mpi()->get_count
#define MPI_Test current_mpi()->test
#define MPI_Probe current_mpi()->probe
#define MPI_Cancel current_mpi()->cancel
#define MPI_Request_free current_mpi()->request_free
#define MPI_Comm_dup current_mpi()->comm_dup
#define MPI_Comm_rank current_mpi()->comm_rank
#define MPI_Comm_split current_mpi()->comm_split
#define MPI_Comm_size current_mpi()->comm_size
#define MPI_Comm_free current_mpi()->comm_free
#define MPI_Comm_create current_mpi()->comm_create
#define MPI_Comm_group current_mpi()->comm_group
#define MPI_Group_incl current_mpi()->group_incl
#define MPI_Group_free current_mpi()->group_free
#define MPI_Sendrecv current_mpi()->sendrecv
#define MPI_Send current_mpi()->send
#define MPI_Isend current_mpi()->isend
#define MPI_Recv current_mpi()->recv
#define MPI_Irecv current_mpi()->irecv
#define MPI_Bsend current_mpi()->send
#define MPI_Rsend current_mpi()->send
#define MPI_Ssend current_mpi()->send
#define MPI_Ibsend current_mpi()->isend
#define MPI_Issend current_mpi()->isend
#define MPI_Irsend current_mpi()->isend
#define MPI_Allreduce current_mpi()->allreduce
#define MPI_Reduce current_mpi()->reduce
#define MPI_Barrier current_mpi()->barrier
#define MPI_Bcast current_mpi()->bcast
#define MPI_Scan current_mpi()->scan
#define MPI_Gather current_mpi()->gather
#define MPI_Gatherv current_mpi()->gatherv
#define MPI_Allgather current_mpi()->allgather
#define MPI_Allgatherv current_mpi()->allgatherv
#define MPI_Scatter current_mpi()->scatter
#define MPI_Scatterv current_mpi()->scatterv
#define MPI_Alltoall current_mpi()->alltoall
#define MPI_Iallreduce current_mpi()->iallreduce
#define MPI_Ireduce current_mpi()->ireduce
#define MPI_Ibarrier current_mpi()->ibarrier
#define MPI_Ibcast current_mpi()->ibcast
#define MPI_Iscan current_mpi()->iscan
#define MPI_Igather current_mpi()->igather
#define MPI_Igatherv current_mpi()->igatherv
#define MPI_Iallgather current_mpi()->iallgather
#define MPI_Iallgatherv current_mpi()->iallgatherv
#define MPI_Iscatter current_mpi()->iscatter
#define MPI_Iscatterv current_mpi()->iscatterv
#define MPI_Ialltoall current_mpi()->ialltoall
#define MPI_Ialltoallv current_mpi()->ialltoallv
#define MPI_Wtime current_mpi()->wtime
#define MPI_Disable_Payloads current_mpi()->disable_Payloads
#define MPI_Enable_Payloads current_mpi()->enable_Payloads
#define MPI_Recv_init current_mpi()->recv_init
#define MPI_Startall current_mpi()->startall
#define MPI_Start current_mpi()->start
#define MPI_Testall current_mpi()->testall
#define MPI_Testany current_mpi()->testany
#define MPI_Testsome current_mpi()->testsome
#define MPI_Test_cancelled current_mpi()->test_cancelled
#define MPI_Iprobe current_mpi()->iprobe
#define MPI_Gatherv current_mpi()->gatherv
#define MPI_Alltoallv current_mpi()->alltoallv
#define MPI_Type_contiguous current_mpi()->type_contiguous
#define MPI_Type_commit current_mpi()->type_commit
#define MPI_Type_free current_mpi()->type_free
#define MPI_Wtick current_mpi()->wtick
#define MPI_Type_hvector current_mpi()->type_hvector
#define MPI_Type_vector current_mpi()->type_vector
#define MPI_Type_indexed current_mpi()->type_indexed
#define MPI_Type_extent current_mpi()->type_extent
#define MPI_Type_dup current_mpi()->type_dup
#define MPI_Type_set_name current_mpi()->type_set_name
#define MPI_Type_indexed current_mpi()->type_indexed
#define MPI_Type_size current_mpi()->type_size
#define MPI_Type_get_name current_mpi()->type_get_name
#define MPI_Group_excl current_mpi()->group_excl
#define MPI_Group_range_incl current_mpi()->group_range_incl
#define MPI_Keyval_free current_mpi()->keyval_free
#define MPI_Comm_compare current_mpi()->comm_compare
#define MPI_Type_extent current_mpi()->type_extent
#define MPI_Type_dup current_mpi()->type_dup
#define MPI_Type_set_name current_mpi()->type_set_name
#define MPI_Type_indexed current_mpi()->type_indexed
#define MPI_Type_size current_mpi()->type_size
#define MPI_Type_get_name current_mpi()->type_get_name
#define MPI_Group_excl current_mpi()->group_excl
#define MPI_Testany current_mpi()->testany
#define MPI_Testsome current_mpi()->testsome
#define MPI_Test_cancelled current_mpi()->test_cancelled
#define MPI_Type_indexed current_mpi()->type_indexed
#define MPI_Type_hindexed current_mpi()->type_hindexed
#define MPI_Type_extent current_mpi()->type_extent
#define MPI_Type_size current_mpi()->type_size
#define MPI_Type_lb current_mpi()->type_lb
#define MPI_Type_ub current_mpi()->type_ub
#define MPI_Pack current_mpi()->pack
#define MPI_Unpack current_mpi()->unpack
#define MPI_Pack_size current_mpi()->pack_size
#define MPI_Group_size current_mpi()->group_size
#define MPI_Group_rank current_mpi()->group_rank
#define MPI_Group_range_incl current_mpi()->group_range_incl
#define MPI_Group_compare current_mpi()->group_compare
#define MPI_Group_union current_mpi()->group_union
#define MPI_Group_intersection current_mpi()->group_intersection
#define MPI_Group_difference current_mpi()->group_difference
#define MPI_Group_range_excl current_mpi()->group_range_excl
#define MPI_Keyval_free current_mpi()->keyval_free
#define MPI_Reduce_scatter       current_mpi()->reduce_scatter
#define MPI_Finalized current_mpi()->finalized
#define MPI_Type_get_extent current_mpi()->type_get_extent
#define MPI_Type_get_true_extent current_mpi()->type_get_true_extent
#define MPI_Alltoallw current_mpi()->alltoallw
#define MPI_Exscan current_mpi()->exscan
#define MPI_Comm_set_errhandler current_mpi()->comm_set_errhandler
#define MPI_Error_class current_mpi()->error_class
#define MPI_Error_string current_mpi()->error_string
#define MPI_Type_create_struct current_mpi()->type_create_struct
#define MPI_Type_struct current_mpi()->type_create_struct
#define MPI_Get_address current_mpi()->get_address
#define MPI_Buffer_attach current_mpi()->buffer_attach
#define MPI_Buffer_detach current_mpi()->buffer_detach
#define MPI_Init_thread current_mpi()->init_thread
#define MPI_Op_create current_mpi()->op_create
#define MPI_Op_free current_mpi()->op_free
#define MPI_Reduce_scatter_block   current_mpi()->reduce_scatter_block
#define MPI_Ireduce_scatter_block   current_mpi()->ireduce_scatter_block
#define MPI_Send_init current_mpi()->send_init
#define MPI_Bsend_init current_mpi()->send_init
#define MPI_Rsend_init current_mpi()->send_init
#define MPI_Ssend_init current_mpi()->send_init
#else
#define MPI_NodeAddress sstmac_mpi_nodeaddress
#define MPI_Taskid sstmac_mpi_taskid
#define MPI_Errhandler_set sstmac_mpi_errhandler_set
#define MPI_Print sstmac_mpi_print
#define MPI_Init sstmac_mpi_do_init
#define MPI_Initialized sstmac_mpi_initialized
#define MPI_Abort sstmac_mpi_abort
#define MPI_Finalize sstmac_mpi_do_finalize
#define MPI_Wait sstmac_mpi_wait
#define MPI_Waitall sstmac_mpi_waitall
#define MPI_Waitany sstmac_mpi_waitany
#define MPI_Waitsome sstmac_mpi_waitsome
#define MPI_Get_count sstmac_mpi_get_count
#define MPI_Test sstmac_mpi_test
#define MPI_Probe sstmac_mpi_probe
#define MPI_Cancel sstmac_mpi_cancel
#define MPI_Request_free sstmac_mpi_request_free
#define MPI_Comm_dup sstmac_mpi_comm_dup
#define MPI_Comm_rank sstmac_mpi_comm_rank
#define MPI_Comm_split sstmac_mpi_comm_split
#define MPI_Comm_size sstmac_mpi_comm_size
#define MPI_Comm_free sstmac_mpi_comm_free
#define MPI_Comm_create sstmac_mpi_comm_create
#define MPI_Comm_group sstmac_mpi_comm_group
#define MPI_Group_incl sstmac_mpi_group_incl
#define MPI_Group_free sstmac_mpi_group_free
#define MPI_Send sstmac_mpi_send
#define MPI_Isend sstmac_mpi_isend
#define MPI_Recv sstmac_mpi_recv
#define MPI_Irecv sstmac_mpi_irecv
#define MPI_Allreduce sstmac_mpi_allreduce
#define MPI_Reduce sstmac_mpi_reduce
#define MPI_Barrier sstmac_mpi_barrier
#define MPI_Bcast sstmac_mpi_bcast
#define MPI_Scan sstmac_mpi_scan
#define MPI_Gather sstmac_mpi_gather
#define MPI_Gatherv sstmac_mpi_gatherv
#define MPI_Allgather sstmac_mpi_allgather
#define MPI_Allgatherv sstmac_mpi_allgatherv
#define MPI_Scatter sstmac_mpi_scatter
#define MPI_Scatterv sstmac_mpi_scatterv
#define MPI_Alltoall sstmac_mpi_alltoall
#define MPI_Wtime sstmac_mpi_wtime
#define MPI_Disable_Payloads sstmac_mpi_disable_Payloads
#define MPI_Enable_Payloads sstmac_mpi_enable_Payloads
#define MPI_Recv_init sstmac_mpi_recv_init
#define MPI_Startall sstmac_mpi_startall
#define MPI_Start sstmac_mpi_start
#define MPI_Testall sstmac_mpi_testall
#define MPI_Testany sstmac_mpi_testany
#define MPI_Testsome sstmac_mpi_testsome
#define MPI_Test_cancelled sstmac_mpi_test_cancelled
#define MPI_Iprobe sstmac_mpi_iprobe
#define MPI_Gatherv sstmac_mpi_gatherv
#define MPI_Alltoallv sstmac_mpi_alltoallv
#define MPI_Type_struct sstmac_mpi_type_struct
#define MPI_Type_contiguous sstmac_mpi_type_contiguous
#define MPI_Type_commit sstmac_mpi_type_commit
#define MPI_Type_free sstmac_mpi_type_free
#define MPI_Wtick sstmac_mpi_wtick
#define MPI_Type_hvector sstmac_mpi_type_hvector
#define MPI_Type_vector sstmac_mpi_type_vector
#define MPI_Type_indexed sstmac_mpi_type_indexed
#define MPI_Type_extent sstmac_mpi_type_extent
#define MPI_Type_dup sstmac_mpi_type_dup
#define MPI_Type_set_name sstmac_mpi_type_set_name
#define MPI_Type_indexed sstmac_mpi_type_indexed
#define MPI_Type_size sstmac_mpi_type_size
#define MPI_Type_get_name sstmac_mpi_type_get_name
#define MPI_Group_excl sstmac_mpi_group_excl
#define MPI_Group_range_incl sstmac_mpi_group_range_incl
#define MPI_Keyval_free sstmac_mpi_keyval_free
#define MPI_Comm_compare sstmac_mpi_comm_compare
#define MPI_Type_extent sstmac_mpi_type_extent
#define MPI_Type_dup sstmac_mpi_type_dup
#define MPI_Type_set_name sstmac_mpi_type_set_name
#define MPI_Type_indexed sstmac_mpi_type_indexed
#define MPI_Type_size sstmac_mpi_type_size
#define MPI_Type_get_name sstmac_mpi_type_get_name
#define MPI_Group_excl sstmac_mpi_group_excl
#define MPI_Testany sstmac_mpi_testany
#define MPI_Testsome sstmac_mpi_testsome
#define MPI_Test_cancelled sstmac_mpi_test_cancelled
#define MPI_Type_indexed sstmac_mpi_type_indexed
#define MPI_Type_hindexed sstmac_mpi_type_hindexed
#define MPI_Type_extent sstmac_mpi_type_extent
#define MPI_Type_size sstmac_mpi_type_size
#define MPI_Type_lb sstmac_mpi_type_lb
#define MPI_Type_ub sstmac_mpi_type_ub
#define MPI_Pack sstmac_mpi_pack
#define MPI_Unpack sstmac_mpi_unpack
#define MPI_Pack_size sstmac_mpi_pack_size
#define MPI_Op_create sstmac_mpi_op_create
#define MPI_Op_free sstmac_mpi_op_free
#define MPI_Group_size sstmac_mpi_group_size
#define MPI_Group_rank sstmac_mpi_group_rank
#define MPI_Group_range_incl sstmac_mpi_group_range_incl
#define MPI_Group_compare sstmac_mpi_group_compare
#define MPI_Group_union sstmac_mpi_group_union
#define MPI_Group_intersection sstmac_mpi_group_intersection
#define MPI_Group_difference sstmac_mpi_group_difference
#define MPI_Group_range_excl sstmac_mpi_group_range_excl
#define MPI_Keyval_free sstmac_mpi_keyval_free
#define MPI_Reduce_scatter       sstmac_mpi_reduce_scatter
#define MPI_Finalized sstmac_mpi_finalized
#define MPI_Type_get_extent sstmac_mpi_type_get_extent
#define MPI_Type_get_true_extent sstmac_mpi_type_get_true_extent
#define MPI_Alltoallw sstmac_mpi_alltoallw
#define MPI_Exscan sstmac_mpi_exscan
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
#define MPI_Address error not yet implemented
#define MPI_Intercomm_create error not yet implemented
#define MPI_Sendrecv_replace error not yet implemented
#define MPI_Group_translate_ranks error not yet implemented
#define MPI_Get_elements error not yet implemented

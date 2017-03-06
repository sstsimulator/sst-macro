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

#ifndef SSTMAC_SKELETONS_OTF2_TRACE_REPLAY_CALLBASE_H_
#define SSTMAC_SKELETONS_OTF2_TRACE_REPLAY_CALLBASE_H_

#include <sstmac/software/process/operating_system.h>
#include <otf2/otf2.h>
#include <string>

#include "dumpi/common/argtypes.h"
#include "mpi_calls.h"

using namespace std;

// forward declare
class OTF2_trace_replay_app;

class CallBase {
public:
    // Push event to simulator
    virtual void Trigger() = 0;

    // True when the event can be triggered
    virtual bool IsReady();
    virtual string const ToString();

    CallBase();
    CallBase(OTF2_trace_replay_app*);
    CallBase(OTF2_LocationRef, OTF2_TimeStamp, OTF2_trace_replay_app*);
    CallBase(OTF2_LocationRef, OTF2_TimeStamp, OTF2_TimeStamp, OTF2_trace_replay_app*);
    virtual ~CallBase() {}

    sstmac::timestamp GetStart();
    sstmac::timestamp GetEnd();
    virtual int GetID() = 0;

    OTF2_TimeStamp start_time, end_time;
    bool isready;
    OTF2_LocationRef location;
    OTF2_trace_replay_app* app;

    static void assert_call(CallBase* cb, string msg);

private:
    sstmac::timestamp convert_time(const OTF2_TimeStamp);
};

/******************************************************************************
 * Preprocessors for CallBase implementations
 */

#define STATIC_INIT(dumpi_struct) auto ds = (dumpi_struct*)this; \
    memset(ds, 0, sizeof(dumpi_struct));\

#define CREATE_EMPTY_CALL(name, call_id, dumpi_struct, ...) \
    class name : public CallBase, public dumpi_struct \
    { \
    public: \
	    /* Deploy an empty trigger*/ \
	    virtual void Trigger() { cout << "Rank " << location << ":\t"<< #name <<  ": Empty Trigger " << endl; } \
        \
        name(): CallBase(NULL) { }; \
        name(OTF2_trace_replay_app* app) : CallBase(app) { }; \
        name(OTF2_LocationRef location, OTF2_TimeStamp time, OTF2_trace_replay_app* app=NULL) : CallBase(location, time, app) { \
            STATIC_INIT(dumpi_struct) \
        } \
        static const int id = call_id; \
        virtual string const ToString() { return _name; } \
		virtual int GetID() { return id; }\
    private: \
        string _name = #name; \
    };

// A non-empty call will leave 'void Trigger()' undefined. The user must define in callbase_impl.cc
#define CREATE_CALL(name, call_id, dumpi_struct) \
    class name : public CallBase, public dumpi_struct \
    { \
    private: \
        string _name = #name; \
    public: \
        name() : CallBase(NULL) { }; \
        name(OTF2_trace_replay_app* app) : CallBase(app) { }; \
        name(OTF2_LocationRef location, OTF2_TimeStamp time, OTF2_trace_replay_app* app=NULL) : CallBase(location, time, app) { \
            STATIC_INIT(dumpi_struct) \
        } \
        static const int id = call_id; \
        virtual string const ToString() { return _name; } \
		virtual int GetID() { return id; }\
        virtual void Trigger(); \
    };

/*****************************************************************************/

    CREATE_EMPTY_CALL(MpiAbortCall, ID_MPI_Abort, dumpi_abort)
    CREATE_EMPTY_CALL(MpiAccumulateCall, ID_MPI_Accumulate, dumpi_accumulate)
    CREATE_EMPTY_CALL(MpiAdderrorclassCall, ID_MPI_Add_error_class, dumpi_add_error_class)
    CREATE_EMPTY_CALL(MpiAdderrorcodeCall, ID_MPI_Add_error_code, dumpi_add_error_code)
    CREATE_EMPTY_CALL(MpiAdderrorstringCall, ID_MPI_Add_error_string, dumpi_add_error_string)
    CREATE_EMPTY_CALL(MpiAddressCall, ID_MPI_Address, dumpi_address)
//CREATE_EMPTY_CALL(MpiAintaddCall, ID_MPI_Aint_add, dumpi_aint_add)
//CREATE_EMPTY_CALL(MpiAintdiffCall, ID_MPI_Aint_diff, dumpi_aint_diff)
    CREATE_EMPTY_CALL(MpiAllgatherCall, ID_MPI_Allgather, dumpi_allgather)
    CREATE_EMPTY_CALL(MpiAllgathervCall, ID_MPI_Allgatherv, dumpi_allgatherv)
    CREATE_EMPTY_CALL(MpiAllocmemCall, ID_MPI_Alloc_mem, dumpi_alloc_mem)
    CREATE_EMPTY_CALL(MpiAllreduceCall, ID_MPI_Allreduce, dumpi_allreduce)
    CREATE_EMPTY_CALL(MpiAlltoallCall, ID_MPI_Alltoall, dumpi_alltoall)
    CREATE_EMPTY_CALL(MpiAlltoallvCall, ID_MPI_Alltoallv, dumpi_alltoallv)
    CREATE_EMPTY_CALL(MpiAlltoallwCall, ID_MPI_Alltoallw, dumpi_alltoallw)
    CREATE_EMPTY_CALL(MpiAttrdeleteCall, ID_MPI_Attr_delete, dumpi_attr_delete)
    CREATE_EMPTY_CALL(MpiAttrgetCall, ID_MPI_Attr_get, dumpi_attr_get)
    CREATE_EMPTY_CALL(MpiAttrputCall, ID_MPI_Attr_put, dumpi_attr_put)
    CREATE_EMPTY_CALL(MpiBarrierCall, ID_MPI_Barrier, dumpi_barrier)
    CREATE_EMPTY_CALL(MpiBcastCall, ID_MPI_Bcast, dumpi_bcast)
    CREATE_EMPTY_CALL(MpiBsendCall, ID_MPI_Bsend, dumpi_bsend)
    CREATE_EMPTY_CALL(MpiBsendinitCall, ID_MPI_Bsend_init, dumpi_bsend_init)
    CREATE_EMPTY_CALL(MpiBufferattachCall, ID_MPI_Buffer_attach, dumpi_buffer_attach)
    CREATE_EMPTY_CALL(MpiBufferdetachCall, ID_MPI_Buffer_detach, dumpi_buffer_detach)
    CREATE_EMPTY_CALL(MpiCancelCall, ID_MPI_Cancel, dumpi_cancel)
    CREATE_EMPTY_CALL(MpiCartcoordsCall, ID_MPI_Cart_coords, dumpi_cart_coords)
    CREATE_EMPTY_CALL(MpiCartcreateCall, ID_MPI_Cart_create, dumpi_cart_create)
    CREATE_EMPTY_CALL(MpiCartgetCall, ID_MPI_Cart_get, dumpi_cart_get)
    CREATE_EMPTY_CALL(MpiCartmapCall, ID_MPI_Cart_map, dumpi_cart_map)
    CREATE_EMPTY_CALL(MpiCartrankCall, ID_MPI_Cart_rank, dumpi_cart_rank)
    CREATE_EMPTY_CALL(MpiCartshiftCall, ID_MPI_Cart_shift, dumpi_cart_shift)
    CREATE_EMPTY_CALL(MpiCartsubCall, ID_MPI_Cart_sub, dumpi_cart_sub)
    CREATE_EMPTY_CALL(MpiCartdimgetCall, ID_MPI_Cartdim_get, dumpi_cartdim_get)
    CREATE_EMPTY_CALL(MpiCloseportCall, ID_MPI_Close_port, dumpi_close_port)
    CREATE_EMPTY_CALL(MpiCommacceptCall, ID_MPI_Comm_accept, dumpi_comm_accept)
    CREATE_EMPTY_CALL(MpiCommcallerrhandlerCall, ID_MPI_Comm_call_errhandler, dumpi_comm_call_errhandler)
    CREATE_EMPTY_CALL(MpiCommcompareCall, ID_MPI_Comm_compare, dumpi_comm_compare)
    CREATE_EMPTY_CALL(MpiCommconnectCall, ID_MPI_Comm_connect, dumpi_comm_connect)
    CREATE_EMPTY_CALL(MpiCommcreateCall, ID_MPI_Comm_create, dumpi_comm_create)
    CREATE_EMPTY_CALL(MpiCommcreateerrhandlerCall, ID_MPI_Comm_create_errhandler, dumpi_comm_create_errhandler)
//CREATE_EMPTY_CALL(MpiCommcreategroupCall, ID_MPI_Comm_create_group, dumpi_comm_create_group)
    CREATE_EMPTY_CALL(MpiCommcreatekeyvalCall, ID_MPI_Comm_create_keyval, dumpi_comm_create_keyval)
    CREATE_EMPTY_CALL(MpiCommdeleteattrCall, ID_MPI_Comm_delete_attr, dumpi_comm_delete_attr)
    CREATE_EMPTY_CALL(MpiCommdisconnectCall, ID_MPI_Comm_disconnect, dumpi_comm_disconnect)
    CREATE_EMPTY_CALL(MpiCommdupCall, ID_MPI_Comm_dup, dumpi_comm_dup)
//CREATE_EMPTY_CALL(MpiCommdupwithinfoCall, ID_MPI_Comm_dup_with_info, dumpi_comm_dup_with_info)
    CREATE_EMPTY_CALL(MpiCommfreeCall, ID_MPI_Comm_free, dumpi_comm_free)
    CREATE_EMPTY_CALL(MpiCommfreekeyvalCall, ID_MPI_Comm_free_keyval, dumpi_comm_free_keyval)
    CREATE_EMPTY_CALL(MpiCommgetattrCall, ID_MPI_Comm_get_attr, dumpi_comm_get_attr)
    CREATE_EMPTY_CALL(MpiCommgeterrhandlerCall, ID_MPI_Comm_get_errhandler, dumpi_comm_get_errhandler)
//CREATE_EMPTY_CALL(MpiCommgetinfoCall, ID_MPI_Comm_get_info, dumpi_comm_get_info)
    CREATE_EMPTY_CALL(MpiCommgetnameCall, ID_MPI_Comm_get_name, dumpi_comm_get_name)
    CREATE_EMPTY_CALL(MpiCommgetparentCall, ID_MPI_Comm_get_parent, dumpi_comm_get_parent)
    CREATE_EMPTY_CALL(MpiCommgroupCall, ID_MPI_Comm_group, dumpi_comm_group)
//CREATE_EMPTY_CALL(MpiCommidupCall, ID_MPI_Comm_idup, dumpi_comm_idup)
    CREATE_EMPTY_CALL(MpiCommjoinCall, ID_MPI_Comm_join, dumpi_comm_join)
    CREATE_EMPTY_CALL(MpiCommrankCall, ID_MPI_Comm_rank, dumpi_comm_rank)
    CREATE_EMPTY_CALL(MpiCommremotegroupCall, ID_MPI_Comm_remote_group, dumpi_comm_remote_group)
    CREATE_EMPTY_CALL(MpiCommremotesizeCall, ID_MPI_Comm_remote_size, dumpi_comm_remote_size)
    CREATE_EMPTY_CALL(MpiCommsetattrCall, ID_MPI_Comm_set_attr, dumpi_comm_set_attr)
    CREATE_EMPTY_CALL(MpiCommseterrhandlerCall, ID_MPI_Comm_set_errhandler, dumpi_comm_set_errhandler)
//CREATE_EMPTY_CALL(MpiCommsetinfoCall, ID_MPI_Comm_set_info, dumpi_comm_set_info)
    CREATE_EMPTY_CALL(MpiCommsetnameCall, ID_MPI_Comm_set_name, dumpi_comm_set_name)
    CREATE_EMPTY_CALL(MpiCommsizeCall, ID_MPI_Comm_size, dumpi_comm_size)
    CREATE_EMPTY_CALL(MpiCommspawnCall, ID_MPI_Comm_spawn, dumpi_comm_spawn)
    CREATE_EMPTY_CALL(MpiCommspawnmultipleCall, ID_MPI_Comm_spawn_multiple, dumpi_comm_spawn_multiple)
    CREATE_EMPTY_CALL(MpiCommsplitCall, ID_MPI_Comm_split, dumpi_comm_split)
//CREATE_EMPTY_CALL(MpiCommsplittypeCall, ID_MPI_Comm_split_type, dumpi_comm_split_type)
    CREATE_EMPTY_CALL(MpiCommtestinterCall, ID_MPI_Comm_test_inter, dumpi_comm_test_inter)
//CREATE_EMPTY_CALL(MpiCompareandswapCall, ID_MPI_Compare_and_swap, dumpi_compare_and_swap)
    CREATE_EMPTY_CALL(MpiDimscreateCall, ID_MPI_Dims_create, dumpi_dims_create)
//CREATE_EMPTY_CALL(MpiDistgraphcreateCall, ID_MPI_Dist_graph_create, dumpi_dist_graph_create)
//CREATE_EMPTY_CALL(MpiDistgraphcreateadjacentCall, ID_MPI_Dist_graph_create_adjacent, dumpi_dist_graph_create_adjacent)
//CREATE_EMPTY_CALL(MpiDistgraphneighborsCall, ID_MPI_Dist_graph_neighbors, dumpi_dist_graph_neighbors)
//CREATE_EMPTY_CALL(MpiDistgraphneighborscountCall, ID_MPI_Dist_graph_neighbors_count, dumpi_dist_graph_neighbors_count)
    CREATE_EMPTY_CALL(MpiErrhandlercreateCall, ID_MPI_Errhandler_create, dumpi_errhandler_create)
    CREATE_EMPTY_CALL(MpiErrhandlerfreeCall, ID_MPI_Errhandler_free, dumpi_errhandler_free)
    CREATE_EMPTY_CALL(MpiErrhandlergetCall, ID_MPI_Errhandler_get, dumpi_errhandler_get)
    CREATE_EMPTY_CALL(MpiErrhandlersetCall, ID_MPI_Errhandler_set, dumpi_errhandler_set)
    CREATE_EMPTY_CALL(MpiErrorclassCall, ID_MPI_Error_class, dumpi_error_class)
    CREATE_EMPTY_CALL(MpiErrorstringCall, ID_MPI_Error_string, dumpi_error_string)
    CREATE_EMPTY_CALL(MpiExscanCall, ID_MPI_Exscan, dumpi_exscan)
//CREATE_EMPTY_CALL(MpiFetchandopCall, ID_MPI_Fetch_and_op, dumpi_fetch_and_op)
//CREATE_EMPTY_CALL(MpiFilec2fCall, ID_MPI_File_c2f, dumpi_file_c2f)
    CREATE_EMPTY_CALL(MpiFilecallerrhandlerCall, ID_MPI_File_call_errhandler, dumpi_file_call_errhandler)
    CREATE_EMPTY_CALL(MpiFilecloseCall, ID_MPI_File_close, dumpi_file_close)
    CREATE_EMPTY_CALL(MpiFilecreateerrhandlerCall, ID_MPI_File_create_errhandler, dumpi_file_create_errhandler)
    CREATE_EMPTY_CALL(MpiFiledeleteCall, ID_MPI_File_delete, dumpi_file_delete)
//CREATE_EMPTY_CALL(MpiFilef2cCall, ID_MPI_File_f2c, dumpi_file_f2c)
    CREATE_EMPTY_CALL(MpiFilegetamodeCall, ID_MPI_File_get_amode, dumpi_file_get_amode)
    CREATE_EMPTY_CALL(MpiFilegetatomicityCall, ID_MPI_File_get_atomicity, dumpi_file_get_atomicity)
    CREATE_EMPTY_CALL(MpiFilegetbyteoffsetCall, ID_MPI_File_get_byte_offset, dumpi_file_get_byte_offset)
    CREATE_EMPTY_CALL(MpiFilegeterrhandlerCall, ID_MPI_File_get_errhandler, dumpi_file_get_errhandler)
    CREATE_EMPTY_CALL(MpiFilegetgroupCall, ID_MPI_File_get_group, dumpi_file_get_group)
    CREATE_EMPTY_CALL(MpiFilegetinfoCall, ID_MPI_File_get_info, dumpi_file_get_info)
    CREATE_EMPTY_CALL(MpiFilegetpositionCall, ID_MPI_File_get_position, dumpi_file_get_position)
    CREATE_EMPTY_CALL(MpiFilegetpositionsharedCall, ID_MPI_File_get_position_shared, dumpi_file_get_position_shared)
    CREATE_EMPTY_CALL(MpiFilegetsizeCall, ID_MPI_File_get_size, dumpi_file_get_size)
    CREATE_EMPTY_CALL(MpiFilegettypeextentCall, ID_MPI_File_get_type_extent, dumpi_file_get_type_extent)
    CREATE_EMPTY_CALL(MpiFilegetviewCall, ID_MPI_File_get_view, dumpi_file_get_view)
    CREATE_EMPTY_CALL(MpiFileireadCall, ID_MPI_File_iread, dumpi_file_iread)
//CREATE_EMPTY_CALL(MpiFileireadallCall, ID_MPI_File_iread_all, dumpi_file_iread_all)
    CREATE_EMPTY_CALL(MpiFileireadatCall, ID_MPI_File_iread_at, dumpi_file_iread_at)
//CREATE_EMPTY_CALL(MpiFileireadatallCall, ID_MPI_File_iread_at_all, dumpi_file_iread_at_all)
    CREATE_EMPTY_CALL(MpiFileireadsharedCall, ID_MPI_File_iread_shared, dumpi_file_iread_shared)
    CREATE_EMPTY_CALL(MpiFileiwriteCall, ID_MPI_File_iwrite, dumpi_file_iwrite)
//CREATE_EMPTY_CALL(MpiFileiwriteallCall, ID_MPI_File_iwrite_all, dumpi_file_iwrite_all)
    CREATE_EMPTY_CALL(MpiFileiwriteatCall, ID_MPI_File_iwrite_at, dumpi_file_iwrite_at)
//CREATE_EMPTY_CALL(MpiFileiwriteatallCall, ID_MPI_File_iwrite_at_all, dumpi_file_iwrite_at_all)
    CREATE_EMPTY_CALL(MpiFileiwritesharedCall, ID_MPI_File_iwrite_shared, dumpi_file_iwrite_shared)
    CREATE_EMPTY_CALL(MpiFileopenCall, ID_MPI_File_open, dumpi_file_open)
    CREATE_EMPTY_CALL(MpiFilepreallocateCall, ID_MPI_File_preallocate, dumpi_file_preallocate)
    CREATE_EMPTY_CALL(MpiFilereadCall, ID_MPI_File_read, dumpi_file_read)
    CREATE_EMPTY_CALL(MpiFilereadallCall, ID_MPI_File_read_all, dumpi_file_read_all)
    CREATE_EMPTY_CALL(MpiFilereadallbeginCall, ID_MPI_File_read_all_begin, dumpi_file_read_all_begin)
    CREATE_EMPTY_CALL(MpiFilereadallendCall, ID_MPI_File_read_all_end, dumpi_file_read_all_end)
    CREATE_EMPTY_CALL(MpiFilereadatCall, ID_MPI_File_read_at, dumpi_file_read_at)
    CREATE_EMPTY_CALL(MpiFilereadatallCall, ID_MPI_File_read_at_all, dumpi_file_read_at_all)
    CREATE_EMPTY_CALL(MpiFilereadatallbeginCall, ID_MPI_File_read_at_all_begin, dumpi_file_read_at_all_begin)
    CREATE_EMPTY_CALL(MpiFilereadatallendCall, ID_MPI_File_read_at_all_end, dumpi_file_read_at_all_end)
    CREATE_EMPTY_CALL(MpiFilereadorderedCall, ID_MPI_File_read_ordered, dumpi_file_read_ordered)
    CREATE_EMPTY_CALL(MpiFilereadorderedbeginCall, ID_MPI_File_read_ordered_begin, dumpi_file_read_ordered_begin)
    CREATE_EMPTY_CALL(MpiFilereadorderedendCall, ID_MPI_File_read_ordered_end, dumpi_file_read_ordered_end)
    CREATE_EMPTY_CALL(MpiFilereadsharedCall, ID_MPI_File_read_shared, dumpi_file_read_shared)
    CREATE_EMPTY_CALL(MpiFileseekCall, ID_MPI_File_seek, dumpi_file_seek)
    CREATE_EMPTY_CALL(MpiFileseeksharedCall, ID_MPI_File_seek_shared, dumpi_file_seek_shared)
    CREATE_EMPTY_CALL(MpiFilesetatomicityCall, ID_MPI_File_set_atomicity, dumpi_file_set_atomicity)
    CREATE_EMPTY_CALL(MpiFileseterrhandlerCall, ID_MPI_File_set_errhandler, dumpi_file_set_errhandler)
    CREATE_EMPTY_CALL(MpiFilesetinfoCall, ID_MPI_File_set_info, dumpi_file_set_info)
    CREATE_EMPTY_CALL(MpiFilesetsizeCall, ID_MPI_File_set_size, dumpi_file_set_size)
    CREATE_EMPTY_CALL(MpiFilesetviewCall, ID_MPI_File_set_view, dumpi_file_set_view)
    CREATE_EMPTY_CALL(MpiFilesyncCall, ID_MPI_File_sync, dumpi_file_sync)
    CREATE_EMPTY_CALL(MpiFilewriteCall, ID_MPI_File_write, dumpi_file_write)
    CREATE_EMPTY_CALL(MpiFilewriteallCall, ID_MPI_File_write_all, dumpi_file_write_all)
    CREATE_EMPTY_CALL(MpiFilewriteallbeginCall, ID_MPI_File_write_all_begin, dumpi_file_write_all_begin)
    CREATE_EMPTY_CALL(MpiFilewriteallendCall, ID_MPI_File_write_all_end, dumpi_file_write_all_end)
    CREATE_EMPTY_CALL(MpiFilewriteatCall, ID_MPI_File_write_at, dumpi_file_write_at)
    CREATE_EMPTY_CALL(MpiFilewriteatallCall, ID_MPI_File_write_at_all, dumpi_file_write_at_all)
    CREATE_EMPTY_CALL(MpiFilewriteatallbeginCall, ID_MPI_File_write_at_all_begin, dumpi_file_write_at_all_begin)
    CREATE_EMPTY_CALL(MpiFilewriteatallendCall, ID_MPI_File_write_at_all_end, dumpi_file_write_at_all_end)
    CREATE_EMPTY_CALL(MpiFilewriteorderedCall, ID_MPI_File_write_ordered, dumpi_file_write_ordered)
    CREATE_EMPTY_CALL(MpiFilewriteorderedbeginCall, ID_MPI_File_write_ordered_begin, dumpi_file_write_ordered_begin)
    CREATE_EMPTY_CALL(MpiFilewriteorderedendCall, ID_MPI_File_write_ordered_end, dumpi_file_write_ordered_end)
    CREATE_EMPTY_CALL(MpiFilewritesharedCall, ID_MPI_File_write_shared, dumpi_file_write_shared)
    CREATE_CALL(MpiFinalizeCall, ID_MPI_Finalize, dumpi_finalize)
    CREATE_EMPTY_CALL(MpiFinalizedCall, ID_MPI_Finalized, dumpi_finalized)
    CREATE_EMPTY_CALL(MpiFreememCall, ID_MPI_Free_mem, dumpi_free_mem)
    CREATE_EMPTY_CALL(MpiGatherCall, ID_MPI_Gather, dumpi_gather)
    CREATE_EMPTY_CALL(MpiGathervCall, ID_MPI_Gatherv, dumpi_gatherv)
    CREATE_EMPTY_CALL(MpiGetCall, ID_MPI_Get, dumpi_get)
//CREATE_EMPTY_CALL(MpiGetaccumulateCall, ID_MPI_Get_accumulate, dumpi_get_accumulate)
    CREATE_EMPTY_CALL(MpiGetaddressCall, ID_MPI_Get_address, dumpi_get_address)
    CREATE_EMPTY_CALL(MpiGetcountCall, ID_MPI_Get_count, dumpi_get_count)
    CREATE_EMPTY_CALL(MpiGetelementsCall, ID_MPI_Get_elements, dumpi_get_elements)
//CREATE_EMPTY_CALL(MpiGetelementsxCall, ID_MPI_Get_elements_x, dumpi_get_elements_x)
//CREATE_EMPTY_CALL(MpiGetlibraryversionCall, ID_MPI_Get_library_version, dumpi_get_library_version)
    CREATE_EMPTY_CALL(MpiGetprocessornameCall, ID_MPI_Get_processor_name, dumpi_get_processor_name)
    CREATE_EMPTY_CALL(MpiGetversionCall, ID_MPI_Get_version, dumpi_get_version)
    CREATE_EMPTY_CALL(MpiGraphcreateCall, ID_MPI_Graph_create, dumpi_graph_create)
    CREATE_EMPTY_CALL(MpiGraphgetCall, ID_MPI_Graph_get, dumpi_graph_get)
    CREATE_EMPTY_CALL(MpiGraphmapCall, ID_MPI_Graph_map, dumpi_graph_map)
    CREATE_EMPTY_CALL(MpiGraphneighborsCall, ID_MPI_Graph_neighbors, dumpi_graph_neighbors)
    CREATE_EMPTY_CALL(MpiGraphneighborscountCall, ID_MPI_Graph_neighbors_count, dumpi_graph_neighbors_count)
    CREATE_EMPTY_CALL(MpiGraphdimsgetCall, ID_MPI_Graphdims_get, dumpi_graphdims_get)
    CREATE_EMPTY_CALL(MpiGrequestcompleteCall, ID_MPI_Grequest_complete, dumpi_grequest_complete)
    CREATE_EMPTY_CALL(MpiGrequeststartCall, ID_MPI_Grequest_start, dumpi_grequest_start)
    CREATE_EMPTY_CALL(MpiGroupcompareCall, ID_MPI_Group_compare, dumpi_group_compare)
    CREATE_EMPTY_CALL(MpiGroupdifferenceCall, ID_MPI_Group_difference, dumpi_group_difference)
    CREATE_EMPTY_CALL(MpiGroupexclCall, ID_MPI_Group_excl, dumpi_group_excl)
    CREATE_EMPTY_CALL(MpiGroupfreeCall, ID_MPI_Group_free, dumpi_group_free)
    CREATE_EMPTY_CALL(MpiGroupinclCall, ID_MPI_Group_incl, dumpi_group_incl)
    CREATE_EMPTY_CALL(MpiGroupintersectionCall, ID_MPI_Group_intersection, dumpi_group_intersection)
    CREATE_EMPTY_CALL(MpiGrouprangeexclCall, ID_MPI_Group_range_excl, dumpi_group_range_excl)
    CREATE_EMPTY_CALL(MpiGrouprangeinclCall, ID_MPI_Group_range_incl, dumpi_group_range_incl)
    CREATE_EMPTY_CALL(MpiGrouprankCall, ID_MPI_Group_rank, dumpi_group_rank)
    CREATE_EMPTY_CALL(MpiGroupsizeCall, ID_MPI_Group_size, dumpi_group_size)
    CREATE_EMPTY_CALL(MpiGrouptranslateranksCall, ID_MPI_Group_translate_ranks, dumpi_group_translate_ranks)
    CREATE_EMPTY_CALL(MpiGroupunionCall, ID_MPI_Group_union, dumpi_group_union)
//CREATE_EMPTY_CALL(MpiIallgatherCall, ID_MPI_Iallgather, dumpi_iallgather)
//CREATE_EMPTY_CALL(MpiIallgathervCall, ID_MPI_Iallgatherv, dumpi_iallgatherv)
//CREATE_EMPTY_CALL(MpiIallreduceCall, ID_MPI_Iallreduce, dumpi_iallreduce)
//CREATE_EMPTY_CALL(MpiIalltoallCall, ID_MPI_Ialltoall, dumpi_ialltoall)
//CREATE_EMPTY_CALL(MpiIalltoallvCall, ID_MPI_Ialltoallv, dumpi_ialltoallv)
//CREATE_EMPTY_CALL(MpiIalltoallwCall, ID_MPI_Ialltoallw, dumpi_ialltoallw)
//CREATE_EMPTY_CALL(MpiIbarrierCall, ID_MPI_Ibarrier, dumpi_ibarrier)
//CREATE_EMPTY_CALL(MpiIbcastCall, ID_MPI_Ibcast, dumpi_ibcast)
//CREATE_EMPTY_CALL(MpiIbsendCall, ID_MPI_Ibsend, dumpi_ibsend)
//CREATE_EMPTY_CALL(MpiIexscanCall, ID_MPI_Iexscan, dumpi_iexscan)
//CREATE_EMPTY_CALL(MpiIgatherCall, ID_MPI_Igather, dumpi_igather)
//CREATE_EMPTY_CALL(MpiIgathervCall, ID_MPI_Igatherv, dumpi_igatherv)
//CREATE_EMPTY_CALL(MpiImprobeCall, ID_MPI_Improbe, dumpi_improbe)
//CREATE_EMPTY_CALL(MpiImrecvCall, ID_MPI_Imrecv, dumpi_imrecv)
//CREATE_EMPTY_CALL(MpiIneighborallgatherCall, ID_MPI_Ineighbor_allgather, dumpi_ineighbor_allgather)
//CREATE_EMPTY_CALL(MpiIneighborallgathervCall, ID_MPI_Ineighbor_allgatherv, dumpi_ineighbor_allgatherv)
//CREATE_EMPTY_CALL(MpiIneighboralltoallCall, ID_MPI_Ineighbor_alltoall, dumpi_ineighbor_alltoall)
//CREATE_EMPTY_CALL(MpiIneighboralltoallvCall, ID_MPI_Ineighbor_alltoallv, dumpi_ineighbor_alltoallv)
//CREATE_EMPTY_CALL(MpiIneighboralltoallwCall, ID_MPI_Ineighbor_alltoallw, dumpi_ineighbor_alltoallw)
    CREATE_EMPTY_CALL(MpiInfocreateCall, ID_MPI_Info_create, dumpi_info_create)
    CREATE_EMPTY_CALL(MpiInfodeleteCall, ID_MPI_Info_delete, dumpi_info_delete)
    CREATE_EMPTY_CALL(MpiInfodupCall, ID_MPI_Info_dup, dumpi_info_dup)
    CREATE_EMPTY_CALL(MpiInfofreeCall, ID_MPI_Info_free, dumpi_info_free)
    CREATE_EMPTY_CALL(MpiInfogetCall, ID_MPI_Info_get, dumpi_info_get)
    CREATE_EMPTY_CALL(MpiInfogetnkeysCall, ID_MPI_Info_get_nkeys, dumpi_info_get_nkeys)
    CREATE_EMPTY_CALL(MpiInfogetnthkeyCall, ID_MPI_Info_get_nthkey, dumpi_info_get_nthkey)
    CREATE_EMPTY_CALL(MpiInfogetvaluelenCall, ID_MPI_Info_get_valuelen, dumpi_info_get_valuelen)
    CREATE_EMPTY_CALL(MpiInfosetCall, ID_MPI_Info_set, dumpi_info_set)
    CREATE_CALL(MpiInitCall, ID_MPI_Init, dumpi_init)
    CREATE_EMPTY_CALL(MpiInitthreadCall, ID_MPI_Init_thread, dumpi_init_thread)
    CREATE_EMPTY_CALL(MpiInitializedCall, ID_MPI_Initialized, dumpi_initialized)
    CREATE_EMPTY_CALL(MpiIntercommcreateCall, ID_MPI_Intercomm_create, dumpi_intercomm_create)
    CREATE_EMPTY_CALL(MpiIntercommmergeCall, ID_MPI_Intercomm_merge, dumpi_intercomm_merge)
    CREATE_EMPTY_CALL(MpiIprobeCall, ID_MPI_Iprobe, dumpi_iprobe)
    CREATE_CALL(MpiIrecvCall, ID_MPI_Irecv, dumpi_irecv)
//CREATE_EMPTY_CALL(MpiIreduceCall, ID_MPI_Ireduce, dumpi_ireduce)
//CREATE_EMPTY_CALL(MpiIreducescatterCall, ID_MPI_Ireduce_scatter, dumpi_ireduce_scatter)
//CREATE_EMPTY_CALL(MpiIreducescatterblockCall, ID_MPI_Ireduce_scatter_block, dumpi_ireduce_scatter_block)
    CREATE_EMPTY_CALL(MpiIrsendCall, ID_MPI_Irsend, dumpi_irsend)
    CREATE_EMPTY_CALL(MpiIsthreadmainCall, ID_MPI_Is_thread_main, dumpi_is_thread_main)
//CREATE_EMPTY_CALL(MpiIscanCall, ID_MPI_Iscan, dumpi_iscan)
//CREATE_EMPTY_CALL(MpiIscatterCall, ID_MPI_Iscatter, dumpi_iscatter)
//CREATE_EMPTY_CALL(MpiIscattervCall, ID_MPI_Iscatterv, dumpi_iscatterv)
    CREATE_CALL(MpiIsendCall, ID_MPI_Isend, dumpi_isend)
    CREATE_EMPTY_CALL(MpiIssendCall, ID_MPI_Issend, dumpi_issend)
    CREATE_EMPTY_CALL(MpiKeyvalcreateCall, ID_MPI_Keyval_create, dumpi_keyval_create)
    CREATE_EMPTY_CALL(MpiKeyvalfreeCall, ID_MPI_Keyval_free, dumpi_keyval_free)
    CREATE_EMPTY_CALL(MpiLookupnameCall, ID_MPI_Lookup_name, dumpi_lookup_name)
//CREATE_EMPTY_CALL(MpiMprobeCall, ID_MPI_Mprobe, dumpi_mprobe)
//CREATE_EMPTY_CALL(MpiMrecvCall, ID_MPI_Mrecv, dumpi_mrecv)
//CREATE_EMPTY_CALL(MpiNeighborallgatherCall, ID_MPI_Neighbor_allgather, dumpi_neighbor_allgather)
//CREATE_EMPTY_CALL(MpiNeighborallgathervCall, ID_MPI_Neighbor_allgatherv, dumpi_neighbor_allgatherv)
//CREATE_EMPTY_CALL(MpiNeighboralltoallCall, ID_MPI_Neighbor_alltoall, dumpi_neighbor_alltoall)
//CREATE_EMPTY_CALL(MpiNeighboralltoallvCall, ID_MPI_Neighbor_alltoallv, dumpi_neighbor_alltoallv)
//CREATE_EMPTY_CALL(MpiNeighboralltoallwCall, ID_MPI_Neighbor_alltoallw, dumpi_neighbor_alltoallw)
//CREATE_EMPTY_CALL(MpiOpcommuteCall, ID_MPI_Op_commute, dumpi_op_commute)
    CREATE_EMPTY_CALL(MpiOpcreateCall, ID_MPI_Op_create, dumpi_op_create)
    CREATE_EMPTY_CALL(MpiOpfreeCall, ID_MPI_Op_free, dumpi_op_free)
    CREATE_EMPTY_CALL(MpiOpenportCall, ID_MPI_Open_port, dumpi_open_port)
    CREATE_EMPTY_CALL(MpiPackCall, ID_MPI_Pack, dumpi_pack)
    CREATE_EMPTY_CALL(MpiPackexternalCall, ID_MPI_Pack_external, dumpi_pack_external)
    CREATE_EMPTY_CALL(MpiPackexternalsizeCall, ID_MPI_Pack_external_size, dumpi_pack_external_size)
    CREATE_EMPTY_CALL(MpiPacksizeCall, ID_MPI_Pack_size, dumpi_pack_size)
//CREATE_EMPTY_CALL(MpiPcontrolCall, ID_MPI_Pcontrol, dumpi_pcontrol)
    CREATE_EMPTY_CALL(MpiProbeCall, ID_MPI_Probe, dumpi_probe)
    CREATE_EMPTY_CALL(MpiPublishnameCall, ID_MPI_Publish_name, dumpi_publish_name)
    CREATE_EMPTY_CALL(MpiPutCall, ID_MPI_Put, dumpi_put)
    CREATE_EMPTY_CALL(MpiQuerythreadCall, ID_MPI_Query_thread, dumpi_query_thread)
//CREATE_EMPTY_CALL(MpiRaccumulateCall, ID_MPI_Raccumulate, dumpi_raccumulate)
    CREATE_CALL(MpiRecvCall, ID_MPI_Recv, dumpi_recv)
    CREATE_EMPTY_CALL(MpiRecvinitCall, ID_MPI_Recv_init, dumpi_recv_init)
    CREATE_EMPTY_CALL(MpiReduceCall, ID_MPI_Reduce, dumpi_reduce)
//CREATE_EMPTY_CALL(MpiReducelocalCall, ID_MPI_Reduce_local, dumpi_reduce_local)
    CREATE_EMPTY_CALL(MpiReducescatterCall, ID_MPI_Reduce_scatter, dumpi_reduce_scatter)
//CREATE_EMPTY_CALL(MpiReducescatterblockCall, ID_MPI_Reduce_scatter_block, dumpi_reduce_scatter_block)
    CREATE_EMPTY_CALL(MpiRegisterdatarepCall, ID_MPI_Register_datarep, dumpi_register_datarep)
    CREATE_EMPTY_CALL(MpiRequestfreeCall, ID_MPI_Request_free, dumpi_request_free)
    CREATE_EMPTY_CALL(MpiRequestgetstatusCall, ID_MPI_Request_get_status, dumpi_request_get_status)
//CREATE_EMPTY_CALL(MpiRgetCall, ID_MPI_Rget, dumpi_rget)
//CREATE_EMPTY_CALL(MpiRgetaccumulateCall, ID_MPI_Rget_accumulate, dumpi_rget_accumulate)
//CREATE_EMPTY_CALL(MpiRputCall, ID_MPI_Rput, dumpi_rput)
    CREATE_EMPTY_CALL(MpiRsendCall, ID_MPI_Rsend, dumpi_rsend)
    CREATE_EMPTY_CALL(MpiRsendinitCall, ID_MPI_Rsend_init, dumpi_rsend_init)
    CREATE_EMPTY_CALL(MpiScanCall, ID_MPI_Scan, dumpi_scan)
    CREATE_EMPTY_CALL(MpiScatterCall, ID_MPI_Scatter, dumpi_scatter)
    CREATE_EMPTY_CALL(MpiScattervCall, ID_MPI_Scatterv, dumpi_scatterv)
    CREATE_CALL(MpiSendCall, ID_MPI_Send, dumpi_send)
    CREATE_EMPTY_CALL(MpiSendinitCall, ID_MPI_Send_init, dumpi_send_init)
    CREATE_EMPTY_CALL(MpiSendrecvCall, ID_MPI_Sendrecv, dumpi_sendrecv)
    CREATE_EMPTY_CALL(MpiSendrecvreplaceCall, ID_MPI_Sendrecv_replace, dumpi_sendrecv_replace)
    CREATE_EMPTY_CALL(MpiSsendCall, ID_MPI_Ssend, dumpi_ssend)
    CREATE_EMPTY_CALL(MpiSsendinitCall, ID_MPI_Ssend_init, dumpi_ssend_init)
    CREATE_EMPTY_CALL(MpiStartCall, ID_MPI_Start, dumpi_start)
    CREATE_EMPTY_CALL(MpiStartallCall, ID_MPI_Startall, dumpi_startall)
    CREATE_EMPTY_CALL(MpiStatussetcancelledCall, ID_MPI_Status_set_cancelled, dumpi_status_set_cancelled)
    CREATE_EMPTY_CALL(MpiStatussetelementsCall, ID_MPI_Status_set_elements, dumpi_status_set_elements)
//CREATE_EMPTY_CALL(MpiStatussetelementsxCall, ID_MPI_Status_set_elements_x, dumpi_status_set_elements_x)
//CREATE_EMPTY_CALL(MpiTcategorychangedCall, ID_MPI_T_category_changed, dumpi_t_category_changed)
//CREATE_EMPTY_CALL(MpiTcategorygetcategoriesCall, ID_MPI_T_category_get_categories, dumpi_t_category_get_categories)
//CREATE_EMPTY_CALL(MpiTcategorygetcvarsCall, ID_MPI_T_category_get_cvars, dumpi_t_category_get_cvars)
//CREATE_EMPTY_CALL(MpiTcategorygetinfoCall, ID_MPI_T_category_get_info, dumpi_t_category_get_info)
//CREATE_EMPTY_CALL(MpiTcategorygetnumCall, ID_MPI_T_category_get_num, dumpi_t_category_get_num)
//CREATE_EMPTY_CALL(MpiTcategorygetpvarsCall, ID_MPI_T_category_get_pvars, dumpi_t_category_get_pvars)
//CREATE_EMPTY_CALL(MpiTcvargetinfoCall, ID_MPI_T_cvar_get_info, dumpi_t_cvar_get_info)
//CREATE_EMPTY_CALL(MpiTcvargetnumCall, ID_MPI_T_cvar_get_num, dumpi_t_cvar_get_num)
//CREATE_EMPTY_CALL(MpiTcvarhandleallocCall, ID_MPI_T_cvar_handle_alloc, dumpi_t_cvar_handle_alloc)
//CREATE_EMPTY_CALL(MpiTcvarhandlefreeCall, ID_MPI_T_cvar_handle_free, dumpi_t_cvar_handle_free)
//CREATE_EMPTY_CALL(MpiTcvarreadCall, ID_MPI_T_cvar_read, dumpi_t_cvar_read)
//CREATE_EMPTY_CALL(MpiTcvarwriteCall, ID_MPI_T_cvar_write, dumpi_t_cvar_write)
//CREATE_EMPTY_CALL(MpiTenumgetinfoCall, ID_MPI_T_enum_get_info, dumpi_t_enum_get_info)
//CREATE_EMPTY_CALL(MpiTenumgetitemCall, ID_MPI_T_enum_get_item, dumpi_t_enum_get_item)
//CREATE_EMPTY_CALL(MpiTfinalizeCall, ID_MPI_T_finalize, dumpi_t_finalize)
//CREATE_EMPTY_CALL(MpiTinitthreadCall, ID_MPI_T_init_thread, dumpi_t_init_thread)
//CREATE_EMPTY_CALL(MpiTpvargetinfoCall, ID_MPI_T_pvar_get_info, dumpi_t_pvar_get_info)
//CREATE_EMPTY_CALL(MpiTpvargetnumCall, ID_MPI_T_pvar_get_num, dumpi_t_pvar_get_num)
//CREATE_EMPTY_CALL(MpiTpvarhandleallocCall, ID_MPI_T_pvar_handle_alloc, dumpi_t_pvar_handle_alloc)
//CREATE_EMPTY_CALL(MpiTpvarhandlefreeCall, ID_MPI_T_pvar_handle_free, dumpi_t_pvar_handle_free)
//CREATE_EMPTY_CALL(MpiTpvarreadCall, ID_MPI_T_pvar_read, dumpi_t_pvar_read)
//CREATE_EMPTY_CALL(MpiTpvarreadresetCall, ID_MPI_T_pvar_readreset, dumpi_t_pvar_readreset)
//CREATE_EMPTY_CALL(MpiTpvarresetCall, ID_MPI_T_pvar_reset, dumpi_t_pvar_reset)
//CREATE_EMPTY_CALL(MpiTpvarsessioncreateCall, ID_MPI_T_pvar_session_create, dumpi_t_pvar_session_create)
//CREATE_EMPTY_CALL(MpiTpvarsessionfreeCall, ID_MPI_T_pvar_session_free, dumpi_t_pvar_session_free)
//CREATE_EMPTY_CALL(MpiTpvarstartCall, ID_MPI_T_pvar_start, dumpi_t_pvar_start)
//CREATE_EMPTY_CALL(MpiTpvarstopCall, ID_MPI_T_pvar_stop, dumpi_t_pvar_stop)
//CREATE_EMPTY_CALL(MpiTpvarwriteCall, ID_MPI_T_pvar_write, dumpi_t_pvar_write)
    CREATE_EMPTY_CALL(MpiTestCall, ID_MPI_Test, dumpi_test)
    CREATE_EMPTY_CALL(MpiTestcancelledCall, ID_MPI_Test_cancelled, dumpi_test_cancelled)
    CREATE_EMPTY_CALL(MpiTestallCall, ID_MPI_Testall, dumpi_testall)
    CREATE_EMPTY_CALL(MpiTestanyCall, ID_MPI_Testany, dumpi_testany)
    CREATE_EMPTY_CALL(MpiTestsomeCall, ID_MPI_Testsome, dumpi_testsome)
    CREATE_EMPTY_CALL(MpiTopotestCall, ID_MPI_Topo_test, dumpi_topo_test)
    CREATE_EMPTY_CALL(MpiTypecommitCall, ID_MPI_Type_commit, dumpi_type_commit)
    CREATE_EMPTY_CALL(MpiTypecontiguousCall, ID_MPI_Type_contiguous, dumpi_type_contiguous)
    CREATE_EMPTY_CALL(MpiTypecreatedarrayCall, ID_MPI_Type_create_darray, dumpi_type_create_darray)
    CREATE_EMPTY_CALL(MpiTypecreatehindexedCall, ID_MPI_Type_create_hindexed, dumpi_type_create_hindexed)
//CREATE_EMPTY_CALL(MpiTypecreatehindexedblockCall, ID_MPI_Type_create_hindexed_block, dumpi_type_create_hindexed_block)
    CREATE_EMPTY_CALL(MpiTypecreatehvectorCall, ID_MPI_Type_create_hvector, dumpi_type_create_hvector)
    CREATE_EMPTY_CALL(MpiTypecreateindexedblockCall, ID_MPI_Type_create_indexed_block, dumpi_type_create_indexed_block)
    CREATE_EMPTY_CALL(MpiTypecreatekeyvalCall, ID_MPI_Type_create_keyval, dumpi_type_create_keyval)
    CREATE_EMPTY_CALL(MpiTypecreateresizedCall, ID_MPI_Type_create_resized, dumpi_type_create_resized)
    CREATE_EMPTY_CALL(MpiTypecreatestructCall, ID_MPI_Type_create_struct, dumpi_type_create_struct)
    CREATE_EMPTY_CALL(MpiTypecreatesubarrayCall, ID_MPI_Type_create_subarray, dumpi_type_create_subarray)
    CREATE_EMPTY_CALL(MpiTypedeleteattrCall, ID_MPI_Type_delete_attr, dumpi_type_delete_attr)
    CREATE_EMPTY_CALL(MpiTypedupCall, ID_MPI_Type_dup, dumpi_type_dup)
    CREATE_EMPTY_CALL(MpiTypeextentCall, ID_MPI_Type_extent, dumpi_type_extent)
    CREATE_EMPTY_CALL(MpiTypefreeCall, ID_MPI_Type_free, dumpi_type_free)
    CREATE_EMPTY_CALL(MpiTypefreekeyvalCall, ID_MPI_Type_free_keyval, dumpi_type_free_keyval)
    CREATE_EMPTY_CALL(MpiTypegetattrCall, ID_MPI_Type_get_attr, dumpi_type_get_attr)
    CREATE_EMPTY_CALL(MpiTypegetcontentsCall, ID_MPI_Type_get_contents, dumpi_type_get_contents)
    CREATE_EMPTY_CALL(MpiTypegetenvelopeCall, ID_MPI_Type_get_envelope, dumpi_type_get_envelope)
    CREATE_EMPTY_CALL(MpiTypegetextentCall, ID_MPI_Type_get_extent, dumpi_type_get_extent)
//CREATE_EMPTY_CALL(MpiTypegetextentxCall, ID_MPI_Type_get_extent_x, dumpi_type_get_extent_x)
    CREATE_EMPTY_CALL(MpiTypegetnameCall, ID_MPI_Type_get_name, dumpi_type_get_name)
    CREATE_EMPTY_CALL(MpiTypegettrueextentCall, ID_MPI_Type_get_true_extent, dumpi_type_get_true_extent)
//CREATE_EMPTY_CALL(MpiTypegettrueextentxCall, ID_MPI_Type_get_true_extent_x, dumpi_type_get_true_extent_x)
    CREATE_EMPTY_CALL(MpiTypehindexedCall, ID_MPI_Type_hindexed, dumpi_type_hindexed)
    CREATE_EMPTY_CALL(MpiTypehvectorCall, ID_MPI_Type_hvector, dumpi_type_hvector)
    CREATE_EMPTY_CALL(MpiTypeindexedCall, ID_MPI_Type_indexed, dumpi_type_indexed)
    CREATE_EMPTY_CALL(MpiTypelbCall, ID_MPI_Type_lb, dumpi_type_lb)
    CREATE_EMPTY_CALL(MpiTypematchsizeCall, ID_MPI_Type_match_size, dumpi_type_match_size)
    CREATE_EMPTY_CALL(MpiTypesetattrCall, ID_MPI_Type_set_attr, dumpi_type_set_attr)
    CREATE_EMPTY_CALL(MpiTypesetnameCall, ID_MPI_Type_set_name, dumpi_type_set_name)
    CREATE_EMPTY_CALL(MpiTypesizeCall, ID_MPI_Type_size, dumpi_type_size)
//CREATE_EMPTY_CALL(MpiTypesizexCall, ID_MPI_Type_size_x, dumpi_type_size_x)
    CREATE_EMPTY_CALL(MpiTypestructCall, ID_MPI_Type_struct, dumpi_type_struct)
    CREATE_EMPTY_CALL(MpiTypeubCall, ID_MPI_Type_ub, dumpi_type_ub)
    CREATE_EMPTY_CALL(MpiTypevectorCall, ID_MPI_Type_vector, dumpi_type_vector)
    CREATE_EMPTY_CALL(MpiUnpackCall, ID_MPI_Unpack, dumpi_unpack)
    CREATE_EMPTY_CALL(MpiUnpackexternalCall, ID_MPI_Unpack_external, dumpi_unpack_external)
    CREATE_EMPTY_CALL(MpiUnpublishnameCall, ID_MPI_Unpublish_name, dumpi_unpublish_name)
    CREATE_CALL(MpiWaitCall, ID_MPI_Wait, dumpi_wait)
    CREATE_EMPTY_CALL(MpiWaitallCall, ID_MPI_Waitall, dumpi_waitall)
    CREATE_EMPTY_CALL(MpiWaitanyCall, ID_MPI_Waitany, dumpi_waitany)
    CREATE_EMPTY_CALL(MpiWaitsomeCall, ID_MPI_Waitsome, dumpi_waitsome)
//CREATE_EMPTY_CALL(MpiWinallocateCall, ID_MPI_Win_allocate, dumpi_win_allocate)
//CREATE_EMPTY_CALL(MpiWinallocatesharedCall, ID_MPI_Win_allocate_shared, dumpi_win_allocate_shared)
//CREATE_EMPTY_CALL(MpiWinattachCall, ID_MPI_Win_attach, dumpi_win_attach)
    CREATE_EMPTY_CALL(MpiWincallerrhandlerCall, ID_MPI_Win_call_errhandler, dumpi_win_call_errhandler)
    CREATE_EMPTY_CALL(MpiWincompleteCall, ID_MPI_Win_complete, dumpi_win_complete)
    CREATE_EMPTY_CALL(MpiWincreateCall, ID_MPI_Win_create, dumpi_win_create)
//CREATE_EMPTY_CALL(MpiWincreatedynamicCall, ID_MPI_Win_create_dynamic, dumpi_win_create_dynamic)
    CREATE_EMPTY_CALL(MpiWincreateerrhandlerCall, ID_MPI_Win_create_errhandler, dumpi_win_create_errhandler)
    CREATE_EMPTY_CALL(MpiWincreatekeyvalCall, ID_MPI_Win_create_keyval, dumpi_win_create_keyval)
    CREATE_EMPTY_CALL(MpiWindeleteattrCall, ID_MPI_Win_delete_attr, dumpi_win_delete_attr)
//CREATE_EMPTY_CALL(MpiWindetachCall, ID_MPI_Win_detach, dumpi_win_detach)
    CREATE_EMPTY_CALL(MpiWinfenceCall, ID_MPI_Win_fence, dumpi_win_fence)
//CREATE_EMPTY_CALL(MpiWinflushCall, ID_MPI_Win_flush, dumpi_win_flush)
//CREATE_EMPTY_CALL(MpiWinflushallCall, ID_MPI_Win_flush_all, dumpi_win_flush_all)
//CREATE_EMPTY_CALL(MpiWinflushlocalCall, ID_MPI_Win_flush_local, dumpi_win_flush_local)
//CREATE_EMPTY_CALL(MpiWinflushlocalallCall, ID_MPI_Win_flush_local_all, dumpi_win_flush_local_all)
    CREATE_EMPTY_CALL(MpiWinfreeCall, ID_MPI_Win_free, dumpi_win_free)
    CREATE_EMPTY_CALL(MpiWinfreekeyvalCall, ID_MPI_Win_free_keyval, dumpi_win_free_keyval)
    CREATE_EMPTY_CALL(MpiWingetattrCall, ID_MPI_Win_get_attr, dumpi_win_get_attr)
    CREATE_EMPTY_CALL(MpiWingeterrhandlerCall, ID_MPI_Win_get_errhandler, dumpi_win_get_errhandler)
    CREATE_EMPTY_CALL(MpiWingetgroupCall, ID_MPI_Win_get_group, dumpi_win_get_group)
//CREATE_EMPTY_CALL(MpiWingetinfoCall, ID_MPI_Win_get_info, dumpi_win_get_info)
    CREATE_EMPTY_CALL(MpiWingetnameCall, ID_MPI_Win_get_name, dumpi_win_get_name)
    CREATE_EMPTY_CALL(MpiWinlockCall, ID_MPI_Win_lock, dumpi_win_lock)
//CREATE_EMPTY_CALL(MpiWinlockallCall, ID_MPI_Win_lock_all, dumpi_win_lock_all)
    CREATE_EMPTY_CALL(MpiWinpostCall, ID_MPI_Win_post, dumpi_win_post)
    CREATE_EMPTY_CALL(MpiWinsetattrCall, ID_MPI_Win_set_attr, dumpi_win_set_attr)
    CREATE_EMPTY_CALL(MpiWinseterrhandlerCall, ID_MPI_Win_set_errhandler, dumpi_win_set_errhandler)
//CREATE_EMPTY_CALL(MpiWinsetinfoCall, ID_MPI_Win_set_info, dumpi_win_set_info)
    CREATE_EMPTY_CALL(MpiWinsetnameCall, ID_MPI_Win_set_name, dumpi_win_set_name)
//CREATE_EMPTY_CALL(MpiWinsharedqueryCall, ID_MPI_Win_shared_query, dumpi_win_shared_query)
    CREATE_EMPTY_CALL(MpiWinstartCall, ID_MPI_Win_start, dumpi_win_start)
//CREATE_EMPTY_CALL(MpiWinsyncCall, ID_MPI_Win_sync, dumpi_win_sync)
    CREATE_EMPTY_CALL(MpiWintestCall, ID_MPI_Win_test, dumpi_win_test)
    CREATE_EMPTY_CALL(MpiWinunlockCall, ID_MPI_Win_unlock, dumpi_win_unlock)
//CREATE_EMPTY_CALL(MpiWinunlockallCall, ID_MPI_Win_unlock_all, dumpi_win_unlock_all)
    CREATE_EMPTY_CALL(MpiWinwaitCall, ID_MPI_Win_wait, dumpi_win_wait)
    CREATE_EMPTY_CALL(MpiWtickCall, ID_MPI_Wtick, dumpi_wtick)
    CREATE_EMPTY_CALL(MpiWtimeCall, ID_MPI_Wtime, dumpi_wtime)

#undef STATIC_INIT
#undef CREATE_EMPTY_CALL
#undef CREATE_CALL

#endif /* SSTMAC_SKELETONS_OTF2_TRACE_REPLAY_CALLBASE_H_ */

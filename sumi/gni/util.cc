#include <gni/gni_transport.h>


#define enumcase(x) case x: return #x

namespace sumi {

const char* 
gni_transport::strerror(gni_return_t rc)
{
  switch(rc)
  {
    enumcase(GNI_RC_SUCCESS);
    enumcase(GNI_RC_NOT_DONE);
    enumcase(GNI_RC_INVALID_PARAM);
    enumcase(GNI_RC_ERROR_RESOURCE);
    enumcase(GNI_RC_TIMEOUT);
    enumcase(GNI_RC_PERMISSION_ERROR);
    enumcase(GNI_RC_DESCRIPTOR_ERROR);
    enumcase(GNI_RC_ALIGNMENT_ERROR);
    enumcase(GNI_RC_INVALID_STATE);
    enumcase(GNI_RC_NO_MATCH);
    enumcase(GNI_RC_SIZE_ERROR);
    enumcase(GNI_RC_TRANSACTION_ERROR);
    enumcase(GNI_RC_ILLEGAL_OP);
    enumcase(GNI_RC_ERROR_NOMEM);
  }
  spkt_throw_printf(sprockit::value_error,
    "invalid return code rc=%d", rc);
  return 0;
}

void
gni_transport::print_post_descriptor(gni_post_descriptor_t* pd)
{
  gni_debug("local_addr=%p\n"
   "local_mem_handle=(%lu,%lu)\n"
   "length=%lu\n"
   "type=%d\n"
   "post_id=%lu\n"
   "complete=%hu\n"
   "src_cq_handle=%p\n"
   "first_operand=%lu\n"
   "second_operand=%lu\n"
   "write_value=%lu",
   pd->local_addr,
   pd->local_mem_hndl.qword1, pd->local_mem_hndl.qword2,
   pd->length,
   pd->type,
   pd->post_id,
   pd->cq_mode_complete,
   pd->src_cq_hndl,
   pd->first_operand,
   pd->second_operand,
   pd->cqwrite_value);
}

void
gni_transport::print_event_data(gni_cq_entry_t event_data)
{
  gni_debug("event_data_data=%p\n"
   "event_data_source=%lu\n"
   "event_data_status=%lu\n"
   "event_data_info=%lu\n"
   "event_data_inst_id=%lu\n"
   "event_data_tid=%lu\n"
   "event_data_msg_id=%lu\n"
   "event_data_type=%lu",
   gni_cq_get_data(event_data),
   gni_cq_get_source(event_data),
   gni_cq_get_status(event_data),
   gni_cq_get_info(event_data),
   gni_cq_get_inst_id(event_data),
   gni_cq_get_tid(event_data),
   gni_cq_get_msg_id(event_data),
   gni_cq_get_type(event_data));
}

}

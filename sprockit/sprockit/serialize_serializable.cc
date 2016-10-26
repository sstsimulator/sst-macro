#include <sprockit/ptr_type.h>
#include <sprockit/spkt_string.h>
#include <sprockit/serialize_serializable.h>

namespace sprockit {
namespace pvt {

static const long null_ptr_id = -1;

void
size_serializable(serializable* s, serializer& ser){
  long dummy = 0;
  ser.size(dummy);
  if (s) {
    s->serialize_order(ser);
  }
}

void
pack_serializable(serializable* s, serializer& ser){
  if (s) {
    debug_printf(dbg::serialize,
      "packing object with class id %ld: %s",
      s->cls_id(), s->cls_name());
    long cls_id = s->cls_id();
    ser.pack(cls_id);
    s->serialize_order(ser);
  }
  else {
    debug_printf(dbg::serialize, "null object");
    long id = null_ptr_id;
    ser.pack(id);
  }
}

void
unpack_serializable(serializable*& s, serializer& ser){
  long cls_id;
  ser.unpack(cls_id);
  if (cls_id == null_ptr_id) {
    debug_printf(dbg::serialize, "null pointer object");
    
  }
  else {
    debug_printf(dbg::serialize, "unpacking class id %ld", cls_id);
    s = sprockit::serializable_factory::get_serializable(cls_id);
    s->serialize_order(ser);
    debug_printf(dbg::serialize, "unpacked object %s", s->cls_name());
  }
}

}
}

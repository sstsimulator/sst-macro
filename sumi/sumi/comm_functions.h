#ifndef sumi_api_FUNCTIONS_H
#define sumi_api_FUNCTIONS_H

namespace sumi {

typedef void (*reduce_fxn)(void*,const void*,int);

typedef void (*vote_fxn)(int&,const int&);

template <typename data_t>
struct Add {
  typedef data_t type;
    static void
    op(data_t& dst, const data_t& src){
        dst += src;
    }
};

template <typename data_t>
struct Min
{
  typedef data_t type;
  static void
  op(data_t& dst, const data_t& src){
    dst = dst < src ? dst : src;
  }
};

template <typename data_t>
struct Max
{
  typedef data_t type;
  static void
  op(data_t& dst, const data_t& src){
    dst = dst < src ? src : dst;
  }
};

struct Null
{
  static void
  op(void* dst_buffer, const void* src_buffer, int nelems){}
};


template <typename data_t>
struct And
{
  typedef data_t type;
  static void
  op(data_t& dst, const data_t& src){
    dst = dst && src;
  }
};

template <template <typename> class Fxn, typename data_t>
struct ReduceOp
{
  static void
  op(void* dst_buffer, const void* src_buffer, int nelems){
    data_t* dst = reinterpret_cast<data_t*>(dst_buffer);
    const data_t* src = reinterpret_cast<const data_t*>(src_buffer);
    for (int i=0; i < nelems; ++i, ++src, ++dst){
        Fxn<data_t>::op(*dst, *src);
    }
  }
};

}

#endif // SIMPMSG_FUNCTIONS_H

#include <sumi-mpi/mpi_api.h>
#include <sstmac/common/thread_lock.h>
#include <climits>

namespace sstmac {
namespace sumi {

struct fi {
  float a;
  int b;
};

struct di {
  double a;
  int b;
};
struct ii {
  int a;
  int b;
};
struct si {
  short a;
  int b;
};
struct li {
  long a;
  int b;
};
struct ldi {
  long double a;
  int b;
};

struct complex {
  float r;
  float i;
};

struct dcomplex {
  double r;
  double i;
};

struct ldcomplex {
  long double r;
  long double i;
};



void
mpi_api::precommit_types()
{
  static const int builtin_sizes[] = {1, 2, 4, 6, 8, 12, 16, 20, 32, 48, 64};
  static const int num_builtins = sizeof(builtin_sizes) / sizeof(int);

  static thread_lock lock;
  lock.lock();
  if (!mpi_type::mpi_null->committed()){
    mpi_type::mpi_null->init_primitive("MPI_NULL", 0, 0, MPI_COMBINER_NAMED);

    mpi_type::mpi_char->init_primitive("MPI_CHAR", 1, 4, MPI_COMBINER_NAMED);
    mpi_type::mpi_char->init_ops<char>();

    mpi_type::mpi_signed_char->init_primitive("MPI_SIGNED_CHAR", 1, 4, MPI_COMBINER_NAMED);

    mpi_type::mpi_wchar
          ->init_primitive("MPI_WCHAR", 2, 4, MPI_COMBINER_NAMED);

    mpi_type::mpi_unsigned_long_long
          ->init_primitive("MPI_UNSIGNED_LONG_LONG", 8, 8, MPI_COMBINER_NAMED);

    mpi_type::mpi_lb->init_primitive("MPI_LB", 0, 0, MPI_COMBINER_NAMED);

    mpi_type::mpi_ub->init_primitive("MPI_UB", 0, 0, MPI_COMBINER_NAMED);

    mpi_type::mpi_unsigned_char->init_primitive("MPI_UNSIGNED_CHAR", 1, 4, MPI_COMBINER_NAMED);

    mpi_type::mpi_byte->init_primitive("MPI_BYTE", 1, 4, MPI_COMBINER_NAMED);

    mpi_type::mpi_short->init_primitive("MPI_SHORT", 2, 4, MPI_COMBINER_NAMED);

    mpi_type::mpi_unsigned_short->init_primitive("MPI_UNSIGNED_SHORT", 2, 4, MPI_COMBINER_NAMED);

    mpi_type::mpi_int->init_primitive("MPI_INT", 4, 4, MPI_COMBINER_NAMED);
    mpi_type::mpi_int->init_ops<int>();

    mpi_type::mpi_unsigned->init_primitive("MPI_UNSIGNED", 4, 4, MPI_COMBINER_NAMED);
    mpi_type::mpi_unsigned->init_ops<unsigned>();

    mpi_type::mpi_long->init_primitive("MPI_LONG", 8, 8, MPI_COMBINER_NAMED);
    mpi_type::mpi_long->init_ops<long>();

    mpi_type::mpi_unsigned_long->init_primitive("MPI_UNSIGNED_LONG", 8, 8, MPI_COMBINER_NAMED);
    mpi_type::mpi_unsigned_long->init_ops<unsigned long>();

    mpi_type::mpi_float->init_primitive("MPI_FLOAT", 4, sizeof(float), MPI_COMBINER_NAMED);
    mpi_type::mpi_float->init_ops<float>();

    mpi_type::mpi_double->init_primitive("MPI_DOUBLE", 8, 8, MPI_COMBINER_NAMED);
    mpi_type::mpi_double->init_ops<double>();

    mpi_type::mpi_long_double->init_primitive("MPI_LONG_DOUBLE", 16, 16, MPI_COMBINER_NAMED);

    mpi_type::mpi_long_long_int->init_primitive("MPI_LONG_LONG_INT", 8, 8, MPI_COMBINER_NAMED);

    mpi_type::mpi_long_long->init_primitive("MPI_LONG_LONG", 8, 8, MPI_COMBINER_NAMED);

    mpi_type::mpi_packed->init_primitive("MPI_PACKED", 1, 0, MPI_COMBINER_NAMED);

    mpi_type::mpi_float_int->init_primitive("MPI_FLOAT_INT", mpi_type::mpi_float,
                                       mpi_type::mpi_int, sizeof(fi), MPI_COMBINER_NAMED);

    mpi_type::mpi_double_int->init_primitive("MPI_DOUBLE_INT",
                                        mpi_type::mpi_double, mpi_type::mpi_int, sizeof(di), MPI_COMBINER_NAMED);

    mpi_type::mpi_long_int->init_primitive("MPI_LONG_INT", mpi_type::mpi_long,
                                      mpi_type::mpi_int, sizeof(li), MPI_COMBINER_NAMED);

    mpi_type::mpi_short_int->init_primitive("MPI_SHORT_INT", mpi_type::mpi_short,
                                       mpi_type::mpi_int, sizeof(si), MPI_COMBINER_NAMED);

    mpi_type::mpi_2int->init_primitive("MPI_2INT", mpi_type::mpi_int,
                                  mpi_type::mpi_int, sizeof(ii), MPI_COMBINER_NAMED);

    mpi_type::mpi_long_double_int->init_primitive("MPI_LONG_DOUBLE_INT",
        mpi_type::mpi_long_double, mpi_type::mpi_int, sizeof(ldi),
        MPI_COMBINER_NAMED);

    //fortran nonsense
    mpi_type::mpi_complex->init_primitive("MPI_COMPLEX", mpi_type::mpi_float,
                                     mpi_type::mpi_float, sizeof(complex), MPI_COMBINER_NAMED);

    mpi_type::mpi_double_complex->init_primitive("MPI_DOUBLE_COMPLEX",
        mpi_type::mpi_double, mpi_type::mpi_double, sizeof(dcomplex),
        MPI_COMBINER_NAMED);

    mpi_type::mpi_logical->init_primitive("MPI_LOGICAL", 1, 4, MPI_COMBINER_NAMED);

    mpi_type::mpi_real->init_primitive("MPI_REAL", 4, 4, MPI_COMBINER_NAMED);

    mpi_type::mpi_double_precision->init_primitive("MPI_DOUBLE_PRECISION", 8, 8, MPI_COMBINER_NAMED);

    mpi_type::mpi_integer->init_primitive("MPI_INTEGER", 4, 4, MPI_COMBINER_NAMED);

    mpi_type::mpi_integer1->init_primitive("MPI_INTEGER1", 1, 4, MPI_COMBINER_NAMED);

    mpi_type::mpi_integer2->init_primitive("MPI_INTEGER2", 2, 4, MPI_COMBINER_NAMED);

    mpi_type::mpi_integer4->init_primitive("MPI_INTEGER4", 4, 4, MPI_COMBINER_NAMED);

    mpi_type::mpi_integer8->init_primitive("MPI_INTEGER8", 8, 8, MPI_COMBINER_NAMED);

    mpi_type::mpi_real4->init_primitive("MPI_REAL4", 4, 4, MPI_COMBINER_NAMED);

    mpi_type::mpi_real8->init_primitive("MPI_REAL8", 8, 8, MPI_COMBINER_NAMED);

    mpi_type::mpi_real16->init_primitive("MPI_REAL16", 16, 16, MPI_COMBINER_NAMED);

    mpi_type::mpi_complex8->init_primitive("MPI_COMPLEX8", mpi_type::mpi_float,
                                      mpi_type::mpi_float, sizeof(complex), MPI_COMBINER_NAMED);

    mpi_type::mpi_complex16->init_primitive("MPI_COMPLEX16", mpi_type::mpi_double,
                                       mpi_type::mpi_double, sizeof(dcomplex), MPI_COMBINER_NAMED);

    mpi_type::mpi_complex32->init_primitive("MPI_COMPLEX32",
                                       mpi_type::mpi_long_double, mpi_type::mpi_long_double, sizeof(ldcomplex),
                                       MPI_COMBINER_NAMED);

    //fortran pairs
    mpi_type::mpi_2integer->init_primitive("MPI_2INTEGER", mpi_type::mpi_int,
                                      mpi_type::mpi_int, sizeof(ii), MPI_COMBINER_NAMED);

    mpi_type::mpi_2complex->init_primitive("MPI_2COMPLEX", mpi_type::mpi_complex,
                                      mpi_type::mpi_complex, 16, MPI_COMBINER_NAMED);

    mpi_type::mpi_2double_complex->init_primitive("MPI_2DOUBLE_COMPLEX",
        mpi_type::mpi_double_complex, mpi_type::mpi_double_complex, 16,
        MPI_COMBINER_NAMED);

    mpi_type::mpi_2real->init_primitive("MPI_2REAL", mpi_type::mpi_real,
                                   mpi_type::mpi_real, 8, MPI_COMBINER_NAMED);

    mpi_type::mpi_2double_precision->init_primitive("MPI_2DOUBLE_PRECISION",
        mpi_type::mpi_double_precision, mpi_type::mpi_double_precision, 16,
        MPI_COMBINER_NAMED);

    mpi_type::mpi_character->init_primitive("MPI_CHARACTER", 1, 4,
         MPI_COMBINER_NAMED);

    for (int i=0; i < num_builtins; ++i){
      int size = builtin_sizes[i];
      std::string label = sprockit::printf("Built-in type size %d", size);
      mpi_type::builtins[size].init_primitive(label,
        size, 0, MPI_COMBINER_NAMED);
    }
  }

  precommit_type(mpi_type::mpi_null, MPI_NULL);

  precommit_type(mpi_type::mpi_char, MPI_CHAR);

  precommit_type(mpi_type::mpi_unsigned_char, MPI_UNSIGNED_CHAR);

  precommit_type(mpi_type::mpi_signed_char, MPI_SIGNED_CHAR);

  precommit_type(mpi_type::mpi_wchar, MPI_WCHAR);

  precommit_type(mpi_type::mpi_unsigned_long_long, MPI_UNSIGNED_LONG_LONG);

  precommit_type(mpi_type::mpi_lb, MPI_LB);

  precommit_type(mpi_type::mpi_ub, MPI_UB);

  precommit_type(mpi_type::mpi_byte, MPI_BYTE);

  precommit_type(mpi_type::mpi_double, MPI_DOUBLE);

  precommit_type(mpi_type::mpi_int, MPI_INT);

  precommit_type(mpi_type::mpi_unsigned, MPI_UNSIGNED);

  precommit_type(mpi_type::mpi_short, MPI_SHORT);

  precommit_type(mpi_type::mpi_unsigned_short, MPI_UNSIGNED_SHORT);

  precommit_type(mpi_type::mpi_long, MPI_LONG);

  precommit_type(mpi_type::mpi_long_long_int, MPI_LONG_LONG_INT);

  precommit_type(mpi_type::mpi_unsigned_long, MPI_UNSIGNED_LONG);

  precommit_type(mpi_type::mpi_float, MPI_FLOAT);

  precommit_type(mpi_type::mpi_double_int, MPI_DOUBLE_INT);

  precommit_type(mpi_type::mpi_2int, MPI_2INT);

  precommit_type(mpi_type::mpi_float_int, MPI_FLOAT_INT);

  precommit_type(mpi_type::mpi_long_int, MPI_LONG_INT);

  precommit_type(mpi_type::mpi_short_int, MPI_SHORT_INT);

  precommit_type(mpi_type::mpi_long_double, MPI_LONG_DOUBLE);

  precommit_type(mpi_type::mpi_long_double_int, MPI_LONG_DOUBLE_INT);

  precommit_type(mpi_type::mpi_packed, MPI_PACKED);

  //fortran nonsense
  precommit_type(mpi_type::mpi_complex, MPI_COMPLEX);

  precommit_type(mpi_type::mpi_double_complex, MPI_DOUBLE_COMPLEX);

  precommit_type(mpi_type::mpi_logical, MPI_LOGICAL);

  precommit_type(mpi_type::mpi_real, MPI_REAL);

  precommit_type(mpi_type::mpi_double_precision, MPI_DOUBLE_PRECISION);

  precommit_type(mpi_type::mpi_integer, MPI_INTEGER);

  precommit_type(mpi_type::mpi_integer1, MPI_INTEGER1);

  precommit_type(mpi_type::mpi_integer2, MPI_INTEGER2);

  precommit_type(mpi_type::mpi_integer4, MPI_INTEGER4);

  precommit_type(mpi_type::mpi_integer8, MPI_INTEGER8);

  precommit_type(mpi_type::mpi_real4, MPI_REAL4);

  precommit_type(mpi_type::mpi_real8, MPI_REAL8);

  precommit_type(mpi_type::mpi_real16, MPI_REAL16);

  precommit_type(mpi_type::mpi_complex8, MPI_COMPLEX8);

  precommit_type(mpi_type::mpi_complex16, MPI_COMPLEX16);

  precommit_type(mpi_type::mpi_complex32, MPI_COMPLEX32);

    //fortran pairs
  precommit_type(mpi_type::mpi_2integer, MPI_2INTEGER);
  //precommit_type(mpi_type::mpi_2complex, MPI_2COMPLEX);
  //precommit_type(mpi_type::mpi_2double_complex, MPI_2DOUBLE_COMPLEX);

  precommit_type(mpi_type::mpi_2real, MPI_2REAL);

  precommit_type(mpi_type::mpi_2double_precision, MPI_2DOUBLE_PRECISION);

  precommit_type(mpi_type::mpi_character, MPI_CHARACTER);

  for (int i=0; i < num_builtins; ++i){
    int size = builtin_sizes[i];
    allocate_type_id(&mpi_type::builtins[size]);
  }

  lock.unlock();
}

int
mpi_api::type_set_name(MPI_Datatype id, const std::string &name)
{
  type_map::iterator it = known_types_.find(id);
  if (it == known_types_.end()){
    spkt_throw_printf(sprockit::value_error,
        "mpi_api::type_set_name: cannot set name %s for unknown id %d",
        name.c_str(), int(id));
  }
  it->second->label = name;
  return MPI_SUCCESS;
}

int
mpi_api::type_indexed(int count, int lens[], const int* ind,
                      MPI_Datatype intype, MPI_Datatype* outtype, bool in_elem, int comb)
{
  mpi_type* out_type_obj = new mpi_type;
  if (count > 0) {
    mpi_type* in_type_obj = type_from_id(intype);
    inddata* idata = new inddata;
    int maxdisp = -1;
    int maxindex = 0;
    int mindisp = INT_MAX;
    int index = 0;
    int typesizes = 0;
    for (int i = 0; i < count; i++) {
      if (lens[i] > 0) {
        int bytesdisp;
        if (in_elem) {
          bytesdisp = ind[i] * in_type_obj->extent();
        }
        else {
          bytesdisp = ind[i];
        }


        idata->blocks[index] = ind_block();
        idata->blocks[index].base = in_type_obj;
        idata->blocks[index].disp = bytesdisp;
        idata->blocks[index].num = lens[i];
        typesizes += lens[i] * in_type_obj->packed_size();
        //  size += intype.size * lens[i];
        if (bytesdisp > maxdisp) {
          maxdisp = bytesdisp;
          maxindex = i;
        }

        if (bytesdisp < mindisp) {
          mindisp = bytesdisp;
        }
        index++;
      }

    }

    idata->ub = maxdisp + in_type_obj->extent() * lens[maxindex];
    idata->lb = mindisp;
    idata->mindisp = mindisp;
    idata->maxbyte = maxdisp + in_type_obj->extent() * lens[maxindex];

    out_type_obj->init_indexed(in_type_obj->label, idata, typesizes,
                       idata->ub - idata->lb, index * 2 + 1, 1, comb);


  }
  else {
    inddata* idata = new inddata;
    idata->ub = 0;
    idata->lb = 0;
    idata->mindisp = 0;
    idata->maxbyte = 0;

    out_type_obj->init_indexed("struct", idata, 0, 0, 1, 0, comb);
  }
  allocate_type_id(out_type_obj);
  *outtype = out_type_obj->id;
  return MPI_SUCCESS;
}

std::string
mpi_api::type_label(MPI_Datatype tid)
{
  mpi_type* ty = type_from_id(tid);
  return ty->label;
}

//
// A datatype object has to be committed before use in communication.
//
int
mpi_api::type_commit(MPI_Datatype type)
{
  mpi_type* type_obj = type_from_id(type);
  type_obj->set_committed(true);
  return MPI_SUCCESS;
}

void
mpi_api::allocate_type_id(mpi_type* type)
{
  spkt_unordered_map<MPI_Datatype, mpi_type*>::iterator it, end = known_types_.end();
  while ((it = known_types_.find(next_type_id_)) != end){
    ++next_type_id_;
  }
  type->id = next_type_id_;
  known_types_[type->id] = type;
}

void
mpi_api::precommit_type(mpi_type* type, MPI_Datatype id)
{
  MPI_Datatype tid(id);
  if (known_types_.find(tid) != known_types_.end()){
    spkt_throw_printf(sprockit::value_error,
      "mpi_api::precommit_type: %d already exists",
      id);
  }
  type->id = tid;
  known_types_[tid] = type;
  known_types_[tid]->set_committed(true);
}

//
// Creates a contiguous datatype
//
int
mpi_api::type_contiguous(int count, MPI_Datatype old_type,
                         MPI_Datatype* new_type)
{
  mpi_type* new_type_obj = new mpi_type;
  mpi_type* old_type_obj = type_from_id(old_type);
  new_type_obj->init_vector("contiguous-" + old_type_obj->label,
                        old_type_obj,
                        count, 1,
                        1, true, MPI_COMBINER_CONTIGUOUS);

  allocate_type_id(new_type_obj);
  *new_type = new_type_obj->id;
  return MPI_SUCCESS;
}

/// Creates a vector (strided) datatype
int
mpi_api::type_vector(int count, int blocklength, int stride,
                     MPI_Datatype old_type, MPI_Datatype &new_type, bool stride_in_elem)
{
  int comb = (stride_in_elem) ? MPI_COMBINER_VECTOR : MPI_COMBINER_HVECTOR;
  std::stringstream ss;
  ss << "vector-" << type_label(old_type) << "\n";

  mpi_type* new_type_obj = new mpi_type;
  new_type_obj->init_vector(ss.str(), type_from_id(old_type),
                        count, blocklength, stride,
                        stride_in_elem, comb);

  allocate_type_id(new_type_obj);
  new_type = new_type_obj->id;
  return MPI_SUCCESS;
}

//
// Creates a struct datatype
//
int
mpi_api::type_struct(const int count, const int* blocklens,
                     const int* indices, const MPI_Datatype* old_types,
                     MPI_Datatype &newtype)
{
  mpi_type* new_type_obj = new mpi_type;
  if (count > 0) {
    inddata* idata = new inddata;
    int maxdisp = -1;
    int maxindex = 0;
    int typesizes = 0;
    int maxsize = 0;
    int mindisp = INT_MAX;
    int index = 0;
    int ub = -1;
    int lb = -1;
    bool ubset = false;
    bool lbset = false;
    for (int i = 0; i < count; i++) {
      if (old_types[i] == mpi_type::mpi_lb->id) {
        lb = indices[i];
        lbset = true;
      }
      else if (old_types[i] == mpi_type::mpi_ub->id) {
        ub = indices[i];
        ubset = true;

        //if (ub > maxdisp)
        // {
        //  maxdisp = ub;
        // maxindex = i;
        // maxsize = 0;
        // }

      }
      else if (blocklens[i] > 0) {
        mpi_type* old_type_obj = type_from_id(old_types[i]);
        idata->blocks[index] = ind_block();
        idata->blocks[index].base = old_type_obj;
        idata->blocks[index].disp = indices[i];
        idata->blocks[index].num = blocklens[i];
        typesizes += old_type_obj->packed_size() * blocklens[i];
        if (indices[i] > maxdisp) {
          maxdisp = indices[i];
          maxindex = i;
          maxsize = old_type_obj->extent();
        }

        if (indices[i] < mindisp) {
          mindisp = indices[i];
        }

        index++;
      }
    }

    if (!lbset) {
      lb = mindisp;
    }
    if (!ubset) {
      if (index == 0) {
        //there was no data
        ub = 0;
      }
      else {
        ub = maxdisp + maxsize * blocklens[maxindex];
      }
    }
    idata->ub = ub;
    idata->lb = lb;
    idata->mindisp = mindisp;
    idata->maxbyte = maxdisp + maxsize * blocklens[maxindex];

    new_type_obj->init_indexed("struct", idata, typesizes, ub - lb,
                         index * 2 + 1, index, MPI_COMBINER_STRUCT);


  }
  else {
    inddata* idata = new inddata;
    idata->ub = 0;
    idata->lb = 0;
    idata->mindisp = 0;
    idata->maxbyte = 0;

    new_type_obj->init_indexed("struct", idata, 0, 0, 1, 0, MPI_COMBINER_STRUCT);
  }
  allocate_type_id(new_type_obj);
  newtype = new_type_obj->id;
  return MPI_SUCCESS;
}

int
mpi_api::type_size(MPI_Datatype type)
{
  return type_from_id(type)->packed_size();
}

int
mpi_api::type_dup(MPI_Datatype intype, MPI_Datatype* outtype)
{
  mpi_type* new_type_obj = type_from_id(intype);
  if (new_type_obj->comb() == MPI_COMBINER_NAMED) {
    new_type_obj->set_comb(MPI_COMBINER_DUP);
  }
  allocate_type_id(new_type_obj);
  *outtype = new_type_obj->id;
  return MPI_SUCCESS;
}


//
// Mark datatype for deallocation.
//
int
mpi_api::type_free(MPI_Datatype type)
{
  mpi_type* type_obj = type_from_id(type);
  type_obj->set_committed(false);
  return MPI_SUCCESS;
}

//
// Get the derived mpitype mapped to an id
//
mpi_type*
mpi_api::type_from_id(MPI_Datatype id)
{
  type_map::iterator it = known_types_.find(id);
  if (it == known_types_.end()){
    spkt_throw_printf(sprockit::invalid_key_error,
        "mpi_api: unknown type id %d",
        int(id));
  }
  return it->second;
}

}
}

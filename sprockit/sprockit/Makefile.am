#
#  This file is part of SST/macroscale:
#               The macroscale architecture simulator from the SST suite.
#  Copyright (c) 2009 Sandia Corporation.
#  This software is distributed under the BSD License.
#  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
#  the U.S. Government retains certain rights in this software.
#  For more information, see the LICENSE file in the top
#  SST/macroscale directory.
#

ACLOCAL_AMFLAGS = -I acinclude

include $(top_srcdir)/Makefile.common

SUBDIRS = 

lib_LTLIBRARIES = libsprockit.la

libsprockit_la_SOURCES = \
  sim_parameters.cc \
  malloc.cc \
  debug.cc \
  statics.cc \
  output.cc \
  fileio.cc \
  factories/factory.cc \
  serialize_serializable.cc \
  serializer.cc \
  spkt_string.cc \
  serializable.cc \
  units.cc \
  driver_util.cc \
  param_expander.cc \
  test/test.cc \
  keyword_registration.cc

library_includedir=$(includedir)/sprockit

nodist_library_include_HEADERS = spkt_config.h 

nobase_library_include_HEADERS = \
  basic_string_tokenizer.h \
  debug.h \
  delete.h \
  errors.h \
  fileio.h \
  driver_util.h \
  ser_ptr_type.h \
  metadata_bits.h \
  opaque_typedef.h \
  output.h \
  ptr_type.h \
  mpi_parameters.h \
  refcount_ptr.h \
  preprocessor.h \
  sim_parameters.h \
  sim_parameters_fwd.h \
  statics.h \
  spkt_string.h \
  stl_string.h \
  spkt_new.h \
  malloc.h \
  typedefs.h \
  util.h \
  units.h \
  factories/factory.h \
  serializer.h \
  serializer_fwd.h \
  serializable.h \
  serializable_type.h \
  serializable_fwd.h \
  serialize.h \
  serialize_array.h \
  serialize_buffer_accessor.h \
  serialize_list.h \
  serialize_map.h \
  serialize_packer.h \
  serialize_serializable.h \
  serialize_set.h \
  serialize_sizer.h \
  serialize_string.h \
  serialize_unpacker.h \
  serialize_vector.h \
  param_expander.h \
  unordered.h \
  test/assert.h \
  test/test.h \
  test/equality.h \
  test/fxn.h \
  test/container.h \
  test/output.h \
  keyword_registration.h

if HAVE_REGEXP
libsprockit_la_SOURCES += regexp.cc
nobase_library_include_HEADERS += regexp.h
endif


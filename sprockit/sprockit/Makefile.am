#
#  This file is part of SST/macroscale:
#               The macroscale architecture simulator from the SST suite.
#  Copyright (c) 2009-2018 NTESS.
#  This software is distributed under the BSD License.
#  Under the terms of Contract DE-NA0003525 with NTESS,
#  the U.S. Government retains certain rights in this software.
#  For more information, see the LICENSE file in the top
#  SST/macroscale directory.
#

ACLOCAL_AMFLAGS = -I acinclude

include $(top_srcdir)/Makefile.common

SUBDIRS = 

noinst_LTLIBRARIES = libspkt_temp.la
lib_LTLIBRARIES = libsprockit.la
libsprockit_la_LIBADD = libspkt_temp.la
EXTRA_DIST = libsprockit.c

SOURCES = \
  sim_parameters.cc \
  malloc.cc \
  debug.cc \
  statics.cc \
  output.cc \
  fileio.cc \
  spkt_string.cc \
  units.cc \
  driver_util.cc \
  test/test.cc \
  keyword_registration.cc

libspkt_temp_la_SOURCES = $(SOURCES)

library_includedir=$(includedir)/sprockit


nodist_library_include_HEADERS = spkt_config.h 

nobase_library_include_HEADERS = \
  basic_string_tokenizer.h \
	allocator.h \
  debug.h \
  delete.h \
  errors.h \
  fileio.h \
  driver_util.h \
  metadata_bits.h \
  output.h \
  mpi_parameters.h \
  sim_parameters.h \
  sim_parameters_fwd.h \
  statics.h \
  spkt_string.h \
  spkt_printf.h \
  printable.h \
  thread_safe_new.h \
  thread_safe.h \
  stl_string.h \
  spkt_new.h \
  malloc.h \
  typedefs.h \
  util.h \
  units.h \
  factories/factory.h \
  test/assert.h \
  test/test.h \
  test/equality.h \
  test/fxn.h \
  test/container.h \
  test/output.h \
  keyword_registration.h

if HAVE_REGEXP
libspkt_temp_la_SOURCES += regexp.cc
nobase_library_include_HEADERS += regexp.h
endif

if !INTEGRATED_SST_CORE
libspkt_temp_la_SOURCES += \
  serialize_serializable.cc \
  serializer.cc \
  serializable.cc 

nobase_library_include_HEADERS += \
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
  serialize_vector.h 
endif

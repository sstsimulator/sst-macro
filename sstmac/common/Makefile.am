#
#   This file is part of SST/macroscale: 
#                The macroscale architecture simulator from the SST suite.
#   Copyright (c) 2009 Sandia Corporation.
#   This software is distributed under the BSD License.
#   Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
#   the U.S. Government retains certain rights in this software.
#   For more information, see the LICENSE file in the top 
#   SST/macroscale directory.
#

include $(top_srcdir)/Makefile.common

noinst_LTLIBRARIES = libsstmac_common.la

distclean-local:
	rm -f sstmac_config.h

libsstmac_common_la_SOURCES = \
  runtime.cc \
  cartgrid.cc \
  event_scheduler.cc \
  mersenne_twister.cc \
  rng.cc \
  timestamp.cc \
  c_params.cc \
  sst_event.cc \
  lodepng.cc \
  param_expander.cc \
  sstmac_env.cc \
  thread_lock.cc \
  messages/message_buffer_cache.cc \
  messages/library_message.cc \
  messages/timed_event.cc \
  stats/stat_collector.cc \
  stats/stat_spyplot.cc \
  stats/stat_histogram.cc \
  stats/event_trace.cc \
  stats/location_trace.cc \
  stats/stat_local_int.cc \
  stats/stat_local_double.cc \
  stats/stat_global_int.cc

nodist_library_include_HEADERS = sstmac_config.h config.h

library_includedir=$(includedir)/sstmac/common

nobase_library_include_HEADERS = \
  event_manager_fwd.h \
  cartgrid.h \
  c_params.h \
  event_callback.h \
  event_handler.h \
  event_handler_fwd.h \
  event_location.h \
  event_scheduler.h \
  event_scheduler_fwd.h \
  mersenne_twister.h \
  node_address.h \
  rng.h	\
  runtime.h \
  timestamp.h \
  timestamp_fwd.h \
  sst_event.h \
  sst_event_fwd.h \
  lodepng.h \
  sstmac_env.h \
  param_expander.h \
  thread_safe_int.h \
  thread_lock.h \
  sim_thread_lock.h \
  serializable.h \
  messages/library_message.h \
  messages/message_buffer_cache.h \
  messages/sleep_event.h \
  messages/sst_message.h \
  messages/sst_message_fwd.h \
  messages/timed_event.h \
  stats/stat_spyplot.h \
  stats/stat_spyplot_fwd.h \
  stats/stat_collector.h \
  stats/stat_collector_fwd.h \
  stats/stat_histogram.h \
  stats/stat_histogram_fwd.h \
  stats/event_trace.h \
  stats/location_trace.h \
  stats/stat_local_int.h \
  stats/stat_local_int_fwd.h \
  stats/stat_local_double.h \
  stats/stat_local_double_fwd.h \
  stats/stat_global_int.h \
  stats/stat_global_int_fwd.h

if !INTEGRATED_SST_CORE
nobase_library_include_HEADERS += \
  event_manager.h 

libsstmac_common_la_SOURCES += \
  event_manager.cc 

endif



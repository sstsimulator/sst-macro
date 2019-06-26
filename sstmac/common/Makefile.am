#
#   This file is part of SST/macroscale: 
#                The macroscale architecture simulator from the SST suite.
#   Copyright (c) 2009-2018 NTESS.
#   This software is distributed under the BSD License.
#   Under the terms of Contract DE-NA0003525 with NTESS,
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
  sstmac_env.cc \
  thread_lock.cc \
  stats/ftq.cc \
  stats/ftq_tag.cc \
  stats/stat_accumulator.cc \
  stats/stat_histogram.cc \
  stats/stat_collector.cc \
  stats/stat_spyplot.cc

nodist_library_include_HEADERS = sstmac_config.h config.h

library_includedir=$(includedir)/sstmac/common

nobase_library_include_HEADERS = \
  event_manager_fwd.h \
  cartgrid.h \
  c_params.h \
  ipc_event.h \
  event_callback.h \
  event_handler.h \
  handler_event_queue_entry.h \
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
  request.h \
  request_fwd.h \
  sst_event.h \
  sst_event_fwd.h \
  sstmac_env.h \
  thread_safe_int.h \
  thread_lock.h \
  sim_thread_lock.h \
  serializable.h \
  stats/ftq_tag.h \
  stats/ftq.h \
  stats/ftq_fwd.h \
  stats/stat_accumulator.h \
  stats/stat_collector.h \
  stats/stat_collector_fwd.h \
  stats/stat_spyplot.h \
  stats/stat_spyplot_fwd.h \
  stats/stat_histogram.h \
  stats/stat_histogram_fwd.h 

if !INTEGRATED_SST_CORE
nobase_library_include_HEADERS += \
  event_manager.h

libsstmac_common_la_SOURCES += \
  event_manager.cc

endif



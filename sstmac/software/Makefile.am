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

AM_CPPFLAGS += -I$(top_builddir)/sst-dumpi -I$(top_srcdir)/sst-dumpi


noinst_LTLIBRARIES = libsstmac_sw.la
libsstmac_sw_la_LDFLAGS = 

libsstmac_sw_la_LIBADD = 

libsstmac_sw_la_SOURCES = \
  ami/ami.cc \
  api/api.cc \
  process/compute_scheduler.cc \
  process/cpuset_compute_scheduler.cc \
  process/simple_compute_scheduler.cc \
  process/gdb.cc \
  process/key.cc \
  process/global.cc \
  process/graphviz.cc \
  process/ftq.cc \
  process/operating_system.cc \
  process/thread.cc \
  process/thread_info.cc \
  process/app.cc \
  process/time.cc \
  threading/context_util.cc \
  threading/stack_alloc_chunk.cc \
  threading/stack_alloc.cc \
  threading/threading_pthread.cc \
  libraries/library.cc \
  libraries/compute/compute_api.cc \
  libraries/compute/compute_event.cc \
  libraries/compute/lib_compute.cc \
  libraries/compute/lib_compute_inst.cc \
  libraries/compute/lib_compute_memmove.cc \
  libraries/compute/lib_compute_time.cc \
  libraries/unblock_event.cc \
  launch/launch_request.cc \
  launch/node_allocator.cc \
  launch/cart_allocation.cc \
  launch/greedy_dfly_allocation.cc \
  launch/coordinate_allocation.cc \
  launch/random_allocation.cc \
  launch/node_id_allocation.cc \
  launch/hostname_allocation.cc \
  launch/dumpi_allocation.cc \
  launch/first_available_allocation.cc \
  launch/task_mapper.cc \
  launch/block_task_mapper.cc \
  launch/coordinate_task_mapper.cc \
  launch/dumpi_task_mapper.cc \
  launch/hostname_task_mapper.cc \
  launch/random_task_mapper.cc \
  launch/round_robin_task_mapper.cc \
  launch/node_id_task_mapper.cc \
  launch/job_launcher.cc \
  launch/app_launcher.cc 


library_includedir=$(includedir)/sstmac/software

nobase_library_include_HEADERS = \
  ami/ami.h \
  api/api.h \
  api/api_fwd.h \
  process/gdb.h \
  process/simple_compute_scheduler.h \
  process/cpuset_compute_scheduler.h \
  process/compute_scheduler.h \
  process/compute_scheduler_fwd.h \
  process/backtrace.h \
  process/graphviz.h \
  process/graphviz_fwd.h \
  process/ftq.h \
  process/ftq_fwd.h \
  process/app.h \
  process/app_fwd.h \
  process/app_id.h \
  process/key.h \
  process/host_timer.h \
  process/operating_system.h \
  process/operating_system_fwd.h \
  process/software_id.h \
  process/task_id.h \
  process/time.h \
  process/thread_info.h \
  process/tls.h \
  process/thread.h \
  process/std_thread.h \
  process/std_mutex.h \
  process/thread_fwd.h \
  process/thread_id.h \
  process/global.h \
  process/cppglobal.h \
  process/process_context.h \
  threading/context_util.h \
  threading/stack_alloc_chunk.h \
  threading/stack_alloc.h \
  threading/threading_interface.h \
  threading/threading_interface_fwd.h \
  libraries/compute/compute_api.h \
  libraries/compute/compute_event.h \
  libraries/compute/compute_event_fwd.h \
  libraries/compute/lib_compute_inst.h \
  libraries/compute/lib_compute_fwd.h \
  libraries/compute/lib_compute_memmove.h \
  libraries/compute/lib_compute_time.h \
  libraries/compute/lib_compute.h \
  libraries/library.h \
  libraries/library_fwd.h \
  libraries/unblock_event.h \
  libraries/service.h \
  libraries/service_fwd.h \
  launch/launch_request.h \
  launch/launch_request_fwd.h \
  launch/node_allocator.h \
  launch/cart_allocation.h  \
  launch/coordinate_allocation.h \
  launch/node_id_allocation.h \
  launch/random_allocation.h \
  launch/hostname_allocation.h \
  launch/dumpi_allocation.h \
  launch/first_available_allocation.h \
  launch/task_mapper.h \
  launch/block_task_mapper.h \
  launch/coordinate_task_mapper.h \
  launch/dumpi_task_mapper.h \
  launch/hostname_task_mapper.h \
  launch/random_task_mapper.h \
  launch/node_id_task_mapper.h \
  launch/round_robin_task_mapper.h  \
  launch/node_set.h \
  launch/job_launcher.h \
  launch/job_launcher_fwd.h \
  launch/app_launcher.h \
  launch/app_launcher_fwd.h \
  launch/job_launch_event.h \
  launch/launch_event.h  \
  launch/launch_event_fwd.h  

if HAVE_UCONTEXT
  libsstmac_sw_la_SOURCES += \
     threading/threading_ucontext.cc 
endif

if HAVE_PTH
  libsstmac_sw_la_SOURCES += \
     threading/threading_pth.cc 
endif

libsstmac_sw_la_SOURCES += \
     threading/threading_fcontext.cc

if FCONTEXT_X86
if FCONTEXT_MAC
   libsstmac_sw_la_SOURCES += \
    threading/asm/make_x86_64_sysv_macho_gas.S \
    threading/asm/jump_x86_64_sysv_macho_gas.S \
    threading/asm/ontop_x86_64_sysv_macho_gas.S 
else
   libsstmac_sw_la_SOURCES += \
    threading/asm/make_x86_64_sysv_elf_gas.S \
    threading/asm/jump_x86_64_sysv_elf_gas.S \
    threading/asm/ontop_x86_64_sysv_elf_gas.S 
endif
else #just assume i386 for now
if FCONTEXT_MAC
   libsstmac_sw_la_SOURCES += \
    threading/asm/make_i386_sysv_macho_gas.S \
    threading/asm/jump_i386_sysv_macho_gas.S \
    threading/asm/ontop_i386_sysv_macho_gas.S 
else
   libsstmac_sw_la_SOURCES += \
    threading/asm/make_i386_sysv_elf_gas.S \
    threading/asm/jump_i386_sysv_elf_gas.S \
    threading/asm/ontop_i386_sysv_elf_gas.S 
endif
endif





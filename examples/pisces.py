import sst
sst.setProgramOption("timebase", "100as")
import sst.macro
from sst.macro import Interconnect

swParams = {
  "name" : "pisces",
  "arbitrator" : "cut_through",
  "mtu" : "4096",
  "router" : {
    "seed" : "42",
    "name" : "dragonfly_minimal",
  },
  "link" : {
    "bandwidth" : "1.0GB/s",
    "latency" : "100ns",
    "credits" : "64KB",
  },
  "xbar" : {
    "bandwidth" : "10GB/s",
  },
  "logp" : {
    "bandwidth" : "1GB/s",
    "hop_latency" : "100ns",
    "out_in_latency" : "100ns",
  }
}

appParams = {
  "allocation" : "first_available",
  "indexing" : "block",
  "name" : "mpi_ping_all",
  "launch_cmd" : "aprun -n 80 -N 2",
  "start" : "0ms",
  "sleep_time" : "1us",
  "message_size" : "1KB"
}

memParams = {
 "name" : "pisces",
 "total_bandwidth" : "10GB/s",
 "latency" : "10ns",
 "max_single_bandwidth" : "10GB/s",
}

nicParams = {
  "name" : "pisces",
  "injection" : {
    "mtu" : "4096",
    "arbitrator" : "cut_through",
    "bandwidth" : "1.0GB/s",
    "latency" : "50ns",
    "credits" : "64KB",
  },
  "ejection" : {
    "latency" : "50ns",
  }
}

nodeParams = {
  "memory" : memParams,
  "nic" : nicParams,
  "app1" : appParams,
  "name" : "simple",
  "proc" : {
    "frequency" : "2GHz",
    "ncores" : "4",
  }
}

topoParams = {
 "name" : "dragonfly",
 "geometry" : "[4,3]",
 "h" : "6",
 "inter_group" : "circulant",
 "concentration" : "4",
}

params = {
  "node" : nodeParams,
  "switch" : swParams,
  "topology" : topoParams,
}

ic = Interconnect(params)
ic.build()

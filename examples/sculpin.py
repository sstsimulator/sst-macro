import sst
sst.setProgramOption("timebase", "100as")
import sst.macro
from sst.macro import Interconnect

swParams = {
  "name" : "sculpin",
  "router" : {
    "seed" : "42",
    "name" : "dragonfly_minimal",
  },
  "link" : {
    "bandwidth" : "1.0GB/s",
    "latency" : "100ns",
    "credits" : "4KB",
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
  "sleep_time" : "1us",
  "message_size" : "1KB",
}

memParams = {
 "name" : "logp",
 "bandwidth" : "10GB/s",
 "latency" : "10ns",
}

nicParams = {
  "name" : "sculpin",
  "injection" : {
    "bandwidth" : "1.0GB/s",
    "latency" : "50ns",
    "mtu" : "1024",
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

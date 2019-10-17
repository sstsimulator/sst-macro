import sst
sst.setProgramOption("timebase", "100as")
import sst.macro
from sst.macro import Interconnect

swParams = {
  "name" : "logp",
  "out_in_latency" : "2us",
}

appParams = {
  "allocation" : "first_available",
  "indexing" : "block",
  "name" : "mpi_ping_all",
  "launch_cmd" : "aprun -n 80 -N 2",
}

memParams = {
 "name" : "logp",
 "latency" : "10ns",
 "bandwidth" : "10GB/s",
}

nicParams = {
  "name" : "logp",
  "injection" : {
    "bandwidth" : "2.0GB/s",
    "latency" : "1us",
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
 "concentration" : "4",
}

params = {
  "node" : nodeParams,
  "switch" : swParams,
  "topology" : topoParams,
}

ic = Interconnect(params)
ic.build()

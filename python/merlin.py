from sst.merlin import *
from sst.macro import *
import sst.macro


mtu = 1204
arb = "cut_through"


params = sst.merlin._params

buffer_size = "64KB"

topo = topoTorus()
params["torus:shape"] = "2x2x2";
params["torus:width"] = "1x1x1";
params["flit_size"] = "8B"
params["link_bw"] = "10GB/s"
params["link_lat"] = "100ns"
params["xbar_bw"] = "15GB/s"
params["input_latency"] = "100ns"
params["output_latency"] = "100ns"
params["input_buf_size"] = buffer_size
params["output_buf_size"] = buffer_size
params["num_peers"] = 8
params["num_dims"] = 3
params["torus:local_ports"] = 1

memParams = {
  "latency" : "10ns",
  "bandwidth" : "10GB/s",
}

procParams = {
  "frequency" : "2.1GHz",
  "ncores" : 1,
}

nicParams = {
  "model" : "pisces",
  "mtu" : mtu,
  "arbitrator" : arb,
  "packetizer" : "merlin",
  "injection" : {
    "latency" : "1us",
    "bandwidth" : "10GB/s",
    "credits" : buffer_size,
  },
  "module" : "merlin.linkcontrol",
}

appParams = {
  "name" : "mpi_coverage",
  "start" : "0ms",
  "launch_cmd" : "aprun -n 8 -N 1",
}


nodeParams = {
  "nic" : nicParams,
  "memory" : memParams,
  "proc" : procParams,
  "topology" : {
    "name" : "merlin",
    "num_nodes" : 8,
    "num_switches" : 8,
  },
  "app1" : appParams,
}


topo.prepParams()

nodeParams = macroToCoreParams(nodeParams)

class TestEP(EndPoint):
  def build( self, nodeID, extraKeys ):
    node = sst.Component( "node" + str(nodeID), "macro.simple_node" )
    node.addParams(extraKeys)
    node.addParams(nodeParams)
    node.addParam("id", nodeID)
    return (node, "rtr", params["link_lat"])

ep = TestEP()
topo.setEndPoint(ep)
topo.build()

#sst.macro.debug("mpi", "timestamp")
#sst.macro.debug("packetizer")





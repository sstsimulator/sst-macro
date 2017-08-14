import sst
from sst.macro import *
import sst.test

latency="1us"
comp1 = sst.Component("1", "test.dummy_switch")
comp1.addParam("id", 1)
comp1.addParam("latency", latency)

comp2 = sst.Component("2", "test.dummy_switch")
comp2.addParam("id", 2)
comp2.addParam("latency", latency)

port=0
comp1Id=1
comp2Id=2
makeBiNetworkLink(comp1,comp1Id,port,
                  comp2,comp2Id,port,
                  latency)


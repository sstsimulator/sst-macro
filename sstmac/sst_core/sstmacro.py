# Load module function in Python is changed
# to look for a libmacro.so in LD_LIBRARY_PATH
import sst
import sst.macro
import sstmac

def addNew(prefix, kw, newDict, oldDict):
  name = "%s.%s" % (prefix, kw)
  newDict[name] = oldDict[kw]

def addParams(prefix, dict, *xargs, **kwargs):
  for entry in xargs:
    if isinstance(entry, type({})):
      for kw in entry:
        addNew(prefix, kw, dict, entry)
    else: #function
      entry(prefix, dict)
    
  for kw in kwargs:
    addNew(prefix, kw, dict, kwargs)

def addSubParams(oldPrefix, newPrefix, dict, *xargs, **kwargs):
  prefix = "%s.%s" % (oldPrefix, newPrefix)
  addParams(prefix, dict, *xargs, **kwargs)

def subParams(prefix, *xargs, **kwargs):
  return lambda x,y: addSubParams(x, prefix, y, *xargs, **kwargs)

def redoSubParams_impl(nsArr, theDict, allParams):
  for key in theDict:
    val = theDict[key]
    if isinstance(val, dict):
      newNsArr = nsArr[:]
      newNsArr.append(key)
      redoSubParams_impl(newNsArr, val, allParams)
    else:
      paramArr = nsArr[:]
      paramArr.append(key)
      newParam = ".".join(paramArr)
      allParams.append((newParam, val))

def macroToCoreParams(theDict):
  allParams = []
  redoSubParams_impl([], theDict, allParams)
  newDict = {}
  for key, val in allParams:
    newDict[key] = val
  return newDict

class Interconnect:
  def __init__(self, params):
    self.params = params
    self.system = sst.macro.System(params)
    self.num_nodes = self.system.numNodes()
    self.num_switches = self.system.numSwitches()
    self.switches = {}
    self.nodes = {}

  def buildSwitches(self):
    for i in range(self.num_switches):
      switchParams = self.system.switchParams(i)
      compName = switchParams["model"] + "_network_switch"
      switch = sst.Component("Switch %d" % i, "macro.%s" % compName)
      switch.addParams(macroToCoreParams(switchParams))
      switch.addParam("id", i)
      self.switches[i] = (switch, switchParams)

  def buildEndpoints(self):
    nodeParams = self.params["node"]
    compName = nodeParams["model"] + "_node"
    for i in range(self.num_nodes):
      node = sst.Component("Node %d" % i, "macro.%s" % compName)
      node.addParams(macroToCoreParams(nodeParams))
      node.addParam("id", i)
      self.nodes[i] = node

  def connectSwitches(self):
    switchParams = self.params["switch"]
    for i in range(self.num_switches):
      linkParams = switchParams["link"]
      connections = self.system.switchConnections(i)
      srcSwitch, params = self.switches[i]
      lat = linkParams["latency"]
      for src, dst, src_outport, dst_inport in connections:
        linkName = "network%d:%d->%d:%d" % (src,src_outport,dst,dst_inport)
        link = sst.Link(linkName)
        dstSwitch, dstParams = self.switches[dst]
        portName = "output %d %d" % (src_outport, dst_inport)
        srcSwitch.addLink(link, portName, lat)
        portName = "input %d %d" % (src_outport, dst_inport)
        dstSwitch.addLink(link, portName, lat)

  def connectEndpoints(self):
    lat = self.params["node"]["nic"]["injection"]["latency"]
    for i in range(self.num_nodes):
      injSwitch,connections = self.system.injectionConnections(i)
      dstSwitch, params = self.switches[injSwitch]
      node = self.nodes[i]
      for port in connections:
        linkName = "injection%d:%d->%d:%d" % (i,0,injSwitch,port)
        link = sst.Link(linkName)
        portName = "output %d %d" % (0, port) #0 is only outport on NIC
        node.addLink(link, portName, lat)
        portName = "input %d %d" % (0, port)
        dstSwitch.addLink(link, portName, "1ns") #no latency to return credits

      ejSwitch,connections = self.system.ejectionConnections(i)
      srcSwitch, params = self.switches[ejSwitch]
      for port in connections:
        linkName = "ejection%d:%d->%d:%d" % (injSwitch,port,i,0)
        link = sst.Link(linkName)
        portName = "input %d %d" % (port, 0)
        node.addLink(link, portName, "1ns") #no latency to return credits
        portName = "output %d %d" % (port, 0) #0 is only inport on NIC
        srcSwitch.addLink(link, portName, lat)
  
  def build(self):
    self.buildSwitches()
    self.buildEndpoints()
    self.connectSwitches()
    self.connectEndpoints()
    

def readCmdLineParams():
  import sstmac
  import sys
  return sstmac.readParams(sys.argv)

def setupDeprecated():
  import sys
  params = readCmdLineParams()

  nodeParams = params["node"]

  builtinApps = [
    "apitest",
    "global_test",
    "hello_world",
    "mpi_coverage",
    "mpi_ping_all",
    "mpi_print_nodes",
    "mpi_topology",
    "parsedumpi",
    "sstmac_mpi_testall",
    "traffic_matrix",
    "user_app_cxx_empty_main",
    "user_app_cxx_full_main",
  ]

  for i in range(10):
    ns = "app%d" % i
    if params.has_key(ns):
      appParams = params[ns]
      nodeParams[ns] = appParams
      appName = appParams["name"]
      if not appName in builtinApps:
        cmd = "import sst.%s" % appName
        exec(cmd)
      del params[ns]

  icParams = {}
  icParams["topology"] = params["topology"]
  nodeParams["interconnect"] = icParams
  nodeParams["nic"] = params["nic"]
  del params["nic"]

  #move every param in the global namespace 
  #into the individal namespaces
  for ns in "node", "switch":
    nsParams = params[ns]
    for key in params:
      val = params[key]
      if isinstance(val, str):
        if not nsParams.has_key(key):
          nsParams[key] = val

  ic = Interconnect(params)
  ic.build()


# Load module function in Python is changed
# to look for a libmacro.so in LD_LIBRARY_PATH
import sst.macro

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

def redoSubParams(theDict):
  allParams = []
  redoSubParams_impl([], theDict, allParams)
  newDict = {}
  for key, val in allParams:
    newDict[key] = val
  return newDict

def setupSimulation(node=None,switch=None,topology=None,debug=[]):
  sst.macro.init()

  if not node: raise Exception("Need Node params")
  if not switch: raise Exception("Need Switch params")
  if not topology: raise Exception("Need Topology params")

  hopLat = switch["hop_latency"]
  injLat = node["nic"]["injection_latency"]

  nodeParams = redoSubParams(node)
  swParams = redoSubParams(switch)
  topParams = redoSubParams(topology)

  top = sst.macro.Topology(topParams)
  if debug: sst.macro.debug(debug)

  for id in range(top.num_switches()):
    name = "switch%d" % id
    sw = sst.Component(name, "macro.packet_flow_switch")
    sw.addParams(swParams)
    sw.addParam("id", id)
    top.register_switch(sw)

  for id in range(top.num_nodes()):
    name = "node%d" % id
    node = sst.Component(name, "macro.simple_node")
    node.addParam("id", id)
    node.addParams(nodeParams)
    top.register_endpoint(node)
    
  top.connect_switches(hop_latency=hopLat)
  top.connect_endpoints(injection_latency=injLat)

def setupDeprecated():
  import sys
  sst.macro.init()
  params = sst.macro.readParams(sys.argv)

  nodeParams = params["node"]
  launchParams = [
    "name",
    "size",
    "launch_cmd",
    "start",
    "launch_allocation",
    "launch_indexing",
  ]

  for i in range(10):
    ns = "app%d" % i
    if params.has_key(ns):
      appParams = params[ns]
      nodeParams[ns] = appParams
      del params[ns]

  known_ns = "node","topology","nic","switch"
  for param in params:
    #anything remaining ... just treat as an application parameter for now
    val = params[param]
    if not param in known_ns:
      appParams[param] = val

  icParams = {}
  icParams["topology"] = params["topology"]
  nodeParams["interconnect"] = icParams
  nodeParams["nic"] = params["nic"]
  del params["nic"]

  setupSimulation(
    topology=params["topology"],
    node=params["node"],
    switch=params["switch"])


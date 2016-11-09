
import sys,getopt

import sst
from sst.merlin import *
from sst.macro import *

#debug("simple_network")

import emberLoadInfo
from emberLoadInfo import *

import random 

topoParams = {
 "name" : "hdtorus",
 "geometry" : "2 2 2",
}

injLat = "1us"
mtu="1KB"
bufSize = "64KB"
arb = "cut_through"

macroParams = {
"topology" : topoParams,
"injection_latency" : injLat,
"switch" : {
 "router" : "minimal",
 "arbitrator" : arb,
 "mtu" : mtu,
 "model" : "pisces",
 "buffer_size" : bufSize,
 "ejection" : {
   "latency" : injLat,
   "bandwidth" : "10GB/s",
 },
 "link" : {
   "send_latency" : "100ns",
   "credit_latency" : "1ns",
   "bandwidth" : "10GB/s",
 },
 "xbar" : {
   "send_latency" : "1ns",
   "credit_latency" : "100ns",
   "bandwidth" : "10GB/s",
  },
},
}

emberNicParams = {
  "mtu" : mtu,
  "bandwidth" : "10GB/s",
  "latency" : "1us",
  "credits" : bufSize,
  "arbitrator" : arb,
}

debug    = 0
emberVerbose = 10
embermotifLog = ''
emberrankmapper = ''

statNodeList = []
jobid = 0
loadFile = '' 
workList = []
workFlow = []
motifDefaults = { 
	'cmd' : "",
	'printStats' : 0, 
	'api': "HadesMP",
	'spyplotmode': 0 
}
motifs = [
  "Init",
  "AllPingPong iterations=10 messageSize=20000", 
  "Fini"
]
for entry in motifs:
  motif = dict.copy(motifDefaults)
  motif['cmd'] = entry
  workFlow.append(motif)

numCores = 1

netTopo = "torus"
netShape = "2x2x2"

platform = 'emberDefault'

netFlitSize = '' 
netBW = '' 
netPktSize = '' 
netTopo = ''
netShape = ''
netInspect = ''
rtrArb = ''

platParams = ""

if workFlow:
	workList.append( [jobid, workFlow] )
	jobid += 1

model = None

print "EMBER: platform: {0}".format( platform )

if not platParams:
	platParams = platform + 'Params'
try:
	config = __import__( platParams, fromlist=[''] )
except:
	sys.exit('Failed: could not import `{0}`'.format(platParams) )


nicParams = config.nicParams
nicParams["module"] = "macro.pisces"
#nicParams["module"] = "merlin.linkcontrol"
networkParams = config.networkParams
hermesParams = config.hermesParams
emberParams = config.emberParams 
platNetConfig = config.netConfig



emptyNids = []

XXX = []

nicParams['verboseLevel'] = debug
nicParams['verboseMask'] = 1
hermesParams['hermesParams.verboseLevel'] = debug
hermesParams['hermesParams.nicParams.verboseLevel'] = debug
hermesParams['hermesParams.functionSM.verboseLevel'] = debug
hermesParams['hermesParams.ctrlMsg.verboseLevel'] = debug
emberParams['verbose'] = emberVerbose
if embermotifLog:
    emberParams['motifLog'] = embermotifLog
if emberrankmapper:
    emberParams['rankmapper'] = emberrankmapper


epParams = {} 
epParams.update(emberParams)
epParams.update(hermesParams)

def buildFxn(ID):
  params = emberNicParams.copy()
  params["id"] = ID
  return loadInfo.setNode(ID).build(ID,params)[0]

ic = Interconnect(macroParams)
numNodes = ic.numNodes()
loadInfo = LoadInfo( nicParams, epParams, numNodes, numCores, ic.numNodes(), model )
loadInfo.initWork( workList, statNodeList )
ic.build(buildFxn)




import re
import os

import matplotlib.pyplot as plt
import numpy as np
import re
import sys

remap = {
"MPI_Waitsome" : "MPI_Wait",
"ComputeTime" : "Compute", 
"ComputeInstructions" : "Compute", 
"MPIEager1Protocol_Handle_RDMA_Header" : "MPI Compute",
"MPIEager1Protocol_Handle_RDMA_Payload" : "MPI Compute",
"MPIEager0Protocol_Handle_Header" : "MPI Compute",
"MPIRendezvousProtocol_RDMA_Configure_Buffer" : "MPI Compute",
"MPIEager1Protocol_Handle_RDMA_Header" : "MPI Compute",
"MPIEager1Protocol_Handle_RDMA_Payload" : "MPI Compute",
"MPIEager1Protocol_Handle_RDMA_Payload" : "MPI Compute",
"MPIEager0Protocol_Handle_Header" : "MPI Compute",
"MPIRendezvousProtocol_RDMA_Configure_Buffer" : "MPI Compute",
"MPIRendezvousProtocol_RDMA_Send_Header" : "MPI Compute",
"MPIRendezvousProtocol_RDMA_Handle_Header" : "MPI Compute",
"MPIRendezvousProtocol_RDMA_Handle_Payload" : "MPI Compute",
"MPIEager1Protocol_Send_RDMA_Header" : "MPI Compute",
"MPIEager0Protocol_Send_Header" : "MPI Compute",
"MPIRendezvousProtocol_RDMA_Send_Header" : "MPI Compute",
"MPIQueuePostRDMARequest" : "MPI Compute",
"MPIQueuePostHeader" : "MPI Compute",
}

class Rank:
  def __init__(self):
    self.bars = {}
    self.totalMPI = 0
    self.totalSync = 0
    self.comp = 0
    self.compMPI = 0

  def total(self):
    return float(self.totalMPI + self.comp)

  def mpiFraction(self, norm=None):
    if not norm: norm = float(self.totalMPI + self.comp)
    return float(self.totalMPI) / norm

  def commFraction(self, norm=None):
    if not norm: norm = float(self.totalMPI + self.comp)
    return float(self.totalMPI - self.totalSync) / norm

  def syncFraction(self, norm=None):
    if not norm: norm = float(self.totalMPI + self.comp)
    return float(self.totalSync) / norm

  def compFraction(self, norm=None):
    if not norm: norm = float(self.totalMPI + self.comp)
    return float(self.comp) / norm

  def __bool__(self):
    return bool(self.bars)

class Bar:
  def __init__(self, name, total):
    self.name = name
    self.total = total
    self.comp = 0
    self.comm = 0
    self.sync = 0

  def __repr__(self):
    return "%d:%d" % (self.total, self.sync)


def parse(fname):
  print "parsing", fname
  folder, ignore = os.path.split(fname)
  text = open(fname).read()

  rank = Rank()
  currentBar = None
  redundantCompute = 0
  started = False
  inMain = False
  for line in text.strip().splitlines():
    if "Call Graph Summary" in line:
      started = True
      continue

    if not started: 
      continue

    if "Estimated total" in line:
      started = False
      continue

    if line.startswith(" "):
      name, count = line.strip().split()
      name = name.strip()
      if name in remap: 
        name = remap[name]
      count = int(count)
      if inMain:
        if name == "Compute":
          rank.comp += count
        else:
          if "Finalize" in name:
            continue
          if "MPI_" in name:
            currentBar = Bar(name,count)
            rank.bars[name] = currentBar
            rank.totalMPI += count
      elif currentBar: #not in main, but I guess another MPI call
        if name == "MPI Compute":
          currentBar.comp += count
          rank.compMPI += count
        elif name == "Compute":
          currentBar.comp += count
          rank.compMPI += count
        elif name == "memcopy":
          currentBar.comp += count
          rank.compMPI += count
        elif name == "sync":
          currentBar.sync += count
    else:
      inMain = False
      entries = line.strip().split()
      fxn = " ".join(entries[:-2])
      if fxn in remap:
        fxn = remap[fxn]
      self = eval(entries[-1])
      total = eval(entries[-2])

      if "MPI_" in fxn:
        if rank.bars.has_key(fxn):
          currentBar = rank.bars[fxn]
      #elif fxn == "MPI Compute":
      #  rank.compMPI += total
      elif fxn == "sync":
        rank.totalSync += total
      elif fxn == "main":
        inMain = True
      else:
        currentBar = None
  rank.totalComm = rank.totalMPI - rank.totalSync - rank.compMPI

  cwd = os.getcwd()
  if folder:
    os.chdir(folder)

  if folder:
    os.chdir(cwd)

  return rank

def plotBars(data, title=None, output=None):
  main = data

  fig = plt.figure()
  ax = fig.add_subplot(111)

  colors = [
   "#afeeee", #pale turquoise
   '#f5deb3', #pale wheat
   "#cc99ff", #purple
   "green",
   'red',
   "#ffcc99", #orange
   'cyan',
   'yellow',
   'magenta',
  ]

  barWidth=0.25
  thk = 2

  fxns = main.bars.keys()
  fxns.sort()
  totalMPI = float(main.totalMPI)

  idx = 1
  colorIdx = 0

  mainTotal = float(main.totalMPI + main.comp)
  mainMPI = float(main.totalMPI)

  totalSync = main.totalSync / mainTotal
  totalComm = main.totalComm / mainTotal
  totalMPIComp = main.compMPI / mainTotal
  totalMPI = main.totalMPI / mainTotal
  totalComp = main.comp / mainTotal

  comms = [totalComm]
  syncs = [totalSync]
  comps = [totalMPIComp]

  xlabels = ["Total"]

  #just take the 5 top functions
  maxFxns = min(5,len(fxns))

  sorter = []
  for f in fxns:
    b = main.bars[f]
    sorter.append((b.total,f))

  sorter.sort()
  sorter.reverse()

  for ignore, f in sorter[:maxFxns]:
    b = main.bars[f]
    comm = (b.total - b.sync - b.comp) / mainMPI
    sync = b.sync / mainMPI
    comp = b.comp / mainMPI

    comms.append(comm)
    syncs.append(sync)
    comps.append(comp)

    xlabels.append(f.replace("MPI_",""))

  xs = range(2,maxFxns+2)
  xs.insert(0, 0.6)

  comms = np.array(comms)
  syncs = np.array(syncs)
  comps = np.array(comps)

  commBar = ax.bar(xs, comms, barWidth, color=colors[0], ecolor='block')
  syncBar = ax.bar(xs, syncs, barWidth, color=colors[1], ecolor='block', hatch='///', bottom=comms)
  mpiStackBar = ax.bar(xs, comps, barWidth,color=colors[2], ecolor='block', bottom=comms+syncs)
  ax.set_ylabel("Fraction Total Time")

  #now add the bars for actual computation, not MPI stack
  ax2 = ax.twinx()
  myx=[0.6]
  myy=[totalComp]
  bottoms=[totalMPI]
  compBar = ax2.bar(myx,myy,barWidth,color=colors[3], ecolor='block', bottom=bottoms)
  ax2.set_ylabel("Fraction MPI Time")
  ax2.tick_params('y', length=0, labelright='off')
  ax2.set_ylim([0,1])


  plt.xticks(xs, xlabels)
  plt.axvline(1.5, color='black', lw=5, ls='dashed')
  ax.legend([commBar, syncBar, mpiStackBar, compBar], ["Network", "Sync", "MPI Stack", "Compute"])

  #locs, labels = plt.xticks()
  #plt.setp(labels, rotation=45)

  ax.set_ylim([0,1])
  plt.xlim([0,maxFxns+2])

  if title:
    ax.set_title(title)

  if output:
    plt.savefig(output)
  else:
    plt.show()


import re
import os

remap = {
"MPI_Waitsome" : "MPI_Wait",
"ComputeTime" : "Compute", 
"ComputeInstructions" : "Compute", 
"MPIRendezvousProtocol_RDMA_Configure_Buffer" : "MPI Compute",
"MPIEager1Protocol_Handle_RDMA_Header" : "MPI Compute",
"MPIEager1Protocol_Handle_RDMA_Payload" : "MPI Compute",
"MPIEager1Protocol_Handle_RDMA_Payload" : "MPI Compute",
"MPIEager0Protocol_Handle_Header" : "MPI Compute",
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
  def __init__(self, total):
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
            currentBar = Bar(count)
            rank.bars[name] = currentBar
            rank.totalMPI += count
      elif currentBar: #not in main, but I guess another MPI call
        if name == "MPI Compute":
          currentBar.comp += count
        elif name == "memcopy":
          currentBar.comp += count
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
      elif fxn == "MPI Compute":
        rank.compMPI += total
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

  print rank.compMPI
  print rank.totalMPI

  return rank




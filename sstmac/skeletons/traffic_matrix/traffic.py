
def getVals(model):
  fileName = "%s.out" % model
  import re
  text = open(fileName).read()
  regexp = re.compile("throughput=\s+(\d+[.]\d+)")
  matches = regexp.findall(text)
  return map(float, matches)

def absError(lvals, rvals):
  length = len(lvals)
  err = 0
  for i in range(length):
    r = lvals[i]
    l = rvals[i]
    err += abs(l-r)
  return err / length

def error(lvals, rvals):
  length = len(lvals)
  err = 0
  for i in range(length):
    r = lvals[i]
    l = rvals[i]
    err += l-r
  return err / length

def avgTput(vals):
  summer = 0
  for v in vals:
    summer += v
  return summer / len(vals)

def scatter(vals):
  delta = 0
  mean = avgTput(vals)
  for v in vals:
    delta += abs(mean-v) 
  return delta / len(vals)

def run(model, **kwargs):
  import os
  args = [
    '-p network_bandwidth=2.5GB/s',
  ]
  for argName in kwargs:
    argVal = kwargs[argName]
    args.append("-p %s=%s" % (argName, argVal))

  if model == "amm1":
    args.append('-p congestion_model=simple')
  else:
    args.append('-p congestion_model=pisces')
  args.append('-p amm_model=%s' % model)
  args = " ".join(args)
  cmd  = "./runtraffic %s >& %s.out" % (args, model)
  os.system(cmd)


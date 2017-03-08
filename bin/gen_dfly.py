#!/usr/bin/python

gsize = 96

node_ncol = 6
port_ncol = 6

def node_row( n ):
  return n / node_ncol

def node_col( n ):
  return n % node_ncol

def node_num(r, c):
  return r * node_ncol + c

def port_row( n ):
  return n / port_ncol

def port_col( n ):
  return n % port_ncol

def port_num(r, c):
  return r * port_ncol + c

intrafile = open('intragroup.txt','w')

for src in range(0,gsize):

  src_row = node_row(src)
  src_col = node_col(src)

  
  #connect in row
  row_red = 3
  dim_red = row_red
  dst_row = src_row
  for dst_col in range(0,6):
    if dst_col == src_col:
      continue
    outport = dst_col
    if outport >= src_col: 
      outport -= 1
    inport = src_col
    if inport >= dst_col:
      inport -= 1
    outport *= dim_red
    inport *= dim_red
    for red in range(0,dim_red):
      intrafile.write( "%s %s : %s %s -> %s %s : %s %s\n" % (
           src_row, src_col,
           port_row(outport), port_col(outport),
           dst_row, dst_col,
           port_row(inport), port_col(inport)) )
      outport += 1
      inport += 1

  #connect in col
  col_red = 1
  dim_red = col_red
  src_row = node_row(src)
  src_col = node_col(src)
  dst_col = src_col
  for dst_row in range(0,16):
    if dst_row == src_row:
      continue
    outport = dst_row 
    if outport >= src_row:
      outport -= 1
    inport = src_row
    if inport >= dst_row:
      inport -= 1
    outport *= dim_red
    inport *= dim_red
    #print "unshifted outport: %s" % outport
    outport += (node_ncol - 1) * row_red
    inport += (node_ncol - 1) * row_red
    #print "shifted outport: %s" % outport
    for red in range(0,dim_red):
      intrafile.write( "%s %s : %s %s -> %s %s : %s %s\n" % (
           src_row, src_col,
           port_row(outport), port_col(outport),
           dst_row, dst_col,
           port_row(inport), port_col(inport)) )
      outport += 1
      inport += 1

intrafile.close()

interfile = open('intergroup.txt','w')

ngroup = 15
ngcon = 5

# number of group partitions
ngp = ngcon 

# nuber of groups in a partition
psize = ngroup / ngp
interfile.write("psize: %s\n" % psize)

gsplit_size = gsize / (psize-1)
interfile.write("gsplit_size: %s\n" % gsplit_size)

# loop over groups
for myg in range(0,ngroup):

  gid = myg % gsize 

  pnum = myg / psize

  # myg's id within the its partition
  pid = myg % psize

  # loop over switches in the group
  for src in range(0,96):

    # determine which neighborhood to connect to
    nid = gid / gsplit_size
    noff = nid
    if noff >= pid:
      noff += 1

    #interfile.write("myg: %s, pid: %s, gid: %s\n" % (myg, pid, gid) )

    # determine which partition to connect to
    for target_p in range(0,ngcon):
 
      #interfile.write("noff: %s, target_p: %s\n" % (noff, target_p))

      destg = target_p * psize + noff
      outport = 30 + target_p
      inport = 30 + pnum
        #if gpdir >= pid:
        #  outport -= 1
        #inport = 30 + pid
        #if inport >= gpdir:
        #  inport -= 1
        #if myg != destg:
      interfile.write("%s %s %s : %s %s -> %s %s %s : %s %s\n" % (
               node_row(src), node_col(src), myg,
               port_row(outport), port_col(outport),
               node_row(src), node_col(src), destg,
               port_row(inport), port_col(inport) ) )
     

      # increment partition direction
      #gpdir += 1

#!/usr/bin/python

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

for src in range(0,96):

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
gpsize = ngroup / ngp
# if remainder present, some partitions will be bigger by one
if (ngroup % ngp) > 0:
  gpsize += 1

# loop over groups
for myg in range(0,ngroup):

  # myg's id within the its partition
  gpid = myg % gpsize

  # loop over group directions
  for gdir in range(0,ngcon):

    gpdir = 0
    # loop over switches in the group
    for src in range(0,96):
 
      # reset partition direction
      if gpdir == gpsize:
        gpdir = 0

      # no self connection
      if gpid != gpdir:
        destg = gdir * gpsize + gpdir    
        outport = 30 + gdir
        if gpdir >= gpid:
          outport -= 1
        inport = 30 + gpid
        if inport >= gpdir:
          inport -= 1
        if myg != destg and gpid != gpdir:
          interfile.write("%s %s %s : %s %s -> %s %s %s : %s %s\n" % (
               node_row(src), node_col(src), myg,
               port_row(outport), port_col(outport),
               node_row(src), node_col(src), destg,
               port_row(inport), port_col(inport) ) )
     

      # increment partition direction
      gpdir += 1

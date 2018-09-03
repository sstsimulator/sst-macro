import sys
import os
import numpy as np

import matplotlib 

from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.mplot3d.art3d import Poly3DCollection, Line3DCollection
import matplotlib.pyplot as plt



def genFaces(Z):
  """should be numbered like so
  Z[0] = corner[:]
  Z[1] = corner + yDelta
  Z[2] = corner + zDelta
  Z[3] = Z[2] + yDelta
  Z[4] = corner + xDelta
  Z[5] = Z[4] + yDelta
  Z[6] = Z[4] + zDelta
  Z[7] = Z[6] + yDelta
  """

  # list of sides' polygons of figure
  faces = [[Z[0],Z[1],Z[3],Z[2]],
    [Z[0],Z[1],Z[5],Z[4]], 
    [Z[0],Z[2],Z[6],Z[4]], 
    [Z[4],Z[5],Z[7],Z[6]], 
    [Z[2],Z[6],Z[7],Z[3]],
    [Z[1],Z[5],Z[7],Z[3]]]

  return faces


def addPoly(ax, verts, color=None, alpha=0.5, rgb=None):
  faces = genFaces(verts)
  # plot sides
  poly = Poly3DCollection(faces, facecolors=color, linewidths=0.01, edgecolors='black')
  alpha_tuple = []
  if not rgb and not color: color = "cyan" #okay to default for now
  if color:
    color_tuple = matplotlib.colors.to_rgb(color)
    for entry in color_tuple: #copy to allow assignment
      alpha_tuple.append(entry)
  elif rgb:
    for entry in rgb:
      alpha_tuple.append(entry)
  else:
    sys.exit("Must give either color name or RGB tuple")
  alpha_tuple.append(alpha)
  poly.set_facecolor(alpha_tuple)
  ax.add_collection3d(poly)


if len(sys.argv) != 2:
  sys.exit("./plot <sst-macro.xyz file>")

path = sys.argv[1]
if not os.path.isfile(path):
  sys.exit("%s is not a valid input file" % path)

text = open(path).read()

def get_xyz(tuple):
  return np.array(map(float, tuple.split(",")))

mins = [ 100000, 100000, 100000 ]
maxs = [-100000,-100000,-100000 ]

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
for line in text.strip().splitlines():
  splitLine = line.split(";")
  geometry = splitLine[0]
  attributes = splitLine[1:]
  geomSplit = geometry.split("->")
  if len(geomSplit) != 8:
    sys.exit("line is not a valid geometry: %s" % line)

  color = None 
  rgb = None
  alpha = 0.5
  for attr in attributes:
    split = attr.split("=")
    if len(split) == 2:
      name, value = map(lambda x: x.strip().lower(), attr.split("="))
      if name == "color":
        color = value
      elif name == "alpha":
        alpha = float(value)
      elif name == "rgb":
        rgb = map(float, value.split(","))
    else:
      sys.stderr.write("Bad key,value pair: %s\n" % attr)

  verts = map(get_xyz, geomSplit)
  for v in verts:
    for i in range(3):
      mins[i] = min(mins[i], v[i])
      maxs[i] = max(maxs[i], v[i]) 

  addPoly(ax, verts, color=color, alpha=alpha, rgb=rgb)



ax.set_xlim(mins[0], maxs[0])
ax.set_ylim(mins[1], maxs[1])
ax.set_zlim(mins[2], maxs[2])


#ax.set_xlabel('X')
#ax.set_ylabel('Y')
#ax.set_zlabel('Z')
#ax.set_xlim(0,nboxes)
#ax.set_ylim(0,nboxes)
#ax.set_zlim(0,nboxes)

plt.show()

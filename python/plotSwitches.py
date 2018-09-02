import sys
import os
import numpy as np
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


def addPoly(ax, verts):
  faces = genFaces(verts)
  # plot sides
  ax.add_collection3d(Poly3DCollection(faces, 
    facecolors='cyan', linewidths=0.5, edgecolors='b', alpha=.95))


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
  splitter = line.split("--")
  if len(splitter) != 8:
    sys.exit("line is not a valid geometry: %s" % line)

  verts = map(get_xyz, splitter)
  for v in verts:
    for i in range(3):
      mins[i] = min(mins[i], v[i])
      maxs[i] = max(maxs[i], v[i]) 

  addPoly(ax, verts)



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

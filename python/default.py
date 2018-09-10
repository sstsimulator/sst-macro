from sst.macro import *
import sys
import os
import sst

ic = setupDeprecated()
print dir(ic.switches[0])
print ic.num_switches

# Set the Statistic Load Level; Statistics with Enable Levels (set in
# elementInfoStatistic) lower or equal to the load can be enabled (default = 0)
sst.setStatisticLoadLevel(7)

trafficStats = sst.StatisticGroup("traffic_intensity_stats")
trafficStats.addStatistic("traffic_intensity", {"resetOnRead": False})
#sst.StatisticOutput("sst.statOutputEXODUS", {"filepath" : "./trafExodus.out"})
trafficStats.setOutput(sst.StatisticOutput("macro.statOutputEXODUS", {"filepath": "/Users/perrinel/Dev/trafEXODUS.e", "count_x":"4", "count_y":"4"}))

for i in range(ic.num_switches):
  s, params = ic.switches[i]
  trafficStats.addComponent(s)
  s.enableStatistics(["traffic_intensity"], {"type":"macro.traffic_intensity","rate":"0ns"})

#for n in self.nodes:
#  s.enableStatistics([
#    "traffic_intensity"], {
#    "type":"sst.TrafficStatistic",
#      "rate":"0ns"
#      })


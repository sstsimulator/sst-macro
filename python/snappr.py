# Load module function in Python is changed
# to look for a libmacro.so in LD_LIBRARY_PATH
import sst
import sst.macro

sst.setProgramOption("timebase", "100as")

mtu = "4KB"
small_latency = "1ps"
nic_latency = "50ns"
nic_bandwidth = "1.0GB/s"
link_latency = "100ns"
link_bandwidth = "1.0GB/s"

topo_params = dict(
  name = "dragonfly",
  geometry = "[4,3]",
  h = "6",
  inter_group = "circulant",
  concentration = "4"
)

router_name = "dragonfly_minimal"

system = sst.macro.System(topo_params)
num_switches = system.numSwitches()
num_nodes = system.numNodes()

switches = [None]*num_switches
nodes = [None]*num_nodes

for i in range(num_switches):
  switch = sst.Component("Switch %d" % i, "macro.snappr_switch")
  switch.addParams({
    "router.name" : router_name,
    "credits" : "8KB",
    "id" : i,
  })
  switches[i] = switch

for i in range(num_nodes):
  node = sst.Component("Node %d" % i, "macro.simple_node")
  node.addParams({
    "proc.frequency" : "2GHz",
    "app1.name" : "mpi_ping_all",
    "app1.launch_cmd" : "aprun -n 4 -N 1",
    "id" : i,
  })
  nodes[i] = node

  #build the NIC
  nic = node.setSubComponent("nic", "macro.snappr_nic")
  nic.addParams(dict(
    mtu=mtu,
    bandwidth=nic_bandwidth,
    latency=nic_latency,
    credits="8KB",
  ))

  inj_port = nic.setSubComponent("outport", "macro.snappr_outport")
  inj_port.addParams(dict(
    bandwidth=nic_bandwidth,
    latency=link_latency,
    credits="8KB",
    mtu=mtu
  ))


  #build the memory system
  mem = node.setSubComponent("memory", "macro.snappr_memory")
  mem.addParams(dict(
    channel_bandwidth="1.0GB/s",
    num_channels=8
  ))

for i in range(num_switches):
  connections = system.switchConnections(i)
  switch = switches[i]
  for src_id, dst_id, src_outport, dst_inport in connections:
      port = switch.setSubComponent("outport%d" % src_outport, "macro.snappr_outport")
      port.addParams(dict(
        bandwidth=link_bandwidth,
        latency=link_latency,
        mtu=mtu
      ))

      link_name = "network%d:%d->%d:%d" % (src_id,src_outport,dst_id,dst_inport)
      link = sst.Link(link_name)
      port_name = "output%d" % (src_outport)
      switch.addLink(link,port_name,link_latency)
      print("Connect %s on switch %d" % (port_name,i))

      dst_switch = switches[dst_id]
      port_name = "input%d" % (dst_inport)
      dst_switch.addLink(link,port_name,link_latency)

for sw_id in range(num_switches):
  connections = system.ejectionConnections(sw_id)
  for ep_id, switch_port, ej_port in connections:
    ep = nodes[ep_id]
    switch = switches[sw_id]

    link_name = "ejection%d:%d->%d:%d" % (ep_id,ej_port,sw_id,switch_port)
    link = sst.Link(link_name)

    port_name = "output%d" % (switch_port)
    switch.addLink(link,port_name,link_latency)

    port_name = "input%d" % (ej_port)
    ep.addLink(link,port_name,link_latency)

  connections = system.ejectionConnections(sw_id)
  for ep_id, switch_port, inj_port, in connections:
    ep = nodes[ep_id]
    switch = switches[sw_id]

    link_name = "injection%d:%d->%d:%d" % (ep_id,inj_port,sw_id,switch_port)
    link = sst.Link(link_name)

    port_name = "input%d" % (switch_port)
    switch.addLink(link,port_name,link_latency)

    port_name = "output%d" % (ej_port)
    ep.addLink(link,port_name,link_latency)

nproc = sst.getMPIRankCount() * sst.getThreadCount()
logp_switches = [None]*nproc
for i in range(nproc):
  switch = sst.Component("LogP %d" % i, "macro.logp_switch")
  switch.addParams(dict(
    bandwidth=link_bandwidth,
    hop_latency="100ns",
    out_in_latency="2us"
  ))
  logp_switches[i] = switch

for i in range(num_nodes):
  ep = nodes[i]
  for p in range(nproc):
    sw = logp_switches[p]
    inj_sw = system.nodeToLogPSwitch(i)

    #use inj_sw to set a no-cut

    link_name = "logPinjection%d->%d" % (i, p)
    link = sst.Link(link_name)
    port_name = "output%d" % (sst.macro.NICLogPInjectionPort)
    ep.addLink(link, port_name, small_latency) #put no latency here
    port_name = "input%d" % (i)
    sw.addLink(link, port_name, small_latency)

    link_name = "logPejection%d->%d" % (p, i)
    link = sst.Link(link_name)
    port_name = "input%d" % (sst.macro.NICLogPInjectionPort)
    ep.addLink(link, port_name, small_latency) #put no latency here
    port_name = "output%d" % (i)
    sw.addLink(link, port_name, small_latency)


sst.macro.debug("mpi")

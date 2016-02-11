/*
 *  This file is part of SST/macroscale:
 *               The macroscale architecture simulator from the SST suite.
 *  Copyright (c) 2009 Sandia Corporation.
 *  This software is distributed under the BSD License.
 *  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
 *  the U.S. Government retains certain rights in this software.
 *  For more information, see the LICENSE file in the top
 *  SST/macroscale directory.
 */
// clos.cc: Implementation of CLOS networks.
//
// Based on clos.c by Jim Schutt
// Adapted by Curtis Janssen <cljanss@ca.sandia.gov>

#include <sstream>
#include <sstmac/hardware/topology/clos.h>
#include <sstmac/hardware/topology/closrouter.h>
#include <sstmac/common/driver_util.h>
#include <sstmac/common/factories/topology_factory.h>

namespace sstmac {
namespace hw {

SpktRegister("clos", topology, clos);

clos*
clos::construct(sprockit::sim_parameters* params)
{
  std::vector<int> args;
  params->get_vector_param("geometry");
  int nps = params->get_optional_int_param("concentration", 1);
  return clos*(new clos(args[0], args[1], args[2], nps));
}

clos*
clos::construct(int n)
{
  return clos*(new clos(n));
}

static void
compute_nodes(int n, std::set<vertex>& vertices)
{
  vertices.clear();

  vertex v(3);
  for (int i = 0; i < 2 * n; i++) {
    v[0] = i;
    for (int j = 0; j < n; j++) {
      v[1] = j;
      for (int k = 0; k < n; k++) {
        v[2] = k;
        vertices.insert(v);
      }
    }

  }
}

static void
compute_switches(int n, std::set<vertex> &vertices)
{
  vertices.clear();

  vertex v(4);

  // the lower tier
  v[0] = 1;
  v[3] = 0;
  for (int i = 0; i < n * 2; i++) {
    v[1] = i;
    for (int j = 0; j < n; j++) {
      v[2] = j;
      vertices.insert(v);
    }
  }

  // the middle tier
  v[0] = 2;
  v[3] = 0;
  for (int i = 0; i < n * 2; i++) {
    v[1] = i;
    for (int j = 0; j < n; j++) {
      v[2] = j;
      vertices.insert(v);
    }
  }

  // the upper tier
  v[0] = 3;
  v[3] = 0;
  for (int i = 0; i < n; i++) {
    v[1] = i;
    for (int j = 0; j < n; j++) {
      v[2] = j;
      vertices.insert(v);
    }
  }
}

static void
compute_edges(int n, std::set<edge> &edges)
{
  edges.clear();

  // Compute the edges between the nodes and tier 1.
  vertex node(3);
  vertex sw1(4);
  sw1[0] = 1;
  sw1[3] = 0;
  for (int i = 0; i < 2 * n; i++) {
    node[0] = i;
    sw1[1] = i;
    for (int j = 0; j < n; j++) {
      node[1] = j;
      sw1[2] = j;
      for (int k = 0; k < n; k++) {
        node[2] = k;
        edges.insert(edge(node, sw1));
        edges.insert(edge(sw1, node));
      }
    }
  }

  // compute the edges between tier 1 and tier 2.
  vertex sw2(4);
  sw2[0] = 2;
  sw2[3] = 0;
  for (int i = 0; i < 2 * n; i++) {
    sw1[1] = i;
    sw2[1] = i;
    for (int j = 0; j < n; j++) {
      sw1[2] = j;
      for (int k = 0; k < n; k++) {
        sw2[2] = k;
        edges.insert(edge(sw1, sw2));
        edges.insert(edge(sw2, sw1));
      }
    }
  }

  // compute the edges between tier 2 and tier 3.
  vertex sw3(4);
  sw3[0] = 3;
  sw3[3] = 0;
  for (int i = 0; i < 2 * n; i++) {
    sw2[1] = i;
    for (int j = 0; j < n; j++) {
      sw2[2] = j;
      sw3[1] = j;
      for (int k = 0; k < n; k++) {
        sw3[2] = k;
        edges.insert(edge(sw2, sw3));
        edges.insert(edge(sw3, sw2));
      }
    }
  }
}

void
clos::init_clos(int n)
{
  n_ = n;
  if (n < 2) {
    throw std::runsprockit::time_error("clos: must have n > 1");
  }

  compute_nodes(n, nodes_);

  std::set<vertex> switches;
  compute_switches(n, switches);

  std::set<edge> edges;
  compute_edges(n, edges);

  std::set<vertex> all_vertices;
  all_vertices.insert(nodes_.begin(), nodes_.end());
  all_vertices.insert(switches.begin(), switches.end());
  init(all_vertices);
}

// static sstmac::xmlclass::register_class reg("sstmac::clos",
//                               sstmac::xmlclass::ctor_template<clos>);

// clos::clos(TiXmlHandle h)
// {
//   int n = 2;
//   h.ToElement()->QueryIntAttribute("n",&n);
//   init_clos(n);
// }

clos::clos(int n, int k, int m, int nps) :
  structured_topology(nps),
  n_(n), k_(k), m_(m)
{
  nonswitch_ = false;
  init_clos(n);
}

clos::clos(int n) :
  topology(1),
  n_(n), k_(-1), m_(-1)
{
  nonswitch_ = true;
  init_clos(n);
}

clos::~clos()
{
}

size_t
clos::size(int n)
{
  return 2 * n * n * n;
}

double
clos::a() const
{
  return 6.0;
}

int64_t
clos::b() const
{
  return n() / 2;
}

void
clos::shortest_path(const vertex &v1, const vertex &v2, walk &w) const
{
  throw std::runsprockit::time_error("clos::shortest_path: not unique");
}

// This hashes on the least significant digits in the target address
// to better distribute load.
void
clos::route_hashed(const vertex &v1, const vertex &v2, walk &w) const
{
  if (v1 == v2) {
    walk empty;
    w = empty;
    return;
  }

  std::vector<lwedge> edges;

  vertex new_sw(v1);
  new_sw.v().insert(new_sw.v().begin(), 1);
  new_sw[3] = 0;

  edges.push_back(g().generate_lwedge(v1, new_sw));
  vertex old_sw(new_sw);

  // see if we've already gotten to a parent of the target
  if (new_sw[2] != v2[1] || new_sw[1] != v2[0]) {

    // Assign digit x: 2.x0 = ..x
    // This hashes on the least significant address.
    new_sw[0] = 2;
    new_sw[2] = v2[2];
    edges.push_back(g().generate_lwedge(old_sw, new_sw));
    old_sw = new_sw;

    // see if we've already gotten to a parent of the target
    if (new_sw[1] != v2[0]) {

      // Assign digit x: 3x.0 = ..x
      // This is required for the route to exist.
      new_sw[0] = 3;
      new_sw[1] = v2[2];
      // Assign digit y: 3.y0 = .y.
      // This hashes on the next to least significant address.
      new_sw[2] = v2[1];
      edges.push_back(g().generate_lwedge(old_sw, new_sw));
      old_sw = new_sw;

      // now begin working back down the tree
      // Assign digit x: 2x.0 = x..
      // Assign digit x: 2.y0 = ..y
      new_sw[0] = 2;
      new_sw[1] = v2[0];
      new_sw[2] = v2[2];
      edges.push_back(g().generate_lwedge(old_sw, new_sw));
      old_sw = new_sw;
    }

    // Assign digit x: 1.x0 = .x.
    new_sw[0] = 1;
    new_sw[2] = v2[1];
    edges.push_back(g().generate_lwedge(old_sw, new_sw));
    old_sw = new_sw;
  }

  edges.push_back(g().generate_lwedge(old_sw, v2));

  w.init(edges);
}

void
clos::route(const vertex &v1, const vertex &v2, walk &w) const
{
  route_hashed(v1, v2, w);
}

int64_t
clos::n() const
{
  if (nonswitch_) {
    return n_;
  }
  else {
    return n_ * m_;
  }
}

int64_t
clos::diameter() const
{
  return 6;
}

bool
clos::vertex_symmetric() const
{
  return true;
}

std::string
clos::name() const
{
  std::ostringstream ostr;
  ostr << "CLOS(" << n_ << ")";
  return ostr.str();
}

node_id
clos::node_to_ejector_addr(node_id addr, int& switch_port)
{
  long idx = addr;
  long ej_idx = (idx / n_) + m_ + k_;
  switch_port = idx % nps_;
  return nodeaddress::convert_from_uniqueid(ej_idx);
}


}
} //end of namespace sstmac


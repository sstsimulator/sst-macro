#include "boxml.h"

namespace lblxml {

#ifdef BOXML_HAVE_METIS

bool compare(const pair<int, idx_t>&i, const pair<int, idx_t>&j)
{
  return i.second > j.second;
}

void
boxml::partition_boxes() {

  //create map of edges and weights
  map< pair<int,int>, int > edge_weights;
  for (int i=0; i < g_events.size(); ++i) {
    event_t *ev = g_events[i];
    if (ev != NULL && ev->event_type() == event::pt2pt) {
      comm_t* comm = static_cast<comm_t*>(ev);
      int size = comm->size();
      pair<int,int> edge(comm->from(),comm->to());
      if (edge_weights.find(edge) != edge_weights.end())
        edge_weights[edge] += size;
      else
        edge_weights[edge] = size;
    }
  }

  //convert edge weight map into metis adjacency description
  list<int> std_xadj, std_adj;
  int ia=0;
  for (int i=0; i < g_boxes.size(); ++i){
    std_xadj.push_back(ia);
    for (int j=0; j < g_boxes.size(); ++j) {
      pair<int,int> edge(i,j);
      if (edge_weights.find(edge) != edge_weights.end()) {
        std_adj.push_back(j);
        ++ia;
      }
    }
  }
  std_xadj.push_back(ia);

  idx_t nvtxs;
  nvtxs = g_boxes.size();

  idx_t ncon;
  ncon = 1;

  idx_t* xadj = new idx_t[std_xadj.size()];
  int i=0;
  for(list<int>::iterator it = std_xadj.begin();
      it != std_xadj.end(); ++it) {
    xadj[i] = idx_t(*it);
    ++i;
  }

  idx_t* adjncy = new idx_t[std_adj.size()];
  i=0;
  for(list<int>::iterator it = std_adj.begin();
      it != std_adj.end(); ++it) {
    adjncy[i] = *it;
    ++i;
  }

  idx_t* vwgt = new idx_t[g_boxes.size()];
  idx_t* vsize = new idx_t[g_boxes.size()];
  int nheavy = 0;
  for (i=0; i < g_boxes.size(); ++i) {
    vsize[i] = 1;
    if (fixed_vertex_ > 0) {
      vwgt[i] = fixed_vertex_;
    }
    else {
      vwgt[i] = 0;
      for (int j=0; j < g_events.size(); ++j) {
        event_t *ev = g_events[j];
        if (ev != NULL && ev->event_type() == event::computation) {
          comp_t* comp = static_cast<comp_t*>(ev);
          if ( comp->at() == i ) {
            vwgt[i] += int(comp->time() / 1e-9);
          }
        }
      }
      vwgt[i] *= vertex_scale_;
      if (vwgt[i] > 0) ++nheavy;
    }
  }
  //std::cout << nheavy << " boxes with non-zero compute\n";

  idx_t* adjwgt = new idx_t[std_adj.size()];
  ia=0;
  for (int x1=0; x1 < nvtxs; ++x1){
    for (int l=xadj[x1]; l < xadj[x1+1]; ++l) {
      int x2 = adjncy[l];
      adjwgt[ia] = 0;
      pair<int,int> edge(x1,x2);
      if (edge_weights.find(edge) != edge_weights.end())
        adjwgt[ia] += edge_weights[edge];
      edge = pair<int,int>(x2,x1);
      if (edge_weights.find(edge) != edge_weights.end())
        adjwgt[ia] += edge_weights[edge];
      if (zero_edge_weight_)
        adjwgt[ia] = 1;
      ++ia;
    }
  }

  idx_t nparts;
  if (repartition_size_ < 1)
    spkt_throw_printf(sprockit::value_error,
                      "need positive boxml_repartition_size for repartitioning\n");
  nparts = repartition_size_;

  real_t ubvec[] = {load_balance_tolerance_};

  idx_t objval;
  idx_t part[nvtxs];

  idx_t options[METIS_NOPTIONS];
  METIS_SetDefaultOptions(options);
  options[METIS_OPTION_DBGLVL] = METIS_DBG_INFO;
  options[METIS_OPTION_NUMBERING] = 0;
  //options[METIS_OPTION_UFACTOR] = 50;

  if (partitioning_ == "knapsack") {
    std::vector< pair<int,idx_t> > sorted;
    sorted.resize(nvtxs);
    for (int i=0; i < nvtxs; ++i) {
      sorted[i].first = i;
      sorted[i].second = vwgt[i];
    }
    sort(sorted.begin(),sorted.end(),compare);
    int new_loc = 0;
    for (int i=0; i < nvtxs; ++i) {
      part[ sorted[i].first ] = new_loc;
      ++new_loc;
      if (new_loc == nparts) new_loc = 0;
    }
  }
  else
    METIS_PartGraphKway(&nvtxs, &ncon, xadj, adjncy,
                        vwgt, vsize, adjwgt, &nparts, NULL,
                        ubvec, options, &objval, part);

  std::cout << "partition vector:\n";
  std::set<int> placed_ranks;
  for (i=0; i < nvtxs; ++i) {
    std::cout << part[i] << " ";
    g_boxes[i]->change_loc(part[i]);
    placed_ranks.insert(part[i]);
  }
  std::cout << "\n";
  std::cout << "number of partitions: " << placed_ranks.size() << "\n";

  delete[] xadj;
  delete[] adjncy;
  delete[] vwgt;
  delete[] vsize;
  delete[] adjwgt;
}
#endif

} // end namespace lblxml

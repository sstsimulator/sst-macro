#ifndef sumi_api_DENSE_RANK_MAP_H
#define sumi_api_DENSE_RANK_MAP_H

#include <set>
#include <sumi/communicator_fwd.h>
#include <sumi/thread_safe_set.h>

namespace sumi {

/**
* @class dense_rank_map
* physical <= dense <= virtual
* sparse rank is a synonym for physical.
* It's the rank you started with when there were no failures
* A job starts with 3 nodes {0,1,2}.
* Node 1 dies. There are 2 live nodes.
* 0 -> 0
* 2 -> 1
*/
class dense_rank_map {

 public:
  int dense_rank(int sparse_rank) const;

  int sparse_rank(int dense_rank) const;

  dense_rank_map();

  dense_rank_map(const thread_safe_set<int>& failed, communicator* dom = 0);

  ~dense_rank_map();

  void
  init(const thread_safe_set<int>& failed, communicator* dom = 0);

 protected:
  static const int tree_cutoff = 4;

  /**
   * O(N) search algorithm for new rank for N = # failures
   * @param sparse_rank
   * @return
   */
  int 
  linear_find_rank(int sparse_rank) const;

  /**
   * O(log N) search algorithm for new rank,
   * but with larger prefactor for N = # failures
   * @param sparse_rank
   * @return
   */
  int 
  tree_find_rank(
    int sparse_rank,
    int offset,
    int num_failed_ranks,
    int* failed_array) const;

 protected:
  int num_failed_ranks_;
  int* failed_ranks_;

};

}

#endif // DENSE_RANK_MAP_H

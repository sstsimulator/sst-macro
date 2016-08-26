#include <sumi/dense_rank_map.h>
#include <sumi/communicator.h>
#include <sprockit/errors.h>
#include <algorithm>

namespace sumi {

dense_rank_map::dense_rank_map(const thread_safe_set<int>& failed,
  communicator* dom) :
  num_failed_ranks_(failed.size()),
  failed_ranks_(0)
{
  init(failed, dom);
}

dense_rank_map::dense_rank_map() :
  num_failed_ranks_(0),
  failed_ranks_(0)
{
}

dense_rank_map::~dense_rank_map()
{
  if (failed_ranks_){
    delete[] failed_ranks_;
  }
}

void
dense_rank_map::init(const thread_safe_set<int>& failed, communicator* dom)
{
  if (failed_ranks_){ //clear out old data
    delete[] failed_ranks_;
  }

  num_failed_ranks_ = failed.size();

  if (num_failed_ranks_ == 0)
    return;

  failed_ranks_ = new int[num_failed_ranks_];

  thread_safe_set<int>::const_iterator it, end = failed.start_iteration();
  int idx = 0;
  for (it=failed.begin(); it != end; ++it){
    int comm_rank = dom ? dom->global_to_comm_rank(*it) : *it;
    failed_ranks_[idx++] = comm_rank;
  }
  failed.end_iteration();

  if (dom){ //we have to sort the failed ranks, potentially
    //the failed rank stuff assumes ascending order
    std::sort(failed_ranks_, failed_ranks_ + num_failed_ranks_);
  }
}

int
dense_rank_map::linear_find_rank(int sparse_rank) const
{
  for (int i=0; i < num_failed_ranks_; ++i){
    if (sparse_rank < failed_ranks_[i]){
        return sparse_rank - i;
    }
  }
  return sparse_rank - num_failed_ranks_;
}

int
dense_rank_map::sparse_rank(int dense_rank) const
{
  int rank = dense_rank;
  for (int i=0; i < num_failed_ranks_; ++i){
    if (rank >= failed_ranks_[i]){
      ++rank;
    }
    else if (rank < failed_ranks_[i]){
      return rank; //all done, found it
    }
  }
  return rank;
}

int
dense_rank_map::dense_rank(int sparse_rank) const
{
  if (num_failed_ranks_ == 0){
    return sparse_rank;
  }
  else if (num_failed_ranks_ <= tree_cutoff){
    return linear_find_rank(sparse_rank);
  }
  else {
    return tree_find_rank(sparse_rank,
        0, num_failed_ranks_, failed_ranks_);
  }
}

int
dense_rank_map::tree_find_rank(
  int sparse_rank,
  int offset,
  int num_failed,
  int* failed_array) const
{
  //printf("Finding dense rank %d at offset=%d for num=%d on array=%p\n",
  //  sparse_rank, offset, num_failed, failed_array);

  if (num_failed == 1){
    if (sparse_rank > failed_array[0]){
      return sparse_rank - (offset + 1);
    }
    else if (sparse_rank < failed_array[0]){
      return sparse_rank - offset;
    }
    else {
      spkt_throw_printf(sprockit::value_error,
        "dense_rank_map::trying to get dense rank for failed process %d",
        sparse_rank);
    }
  }

  int middle_index = num_failed / 2;
  if (sparse_rank < failed_array[middle_index]){
    return tree_find_rank(sparse_rank, offset, middle_index, failed_array);
  }
  else if (sparse_rank > failed_array[middle_index]){
    return tree_find_rank(sparse_rank, 
        offset +  middle_index, 
        num_failed - middle_index, 
        failed_array + middle_index);
  }
  else {
    spkt_throw_printf(sprockit::value_error,
      "dense_rank_map::trying to get dense rank for failed process %d",
      sparse_rank);
  }

}

}

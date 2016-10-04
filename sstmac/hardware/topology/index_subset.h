#ifndef INDEX_SUBSET_H
#define INDEX_SUBSET_H

#include <vector>

namespace sstmac {
namespace hw {

class index_subset  {
 public:
  typedef std::vector<int>::iterator iterator;

 public:
  iterator
  begin() {
    return subset_.begin();
  }

  iterator
  end() {
    return subset_.end();
  }

  void
  push_back(int idx) {
    subset_.push_back(idx);
  }

  void
  clear() {
    subset_.clear();
  }

  size_t
  size() const {
    return subset_.size();
  }

  std::vector<int>&
  data() {
    return subset_;
  }

 private:
  std::vector<int> subset_;
};

}
}

#endif // INDEX_SUBSET_H


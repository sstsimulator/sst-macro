#ifndef sumi_DOMAIN_H
#define sumi_DOMAIN_H

#include <sumi/transport_fwd.h>

namespace sumi {

class domain {
 public:
  virtual int
  nproc() const = 0;

  int
  my_domain_rank() const {
    return my_domain_rank_;
  }

  virtual ~domain(){}

  virtual int
  domain_to_global_rank(int domain_rank) const = 0;

  virtual int
  global_to_domain_rank(int global_rank) const = 0;

 protected:
  int my_domain_rank_;

};

class global_domain :
  public domain
{
 public:
  global_domain(transport* tport);

  int nproc() const;

  int domain_to_global_rank(int domain_rank) const;

  int global_to_domain_rank(int global_rank) const;

 private:
  transport* transport_;
};

class shifted_domain :
  public domain
{
 public:
  shifted_domain(domain* dom, int left_shift) :
    dom_(dom),
    shift_(left_shift),
    nproc_(dom->nproc()) {
    my_domain_rank_ = (dom->my_domain_rank() - left_shift + nproc_) % nproc_;
  }

  int
  nproc() const {
    return dom_->nproc();
  }

  int domain_to_global_rank(int domain_rank) const {
    int shifted_rank = (domain_rank + shift_) % nproc_;
    return dom_->domain_to_global_rank(shifted_rank);
  }

  int global_to_domain_rank(int global_rank) const {
    int domain_rank = dom_->global_to_domain_rank(global_rank);
    int shifted_rank = (domain_rank - shift_ + nproc_) % nproc_;
    return shifted_rank;
  }

 private:
  domain* dom_;
  int nproc_;
  int shift_;

};

class index_domain :
  public domain
{
 public:
  /**
   * @brief index_domain
   * @param nproc
   * @param proc_list
   */
  index_domain(int domain_rank, int nproc, int* proc_list) :
    proc_list_(proc_list), nproc_(nproc)
  {
    my_domain_rank_ = domain_rank;
  }

  int nproc() const {
    return nproc_;
  }

  int domain_to_global_rank(int domain_rank) const {
    return proc_list_[domain_rank];
  }

  int global_to_domain_rank(int global_rank) const;

 private:
  int* proc_list_;
  int nproc_;

};

class rotate_domain :
  public domain
{
 public:
  /**
   * @brief rotate_domain
   * @param nproc
   * @param shift
   * @param me
   */
  rotate_domain(int my_global_rank, int nproc, int shift) :
    nproc_(nproc), shift_(shift)
  {
    my_domain_rank_ = global_to_domain_rank(my_global_rank);
  }

  int nproc() const {
    return nproc_;
  }

  int domain_to_global_rank(int domain_rank) const {
    return (domain_rank + shift_) %  nproc_;
  }

  int global_to_domain_rank(int global_rank) const {
    return (global_rank + nproc_ - shift_) % nproc_;
  }

 private:
  int nproc_;
  int shift_;

};

class subrange_domain :
  public domain
{
 public:
  subrange_domain(int my_global_rank, int start, int nproc) :
    nproc_(nproc), start_(start)
  {
    my_domain_rank_ = global_to_domain_rank(my_global_rank);
  }

  int nproc() const {
    return nproc_;
  }

  int domain_to_global_rank(int domain_rank) const {
    return domain_rank + start_;
  }

  int global_to_domain_rank(int global_rank) const {
    return global_rank - start_;
  }

 private:
  int nproc_;
  int start_;
};

}

#endif // DOMAIN_H

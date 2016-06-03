#ifndef THREAD_SAFE_SET_H
#define THREAD_SAFE_SET_H

#include <set>
#include <sumi/lockable.h>
#include <sprockit/errors.h>

namespace sumi
{

template <class T, class Compare>
class thread_safe_set;

void
set_difference(
    const thread_safe_set<int, std::less<int> >& base,
    const thread_safe_set<int, std::less<int> >& subtract,
    thread_safe_set<int, std::less<int> >& result);

void
set_difference(
    const thread_safe_set<int, std::less<int> >& base,
    const thread_safe_set<int, std::less<int> >& subtract,
    std::set<int, std::less<int> >& result);

/** Thread-safe replacement for std::set, with some differences in API. */
template <class T, class Compare = std::less<T> >
class thread_safe_set
{
  friend void set_difference(
    const thread_safe_set<int, std::less<int> >& base,
    const thread_safe_set<int, std::less<int> >& subtract,
    thread_safe_set<int, std::less<int> >& result);

 public:
  typedef typename std::set<T, Compare>::iterator iterator;
  typedef typename std::set<T, Compare>::const_iterator const_iterator;
  typedef size_t size_type;

  //
  // Constructors.
  //

  thread_safe_set() {}

  void lock() const {}

  void unlock() const {}

  bool locked() const {
    return false;
  }

  thread_safe_set(const std::set<T, Compare> &other){
    lock();
    set_ = other;
    unlock();
  }

  thread_safe_set(const thread_safe_set &other){
    lock();
    other.lock();
    set_ = other.set_;
    other.unlock();
    unlock();
  }

  //
  // These functions acquire a lock.
  //

  const_iterator
  start_iteration() const {
    lock();
    return set_.end();
  }

  iterator
  start_iteration(){
    lock();
    return set_.end();
  }

  //
  // These functions release a previously-acquired lock.
  //

  void
  end_iteration() const {
    unlock();
  }

  //
  // These functions require that a lock has already been acquired
  // (since most take or return iterators).  Note that they do not
  // check WHO acquired the lock (although for correct operation,
  // the caller must be the one who acquired it).
  //

  const_iterator
  begin() const {
    verify("begin");
    return set_.begin();
  }

  iterator
  begin(){
    verify("begin");
    return set_.begin();
  }

  void
  erase(iterator it){
    verify("erase");
    set_.erase(it);
  }

  iterator
  insert(iterator position, const T& entry){
    verify("insert(position,entry)");
    return set_.insert(position, entry);
  }    

  template <class InputIterator>
  void
  insert(InputIterator first, InputIterator last){
    verify("insert(first,last)");
    set_.insert(first, last);
  }

  iterator
  find(const T& entry){
    verify("find");
    return set_.find(entry);
  }

  bool
  empty_locked() const {
    verify("empty_locked");
    return set_.empty();
  }

  void
  clear_locked(){
    verify("clear_locked");
    set_.clear();
  }

  //
  // These functions lock on entry and unlock before returning.
  //

  bool
  empty() const {
    lock();
    bool ret = set_.empty();
    unlock();
    return ret;
  }

  size_type size() const {
    lock();
    size_type ret = set_.size();
    unlock();
    return ret;
  }

  size_type
  count(const T& entry) const {
    lock();
    size_type ret = set_.count(entry);
    unlock();
    return ret;
  }

  /** Generate a string describing the set. */
  std::string
  to_string() const {
    std::stringstream sstr;
    const_iterator it;
    const_iterator end = start_iteration();
    sstr << "{";
    for (it=begin(); it != end; ++it){
      sstr << " " << *it;
    }
    sstr << " }";
    end_iteration();
    return sstr.str();
  }

  /** Get a snapshot of the set that is no longer thread-safe. */
  std::set<T, Compare>
  get_copy() const {
    lock();
    std::set<T, Compare> tmp = set_;
    unlock();
    return tmp;
  }

  thread_safe_set& operator=(const std::set<T, Compare> &rhs){
    lock();
    set_ = rhs;
    unlock();
    return *this;
  }

  thread_safe_set& operator=(const thread_safe_set &rhs){
    if (&rhs != this){
      lock();
      rhs.lock();
      set_ = rhs.set_;
      rhs.unlock();
      unlock();
    }
    return *this;
  }

  size_type
  erase(const T& entry){
    lock();
    size_type ret = set_.erase(entry);
    unlock();
    return ret;
  }

  std::pair<iterator,bool>
  insert(const T& entry){
    lock();
    std::pair<iterator,bool> ret = set_.insert(entry);
    unlock();
    return ret;
  }

  /** Copy all elements from another thread_safe_set into this one. */
  void
  insert_all(const thread_safe_set &other){
    if (&other != this){  // avoid deadlock
      lock();
      const_iterator end = other.start_iteration();
      insert(other.begin(), end);
      other.end_iteration();
      unlock();
    }
  }

  /** Copy all elements of a std::set into this one. */
  void
  insert_all(const std::set<T, Compare> &other){
    lock();
    insert(other.begin(), other.end());
    unlock();
  }

  void
  clear(){
    lock();
    set_.clear();
    unlock();
  }

 protected:
  void
  verify(const char* fxn) const {
    return;
    if (!locked()){
      spkt_throw_printf(sprockit::value_error,
        "thread safe set is not locked in %s", fxn);
    }
  }

  std::set<T, Compare> set_;
};

}

#endif // THREAD_SAFE_SET_H

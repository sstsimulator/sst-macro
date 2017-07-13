/**
namespace sstmac {
class vector {
 public:
  void resize(size_t sz){
    size_ = sz;
  }

  size_t size() const {
    return size_;
  }

  template <class... Args>
  void push_back(Args... args){
    ++size_;
  }

  template <class... Args>
  void emplace_back(Args... args){
    ++size_;
  }

  bool empty() const {
    return size_ == 0;
  }

 private:
  size_t size_;
};
}
*/

template <class T>
struct vector {
 vector() : size_(0), storage_(nullptr){}

 void resize(int size){
  if (size !=  size_){
    delete[] storage_;
    storage_ = new T[size];
    size_ = size;
  } 
 }

 int size() const {
  return size_;
 }

 void null_fxn() {
 }

 T& operator[](int idx){
  return storage_[idx];
 }

 T* storage_;
 int size_;
};



int fxn()
{
#pragma sst null_type sstmac::vector size resize
  vector<double> vec;
  vec.resize(100);

  vec.null_fxn();

  int n = vec.size();

#pragma sst compute
  for (int i=0; i < vec.size(); ++i){
    vec[i] = 5;
  }

  return 0;
}


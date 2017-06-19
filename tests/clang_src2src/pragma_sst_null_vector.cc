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
#pragma sst null_variable except size ctor new resize
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


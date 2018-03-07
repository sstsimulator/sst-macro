
template <class T>
class unique_ptr {
};

template <class T, class... Args>
unique_ptr<T> make_unique(Args&&... args){
  return unique_ptr<T>();
}

struct A {
 template <class... Args>
 A(){}
};


static unique_ptr<A> myA = make_unique<A>(1,2,3,4,"I declare a thumb war");


  

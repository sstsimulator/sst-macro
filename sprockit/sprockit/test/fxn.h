#ifndef FXN_H
#define FXN_H

class TestFxn
{

 public:
  virtual void run() = 0;

};

template <class Fxn>
class TestFxn0 :
  public TestFxn
{
 private:
  Fxn fxn_;

 public:
  TestFxn0(Fxn fxn)
    : fxn_(fxn) {
  }

  void run() {
    (*fxn_)();
  }
};

template <class Fxn, class A>
class TestFxn1 :
  public TestFxn
{
 private:
  Fxn fxn_;

  A a_;

 public:
  TestFxn1(Fxn fxn, const A& a)
    : fxn_(fxn), a_(a) {
  }

  void run() {
    (*fxn_)(a_);
  }
};

template <class Fxn, class A, class B>
class TestFxn2 :
  public TestFxn
{
 private:
  Fxn fxn_;

  A a_;
  
  B b_;

 public:
  TestFxn2(Fxn fxn, const A& a, const B& b)
    : fxn_(fxn), a_(a), b_(b) {
  }

  void run() {
    (*fxn_)(a_, b_);
  }
};

template <class Fxn, class A, class B, class C>
class TestFxn3 :
  public TestFxn
{
 private:
  Fxn fxn_;

  A a_;
  
  B b_;
  
  C c_;

 public:
  TestFxn3(Fxn fxn, const A& a, const B& b, const C& c)
    : fxn_(fxn), a_(a), b_(b), c_(c) {
  }

  void run() {
    (*fxn_)(a_, b_, c_);
  }
};

template <class Fxn, class Cls, class A>
class TestMemberFxn1 :
  public TestFxn
{
 private:
  Fxn fxn_;

  Cls* cls_;

  A a_;

 public:
  TestMemberFxn1(Fxn fxn, Cls* cls, const A& a)
    : fxn_(fxn), cls_(cls), a_(a) {
  }

  void run() {
    (cls_->*fxn_)(a_);
  }
};

template <class Fxn, class Cls, class A, class B>
class TestMemberFxn2 :
  public TestFxn
{
 private:
  Fxn fxn_;

  Cls* cls_;

  A a_;
  
  B b_;

 public:
  TestMemberFxn2(Fxn fxn, Cls* cls, const A& a, const B& b)
    : fxn_(fxn), cls_(cls), a_(a), b_(b) {
  }

  void run() {
    (cls_->*fxn_)(a_,b_);
  }
};

template <class Fxn, class Cls, class A, class B, class C>
class TestMemberFxn3 :
  public TestFxn
{
 private:
  Fxn fxn_;

  Cls* cls_;

  A a_;
  
  B b_;
  
  C c_;

 public:
  TestMemberFxn3(Fxn fxn, Cls* cls, const A& a, const B& b, const C& c)
    : fxn_(fxn), cls_(cls), a_(a), b_(b), c_(c) {
  }

  void run() {
    (cls_->*fxn_)(a_,b_,c_);
  }
};

template <typename Fxn>
TestFxn*
static_fxn(Fxn fxn)
{
  return new TestFxn0<Fxn>(fxn);
}

template <typename Fxn, class A>
TestFxn*
static_fxn(Fxn fxn, const A& a)
{
  return new TestFxn1<Fxn,A>(fxn,a);
}

template <typename Fxn, class A, class B>
TestFxn*
static_fxn(Fxn fxn, const A& a, const B& b)
{
  return new TestFxn2<Fxn,A,B>(fxn,a,b);
}

template <typename Fxn, class A, class B, class C>
TestFxn*
static_fxn(Fxn fxn, const A& a, const B& b, const C& c)
{
  return new TestFxn3<Fxn,A,B,C>(fxn,a,b,c);
}


template <typename Fxn, class Cls, class A>
TestFxn*
member_fxn(Cls* cls, Fxn fxn, const A& a)
{
  return new TestMemberFxn1<Fxn,Cls,A>(fxn,cls,a);
}

template <typename Fxn, class Cls, class A, class B>
TestFxn*
member_fxn(Cls* cls, Fxn fxn, const A& a, const B& b)
{
  return new TestMemberFxn2<Fxn,Cls,A,B>(fxn,cls,a,b);
}

template <typename Fxn, class Cls, class A, class B, class C>
TestFxn*
member_fxn(Cls* cls, Fxn fxn, const A& a, const B& b, const C& c)
{
  return new TestMemberFxn3<Fxn,Cls,A,B,C>(fxn,cls,a,b,c);
}

#endif // FXN_H


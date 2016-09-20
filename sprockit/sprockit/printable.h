#ifndef PRINTABLE_H
#define PRINTABLE_H

namespace sprockit
{

class printable
{
 public:
  virtual std::string
  to_string() const = 0;
};

template <class T>
std::string
to_string(T* t){
  printable* p = dynamic_cast<printable*>(t);
  if (p) return p->to_string();
  else return " (no string) ";
}

}

#endif // PRINTABLE_H

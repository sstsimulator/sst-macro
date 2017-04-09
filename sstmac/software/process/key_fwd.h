#ifndef KEY_FWD_H
#define KEY_FWD_H

namespace sstmac {
namespace sw {

class key;

namespace key_traits {

class category
{
 private:
  friend class library;

  int id_;

  //only callable by library
  category();

 public:
  category(const char* name);

  int id() const {
    return id_;
  }
};

}



}
}


#endif // KEY_FWD_H


#ifndef API_H
#define API_H

#include <string>
#include <sprockit/unordered.h>

namespace sstmac {
namespace sw {


class APIBase
{
 protected:
  static int id_counter;

  static spkt_unordered_map<std::string,int>* name_to_id;

  APIBase(const char* name);

 public:
  static int
  api_id(const std::string& name);

  static void
  delete_statics();

};

template <class T>
class API :
  public APIBase
{
 public:
  static int id;
  static const char* name;
  static const int null_id = -1;

  API() : APIBase(name) {
    id = id_counter;
    ++id_counter;
  }
};


}
}

#define ImplementAPI(nmsp, cls, cls_str) \
    typedef nmsp::cls __tempory_api_##cls; \
    namespace sstmac { \
        namespace sw { \
    template<> int API<__tempory_api_##cls>::id = 0; \
    template<> const char* API<__tempory_api_##cls>::name = cls_str; \
    static API<__tempory_api_##cls> cls##_api_implementer; \
        } \
    }


#endif // API_H


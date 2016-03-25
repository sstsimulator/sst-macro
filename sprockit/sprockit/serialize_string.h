#ifndef SERIALIZE_STRING_H
#define SERIALIZE_STRING_H

#include <sprockit/serializer.h>

namespace sprockit {

template <>
class serialize<std::string> {
 public:
 void operator()(std::string& str, serializer& ser){
   ser.string(str);
 }
};

}

#endif // SERIALIZE_STRING_H

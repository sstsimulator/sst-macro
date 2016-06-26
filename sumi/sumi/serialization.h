#ifndef sumi_serialization_h
#define sumi_serialization_h

#include <sumi/sumi_config.h>

#if SUMI_INTEGRATED_SST_CORE
#include <sst/core/serialization/serializable.h>
#include <sst/core/serialization/serialize_serializable.h>
#include <sst/core/serialization/serializer.h>

#define START_SERIALIZATION_NAMESPACE namespace SST { namespace Core { namespace Serialization {
#define END_SERIALIZATION_NAMESPACE } } }

namespace sumi {
using SST::Core::Serialization::serializable;
using SST::Core::Serialization::serializable_type;
using SST::Core::Serialization::buffer;
using SST::Core::Serialization::array;
using SST::Core::Serialization::serialize;
typedef SST::Core::Serialization::serializer serializer;
}
#else

#include <sprockit/serializable.h>
#include <sprockit/serialize_serializable.h>
#include <sprockit/serializer.h>

#define START_SERIALIZATION_NAMESPACE namespace sprockit {
#define END_SERIALIZATION_NAMESPACE }


namespace sumi {
using sprockit::serializable;
using sprockit::serializable_type;
typedef sprockit::serializer serializer;
using sprockit::buffer;
using sprockit::array;
}
#endif

#endif


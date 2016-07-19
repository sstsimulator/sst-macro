#ifndef sstmac_common_serializable_h
#define sstmac_common_serializable_h

#include <sstmac/common/sstmac_config.h>

#if SSTMAC_INTEGRATED_SST_CORE
#include <sst/core/serialization/serializer_fwd.h>
#include <sst/core/serialization/serializable.h>
#include <sst/core/serialization/serialize_serializable.h>
#include <sst/core/serialization/serializer.h>

#define START_SERIALIZATION_NAMESPACE namespace SST { namespace Core { namespace Serialization {
#define END_SERIALIZATION_NAMESPACE } } }

#define FRIEND_SERIALIZATION   template <class T, class Enable> friend class sstmac::serialize

namespace sstmac {
using SST::Core::Serialization::serializable;
using SST::Core::Serialization::serializable_type;
using SST::Core::Serialization::serialize;
using SST::Core::Serialization::serializer;
using SST::Core::Serialization::array;
using SST::Core::Serialization::raw_ptr;
}

#define SER_NAMESPACE_OPEN \
   namespace SST { namespace Core { namespace Serialization {
#define SER_NAMESPACE_CLOSE } } }

#else
#include <sprockit/serializer_fwd.h>
#include <sprockit/serializable.h>
#include <sprockit/serialize_serializable.h>
#include <sprockit/serializer.h>

#define START_SERIALIZATION_NAMESPACE namespace sprockit {
#define END_SERIALIZATION_NAMESPACE }

#define FRIEND_SERIALIZATION   template <class T> friend class sstmac::serialize

namespace sstmac {
using sprockit::serializable;
using sprockit::serializable_type;
using sprockit::serialize;
using sprockit::serializer;
using sprockit::array;
using sprockit::raw_ptr;
}

#define SER_NAMESPACE_OPEN namespace sprockit {
#define SER_NAMESPACE_CLOSE }

#endif

#endif


#ifndef CLONABLE_H
#define CLONABLE_H


namespace sstmac {
namespace hw {

template <class CloneType, class CloneArg1>
class clone_factory  {
 public:

  /**
   @param arg1 A parameter for creating the clone
   @return The cloned object
   @throws sprockit::value_error If invalid parameter passed in
  */
  virtual CloneType
  clone(const CloneArg1& arg1) const = 0;

  virtual ~clone_factory() {}
};

}
}

#endif // CLONABLE_H


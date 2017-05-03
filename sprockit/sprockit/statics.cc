#include <sprockit/statics.h>

namespace sprockit {

std::list<statics::clear_fxn>* statics::fxns_ = nullptr;

void
statics::register_finish(clear_fxn fxn)
{
  if (fxns_ == nullptr){
    fxns_ = new std::list<statics::clear_fxn>;
  }
  fxns_->push_back(fxn);
}

void
statics::finish()
{
  if (fxns_ == nullptr)
    return;

  std::list<clear_fxn>::iterator it, end = fxns_->end();
  for (it=fxns_->begin(); it != end; ++it){
    clear_fxn fxn = *it;
    fxn();
  }
  fxns_->clear();
  delete fxns_;
  fxns_ = nullptr;
}

}

#ifndef sprockit_common_DELETE_H
#define sprockit_common_DELETE_H

namespace sprockit {

template <class Container>
void
delete_all(const Container& ctr){
  typedef typename Container::value_type T;
  typename Container::const_iterator it, end = ctr.end();
  for (it=ctr.begin(); it != end; ++it){
    delete *it;
  }
}

template <class Container>
void
delete_arrs(const Container& ctr){
  typedef typename Container::value_type T;
  typename Container::const_iterator it, end = ctr.end();
  for (it=ctr.begin(); it != end; ++it){
    delete[] *it;
  }
}

template <class MapType>
void
delete_vals_arrs(const MapType& ctr){
  typedef typename MapType::key_type T;
  typedef typename MapType::mapped_type Val;
  typename MapType::const_iterator it, end = ctr.end();
  for (it=ctr.begin(); it != end; ++it){
    delete[] it->second;
  }
}

template <class MapType>
void
delete_vals(const MapType& ctr){
  typedef typename MapType::key_type T;
  typedef typename MapType::mapped_type Val;
  typename MapType::const_iterator it, end = ctr.end();
  for (it=ctr.begin(); it != end; ++it){
    delete it->second;
  }
}

template <class MapType>
void
delete_keys(const MapType& ctr){
  typedef typename MapType::key_type T;
  typedef typename MapType::mapped_type Val;
  typename MapType::const_iterator it, end = ctr.end();
  for (it=ctr.begin(); it != end; ++it){
    delete it->first;
  }
}

template <class MapType>
void
delete_keys_and_vals(const MapType& ctr){
  typedef typename MapType::key_type T;
  typedef typename MapType::mapped_type Val;
  typename MapType::const_iterator it, end = ctr.end();
  for (it=ctr.begin(); it != end; ++it){
    delete it->first;
    delete it->second;
  }
}

}

#endif // DELETE_H

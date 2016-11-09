#ifndef sprockit_common_DELETE_H
#define sprockit_common_DELETE_H

namespace sprockit {

template <class Container>
void
delete_all(const Container& ctr){
  for (auto& item : ctr){
    delete item;
  }
}

template <class Container>
void
delete_arrs(const Container& ctr){
  for (auto& item : ctr){
    delete[] item;
  }
}

template <class MapType>
void
delete_vals_arrs(const MapType& ctr){
  for (auto& pair : ctr){
    delete[] pair.second;
  }
}

template <class VectorType>
void
delete_vector(const VectorType& v){
  for (auto ptr : v){
    if (ptr) delete ptr;
  }
}


template <class MapType>
void
delete_vals(const MapType& ctr){
  for (auto& pair : ctr){
    delete pair.second;
  }
}

template <class MapType>
void
delete_keys(const MapType& ctr){
  for (auto& pair : ctr){
    delete pair.first;
  }
}

template <class MapType>
void
delete_keys_and_vals(const MapType& ctr){
  for (auto& pair : ctr){
    delete pair.first;
    delete pair.second;
  }
}

}

#endif // DELETE_H

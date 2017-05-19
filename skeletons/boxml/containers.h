/**
Copyright 2009-2017 National Technology and Engineering Solutions of Sandia, 
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S.  Government 
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly 
owned subsidiary of Honeywell International, Inc., for the U.S. Department of 
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2017, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Sandia Corporation nor the names of its
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Questions? Contact sst-macro-help@sandia.gov
*/

#ifndef CONTAINERS_H
#define CONTAINERS_H

#include <vector>
#include <string>
#include <stdio.h>
//#include <regex>
#include <sstmac/common/sstmac_config.h>
#include <sstmac/common/serializable.h>
#include <sprockit/unordered.h>

static std::set<int> empty_set;

namespace lblxml
{
  typedef spkt_unordered_map<int,int> box_to_domain_rank_map;
  typedef spkt_unordered_map<int,std::set<int> > box_to_listener_map;
  typedef spkt_unordered_set<int> int_container_t;
  typedef spkt_unordered_set<int>::iterator int_container_iter;

  int get_index(std::string& id);

  int get_index(const char* cstr);

  // not fully supported by compilers
  //static
  //std::vector<std::string>
  //split(const std::string& input, const std::string& delim) {
  //  std::regex re(delim);
  //  std::sregex_token_iterator
  //      first{input.begin(), input.end(), re, -1},
  //      last;
  //  return {first, last};
  //}

  static
  void
  split(std::string& input, 
        const char* delim, 
        std::vector<std::string>& output)
  {
    char *token;
    char cstr[input.length()+1];
    std::strcpy (cstr, input.c_str());
    token = strtok(cstr,delim);
    while( token != NULL ) 
    {
       output.push_back(token);
       token = strtok(NULL, delim);
    }
  }
  

  class element : 
    public sstmac::serializable
  {
  public: 
    element() : index_(-1), id_("null")
    { }

    element( const element& copy) : index_(copy.index_), id_(copy.id_)
    { std::cerr << "copied element\n"; }

    element(int index, std::string id) : index_(index), id_(id)
    { }

    ~element()
    { }

    int index() const
    { return index_; }

    std::string id() const
    { return id_; }

    virtual void print()
    { std::cout << "index: " << index_ << " id: " << id_ << "\n"; }

    virtual void
    serialize_order(sstmac::serializer &ser){
      ser & index_;
      ser & id_;
    }

  private:
    int index_;
    std::string id_;
  };

  class box :
    public element,
    public sstmac::serializable_type<box>
  {
    ImplementSerializableDefaultConstructor(box)

  public:   
    box() : loc_(-1)
    { }

    box( const box& copy) : element(copy), loc_(copy.loc_)
    { }

    box(int index, std::string id, int loc) : element(index,id),
      loc_(loc)
    { }

    ~box()
    { }

    int loc()
    { return loc_; }

    void change_loc(int newl)
    { loc_ = newl; }

    virtual void print() override
    {
      std::cout << "box: " << "loc: " << loc_ << " ";
      element::print();
    }

    virtual void
    serialize_order(sstmac::serializer &ser) override {
      ser & loc_;
      element::serialize_order(ser);
    }

  private:
    int loc_;
  };

  class event :
    public element
  {
  public:
    typedef enum { none, collective, computation, pt2pt } event_type_t;
    event() : event_type_(none)
    { }

    event(int index, const std::string& id, std::string& dep, int epoch) :
      element(index,id), event_type_(none), epoch_(epoch)
    {
      std::vector<std::string> splitvec;
      split(dep, ",", splitvec);
      if(splitvec.size() == 1 && splitvec[0].length() == 0)
        return;

      int_container_iter dep_it = dep_.begin();
      for(int i=0; i<splitvec.size(); ++i) {
        //std::string dep_id = splitvec[i];
        int dep_index = get_index(splitvec[i]);
        dep_.insert(dep_index);
        //dep_.insert(dep_it,dep_index);
        //++dep_it;
      }
    }

    virtual void
    serialize_order(sstmac::serializer &ser) override {
      ser & dep_;
      ser & event_type_;
      element::serialize_order(ser);
    }

    void
    set_event_type(event_type_t ty) {
      event_type_ = ty;
    }

    event_type_t
    event_type() const {
      return event_type_;
    }

    virtual ~event() {}

    void remove_dep(int index)
    {
      int_container_iter dep_it = dep_.find(index);
      if (dep_it != dep_.end()) 
        dep_.erase(dep_.find(index));
    }

    int n_dep() { return dep_.size(); }

    int_container_t& get_dep() { return dep_; }

    int epoch() { return epoch_; }

    virtual void print() override
    {
      int_container_iter dep_it = dep_.begin();
      if (dep_it == dep_.end() )
        std::cout << "dep: null ";
      else
        for(; dep_it != dep_.end(); ++dep_it)
          std::cout << "dep: " << *dep_it << " ";
      element::print();
    }

  private:
    int_container_t dep_;
    int epoch_;

  protected:
    event_type_t event_type_;
  };

  class simple_event : public event
  {
   public:
    simple_event(int index, const std::string& id, std::string& dep,
                 int epoch)
      : event(index, id, dep, epoch)
    {
    }

    simple_event(){}

    virtual ~simple_event(){}

    void add_listener(int index)
    {
      listeners_.insert(index);
    }

    int_container_t& get_listeners()
    {
      return listeners_;
    }

    virtual void
    serialize_order(sstmac::serializer &ser){
      //don't serialize the listeners - these get computed later
      event::serialize_order(ser);
    }

   private:
    int_container_t listeners_;
  };

  class comp :
    public simple_event,
    public sstmac::serializable_type<comp>
  {
    ImplementSerializableDefaultConstructor(comp)
  public:
    comp() : type_(uninitialized), size_(-1), time_(-1), at_(-1)
    { event_type_ = computation; }

    comp(int index, std::string id, std::string dep, int epoch, std::string type,
         int size, double time, int at) :
      simple_event(index, id, dep, epoch), size_(size), time_(time), at_(at)
    {
      if(type == "averageAndReflux")
        type_ = averageAndReflux;
      else if(type == "integrate")
        type_ = integrate;
      else if(type == "interpolate")
        type_ = interpolate;
      else if(type == "None")
        type_ = uninitialized;
      else {
        //std::cerr << "unknown computation type\n";
        //abort();
        type_ = uninitialized;
      }
      event_type_ = computation;
    }

    ~comp() {}

    int at() { return at_; }

    double time() { return time_; }

    virtual void print() override
    {
      std::cout << "comp: type: " << type_ << " size: " << size_
                << " time: " << time_ << " at: " << at_ << " ";
      event::print();
    }

    virtual void
    serialize_order(sstmac::serializer &ser) override {
      ser & type_;
      ser & size_;
      ser & time_;
      ser & at_;
      simple_event::serialize_order(ser);
    }

    enum operation {uninitialized, averageAndReflux, integrate, interpolate};

  private:
    operation type_;
    int size_;
    double time_;
    int at_;
  };

  class comm :
    public simple_event,
    public sstmac::serializable_type<comm>
  {
    ImplementSerializableDefaultConstructor(comm)
  public:
    enum operation {uninitialized, copy, copyInto};

    comm() : type_(uninitialized), from_(-1), to_(-1), size_(-1)
    { event_type_ = pt2pt; }

    comm( const comm& copy) : type_(copy.type_), from_(copy.from_),
      to_(copy.to_), size_(copy.size_)
    { event_type_ = pt2pt; }

    comm(int index, const std::string& id, std::string& dep,
         int epoch, const std::string& type,
         int from, int to, int size) :
      simple_event(index, id, dep, epoch), from_(from), to_(to), size_(size)
    {
      event_type_ = pt2pt;
      if( type == "copy")
        type_ = copy;
      else if(type == "copyInto")
        type_ = copyInto;
      else {
        //std::cerr << "unknown communication type\n";
        //abort();
        type_ = uninitialized;
      }
    }

    ~comm() {}

    operation type() { return type_; }

    int from() { return from_; }

    int to() { return to_; }

    int size() { return size_; }

    virtual void
    serialize_order(sstmac::serializer &ser) override {
      ser & type_;
      ser & from_;
      ser & to_;
      ser & size_;
      simple_event::serialize_order(ser);
    }

  private:
    operation type_;
    int from_;
    int to_;
    int size_;
  };

  class reduce :
    public event,
    public sstmac::serializable_type<reduce>
  {
    ImplementSerializable(reduce)
   public:
    reduce()
      : event(), size_(0), box_array_(0)
    {
      event_type_ = collective;
    }

    reduce(int index, const std::string& id, std::string& dep,
           int epoch, int size)
      : event(index, id, dep, epoch), size_(size), box_array_(0)
    {
      event_type_ = collective;
    }

    virtual void print() override
    {
      std::cout << "allreduce: "
                << " size: " << size_  << " "
                << " nboxes: " << team_map_.size() << std::endl;
      event::print();
    }

    void add_team(const std::string& teamstr)
    {
      char cstr[teamstr.length()+1];
      std::strcpy (cstr, teamstr.c_str());
      std::vector<std::string> splitvec;
      std::string str(cstr);
      split(str, ",", splitvec);
      if(splitvec.size() == 1 && splitvec[0].length() == 0)
        return;

      for(int i=0; i<splitvec.size(); ++i) {
        std::string id = splitvec[i];
        int box_number = get_index(id);
        int domain_rank = i;
        team_map_[box_number] = domain_rank;
      }
    }

    const int*
    box_array() const {
      return box_array_.data();
    }

    box_to_domain_rank_map& get_team()
    {
      return team_map_;
    }

    int domain_rank(int box_number) const {
      box_to_domain_rank_map::const_iterator it = team_map_.find(box_number);
      if (it == team_map_.end()){
        spkt_throw_printf(sprockit::value_error,
          "invalid box number %d to allreduce id %d", box_number, index());
      }
      return it->second;
    }

    int size() const { return size_; }

    int nboxes() const { return team_map_.size(); }

    void
    compute_box_array() {
      box_array_.resize(team_map_.size());
      box_to_domain_rank_map::iterator it, end = team_map_.end();
      for (it=team_map_.begin(); it != end; ++it){
        int domain_rank = it->second;
        int box_number = it->first;
        box_array_[domain_rank] = box_number;
      }
    }

    void
    add_listener(int box_number, int event_id) {
      listener_map_[box_number].insert(event_id);
    }

    typedef std::set<int>::const_iterator listener_iterator;

    listener_iterator listener_begin(int box_number) const {
      box_to_listener_map::const_iterator it = listener_map_.find(box_number);
      if (it == listener_map_.end())
        return empty_set.begin();
      else
        return it->second.begin();
    }

    listener_iterator listener_end(int box_number) const {
      box_to_listener_map::const_iterator it = listener_map_.find(box_number);
      if (it == listener_map_.end())
        return empty_set.end();
      else
        return it->second.end();
    }

    virtual void
    serialize_order(sstmac::serializer &ser) override {
      ser & size_;
      ser & team_map_;
      //don't do listeners, computed later
      event::serialize_order(ser);
    }

   private:
    int size_;
    box_to_domain_rank_map team_map_;
    box_to_listener_map listener_map_;
    std::vector<int> box_array_;

  };

void
init_event_locks();

void
release_event(int index);

event*
acquire_event(int index);

} // end of namespace lblxml

#endif // CONTAINERS_H
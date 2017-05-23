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

#include "boxml.h"
#include <sstmac/common/sstmac_config.h>

using namespace std;
using namespace tinyxml2;

namespace lblxml
{
  void
  erase_all(std::string& str, const char delim){
    str.erase(std::remove(str.begin(), str.end(), delim), str.end());
  }

  void read_box(XMLElement* element, int debug);
  void read_comm_event(XMLElement* element, int debug);
  void read_coll_event(XMLElement* element, int debug);
  void read_comp_event(XMLElement* element, int debug);
  void populate_listeners();

  int get_index(string& id) {
    erase_all(id, 'B');
    erase_all(id, 'e');
    return atoi( id.c_str() );
  }

  int get_index(const char* cstr){
    std::string str = cstr;
    return get_index(str);
  }

  int count_xml(tinyxml2::XMLDocument* doc,
                const string& level1,
                const string& level2) {

    /**
    TODO debug_ is not defined
    if (debug_ > 1) {
      cout << "Reading " << level1
                        << " of type " << level2 << "\n";
    }
    */

    int count = 0;

    XMLElement* e1 = doc->FirstChildElement( level1.c_str() );
    if (!e1) {
      cerr << "can't find " << level1 << "\n";
      abort();
    }
    XMLElement* e2 = e1->FirstChildElement( level2.c_str() );
    if (!e2) {
      cerr << "can't find first " << level2 << "\n";
      abort();
    }
    XMLElement* elast = e1->LastChildElement( level2.c_str() );
    if (!elast) {
      cerr << "can't find last " << level2 << "\n";
      abort();
    }
    while (e2 != elast) {
      ++count;
      e2 = e2->NextSiblingElement( level2.c_str() );
    }
    ++count;

    return count;
  }

  void boxml::process_xml(tinyxml2::XMLDocument* doc,
                         string level1, string level2,
                         void (*fp)(XMLElement*,int))
  {
    if (debug_ > 1) {
      cout << "Reading " << level1
                        << " of type " << level2 << "\n";
    }
    XMLElement* e1 = doc->FirstChildElement( level1.c_str() );
    if (!e1) {
      cerr << "can't find " << level1 << "\n";
      abort();
    }
    XMLElement* e2 = e1->FirstChildElement( level2.c_str() );
    if (!e2) {
      cerr << "can't find first " << level2 << "\n";
      abort();
    }
    XMLElement* elast = e1->LastChildElement( level2.c_str() );
    if (!elast) {
      cerr << "can't find last " << level2 << "\n";
      abort();
    }
    static int eventnum = 0;
    while (e2 != elast) {
      if (eventnum % 1000000 == 0){
        printf("Processing event %d\n", eventnum);
        fflush(stdout);
      }
      ++eventnum;
      (*fp)(e2,debug_);
      e2 = e2->NextSiblingElement( level2.c_str() );
    }
    (*fp)(elast,debug_);
  }

  void read_box(XMLElement* element, int debug) {
    string boxid = element->Attribute("id");
    int index = get_index(boxid);
    int loc = element->IntAttribute("loc");
    box* b = new box(index, boxid, loc);
    g_boxes[index] = b;
    if (debug > 1) b->print();
  }

  void read_comm_event(XMLElement* element, int debug) {
    string eventid = element->Attribute("id");
    int index = get_index(eventid);
    string dep = element->Attribute("dep");
    int from = get_index( element->Attribute("from") );
    int to = get_index( element->Attribute("to") );
    int size = element->IntAttribute("size");
    int epoch = element->IntAttribute("epoch");
    comm* ev = new comm(index, eventid, dep, epoch, "uninitialized", from, to, size );
    if (index >= g_events.size()){
      spkt_throw_printf(sprockit::value_error,
        "event id %d is greater than max %d - check XML file for largest event id",
        index, g_events.size());
    }
    g_events[index] = ev;
    if (debug > 1) g_events[index]->print();
  }

  void read_coll_event(XMLElement* element, int debug) {
    string eventid = element->Attribute("id");
    int index = get_index(eventid);
    string dep = element->Attribute("dep");
    //string type = element->Attribute("type");
    int size = element->IntAttribute("size");
    int epoch = element->IntAttribute("epoch");
    reduce* commev = new reduce(index, eventid, dep, epoch, size);
    if (index >= g_events.size()){
      spkt_throw_printf(sprockit::value_error,
        "event id %d is greater than max %d - check XML file for largest event id",
        index, g_events.size());
    }
    g_events[index] = commev;
    commev->add_team( element->Attribute("team") );
    commev->compute_box_array();
    if (debug > 1) commev->print();
    fflush(stdout);
  }

  void read_comp_event(XMLElement* element, int debug) {
    string eventid = element->Attribute("id");
    int index = get_index(eventid);
    string dep = element->Attribute("dep");
    int at = get_index( element->Attribute("at") );
    double time = element->DoubleAttribute("time");
    int epoch = element->IntAttribute("epoch");
    comp* ev = new comp(index, eventid, dep, epoch, "uninitialized", -1, time, at );
    if (index >= g_events.size()){
      spkt_throw_printf(sprockit::value_error,
        "event id %d is greater than max %d - check XML file for largest event id",
        index, g_events.size());
    }
    g_events[index] = ev;
    if (debug > 1) g_events[index]->print();
  }

}
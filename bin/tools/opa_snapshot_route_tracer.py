#!/usr/bin/env python3
__license__ = """
Copyright 2009-2023 National Technology and Engineering Solutions of Sandia,
LLC (NTESS).  Under the terms of Contract DE-NA-0003525, the U.S. Government
retains certain rights in this software.

Sandia National Laboratories is a multimission laboratory managed and operated
by National Technology and Engineering Solutions of Sandia, LLC., a wholly
owned subsidiary of Honeywell International, Inc., for the U.S. Department of
Energy's National Nuclear Security Administration under contract DE-NA0003525.

Copyright (c) 2009-2023, NTESS

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, 
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the copyright holder nor the names of its
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
"""

import argparse, json


def parse_args():
    parser = argparse.ArgumentParser(description='Traces network routes between nodes using json output from opareport_snapshot_parser.py')
    parser.add_argument('-l', '--link_delim', default='\n', help='Delimiter to use between fabric hops (including intraswitch)')
    parser.add_argument('-p', '--port_delim', default=':', help='Delimiter to use between port and hostname')
    parser.add_argument('FILE', help='JSON output from opareport_snapshot_parser.py')
    parser.add_argument('START', help='Beginning of the route to trace')
    parser.add_argument('END', help='End of the route to trace')

    return parser.parse_args()


def gen_global_tables(f_name):
    with open(f_name) as f:
        parsed = json.load(f)

        global routing_table
        global name_guid_map
        global guid_name_map
        global guid_lid_map
        global lid_guid_map
        global link_table

        routing_table = parsed['routing_table']
        name_guid_map = parsed['name_guid_map']
        guid_name_map = parsed['guid_name_map']
        guid_lid_map = parsed['guid_lid_map']
        lid_guid_map = parsed['lid_guid_map']
        link_table = parsed['link_table']


def get_link(guid, port):
    return link_table[str(guid)][str(port)] if str(guid) in link_table else False


def trace_route(start_lid, dest_lid):
    dest_guid = lid_guid_map[str(dest_lid)]
    cur_guid = lid_guid_map[str(start_lid)]

    route_list = []
    while cur_guid != dest_guid:
        port = routing_table[str(cur_guid)][str(dest_lid)] if str(cur_guid) in routing_table else 1
        route_list.append((cur_guid, port))
        cur_guid, port = get_link(cur_guid, port)
        route_list.append((cur_guid, port))

    return route_list


def trace_route_from_name(start_name, dest_name):
    return trace_route(
        guid_lid_map[str(name_guid_map[start_name])],
        guid_lid_map[str(name_guid_map[dest_name])])


def print_route(start_name, dest_name, port_delim=':', link_delim='\n', padding=0):
    route = trace_route_from_name(start_name, dest_name)
    hop_list = []
    for hop in route:
        guid, port = hop
        hop_list.append(('%s%s%i' % (guid_name_map[str(guid)], port_delim, port)))
    print(link_delim.join(hop_list))


if __name__ == '__main__':
    args = parse_args()
    gen_global_tables(args.FILE)
    print_route(args.START,
                args.END,
                link_delim=args.link_delim,
                port_delim=args.port_delim)

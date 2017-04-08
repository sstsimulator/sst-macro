#!/usr/bin/perl

#  This file is part of SST/macroscale: 
#               The macroscale architecture simulator from the SST suite.
#  Copyright (c) 2009-2010 Sandia Corporation.
#  This software is distributed under the BSD License.
#  Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
#  the U.S. Government retains certain rights in this software.
#  For more information, see the LICENSE file in the top 
#  SST/macroscale directory.

# APR2011 Evensky : Initial version.
# JUL2011 Evensky : Refactored for public consumption.


# Create nodemap file for undumpi from xtdb2proc output such that:
#
#Valid arguments for --nodemap are names of files with the format:
#       nnode ncoor
#       nodename(0) coor(0,0) coor(0,1) ... coor(0,ncoor)
#       nodename(1) coor(1,0) coor(1,1) ... coor(1,ncoor)
#       ...
#       nodename(nnode) coor(nnode,0) coor(nnode,1) ... coor(nnode,ncoor)
#  Note that the last coordinate is the coordinate of the node
#    on a switch. This is usually zero.
#
# the input looks like:
#
#cpu=2,slot=7,cage=0,cabinet=null,cab_position=0,cab_row=0,x_coord=0,y_coord=2,z_coord=7,process_slots=4,process_slots_free=4,processor_status='up',processor_type='compute',alloc_mode='interactive',processor_id=30,od_allocator_id=0,next_red_black_switch=null,processor_spec=null
#
#
# Usage: currently this script acts as a filter. It takes no command line args, and reads the output of
# xtdb2proc from stdin, and write the node file to stdout.
#
# E.g. xtdb2proc | xt2nodemap.pl > nodefile.

my $index_stride = 100;

sub output {
    my $print_comments = shift;
    my $max_coords = shift;
    my $file_info = shift;

    my $xsize = $max_coords->{x} + 1;
    my $ysize = $max_coords->{y} + 1;
    my $zsize = $max_coords->{z} + 1;
    print  "#Mesh Size: $xsize, $ysize, $zsize\n" if ($print_comments);
    for my $c (keys %{$file_info}) {
	#print "#$c:\n" I don't really see any reason to print this
	#    if ($print_comments);
	for my $n (sort {$a <=> $b} keys %{$file_info->{$c}}) {
	    my $o = $file_info->{$c}->{$n};
	    print "$o\n";
	}
    }
}

sub is_in {
    my $key = shift;
    my $list = shift;
    my $found = 0;
    for $_ (@{$list}) {
	if ($_ eq $key) {
	    $found = 1;
	    last;
	}
    }
    $found;
}

sub get_node_info {
    my $line = shift;
    my $important = ['x_coord','y_coord','z_coord','processor_id','processor_type','alloc_mode'];
    my @busted = split(",",$line);
    my $info = {};
    for my $f (@busted) {
	if ($f =~ /='/ and $f !~ /'$/) {
	    print "We have a bug! there was an unhandled comma in <$f>\n";
	    print "Please report this error.\n";
	    exit(1);
	}
	my ($key,$val) = split("=",$f);
	if (is_in($key,$important)) {
	    $val =~ s/'//g;
	    $info->{$key} = $val;
	}
    }
    $info;
}

sub build_key {
    my $info = shift;
    my $key = join(" ",($info->{processor_type}, $info->{alloc_mode}));
    $key =~ s/ +/ /g;
    $key;
}

sub update_node_list {
    my $print_comments = shift;
    my $file_info = shift;
    my $catagories = shift;
    my $info = shift;
    my $switch_counts = shift;
    my $topology = shift;

    #compute a simple integer hash for evaluating the number of nodes per switch
    my $switch_index = 0;
    $switch_index = $switch_index * $index_stride + $info->{x_coord};
    $switch_index = $switch_index * $index_stride + $info->{y_coord};
    $switch_index = $switch_index * $index_stride + $info->{z_coord};
    my $switch_port = 0;
    if (exists $switch_counts{$switch_index}){
        $switch_port = $switch_counts{$switch_index};
        $switch_counts{$switch_index} += 1;
    }
    else{
        $switch_counts{$switch_index} = 1;
    }

    $file_info->{$catagories} = {}
    unless (defined($file_info->{$catagories}));
    #each coordinate in a 3D torus has to have a switch number
    #associated with it - this gives a total of 6 coordinates
    my $o = "";
    if ($topology eq "torus3"){
        $o = sprintf("nid%05d %d %d %d %d %d %d",
                $info->{processor_id},
                $info->{x_coord},
                $switch_port, #for now just assign the switch number along the x-axis
                $info->{y_coord},
                0,
                $info->{z_coord},
                0);
    }
    elsif ($topology eq "hdtorus"){
        $o = sprintf("nid%05d %d %d %d %d",
                $info->{processor_id},
                $info->{x_coord},$info->{y_coord},$info->{z_coord},
                $switch_port);
    }
    #$o = "$o # $catagories" I don't really see why this needs to be here
	#if ($print_comments);
    $file_info->{$catagories}->{$info->{processor_id}} = $o;
    1;
}

sub update_max {
    my $maxs = shift;
    my $info = shift;
    $maxs->{x} = $info->{x_coord} if ($maxs->{x} < $info->{x_coord});
    $maxs->{y} = $info->{y_coord} if ($maxs->{y} < $info->{y_coord});
    $maxs->{z} = $info->{z_coord} if ($maxs->{z} < $info->{z_coord});
    1;
}

#MAIN;

# the output needs to be different depending on the desired topology
# the topology of the network is not included in the CrayXT db file
# and so must be given by the user
my $topology = "hdtorus"; #default to hdtorus
use Getopt::Long;
GetOptions('topology|t=s' => \$topology);


# currently SST/Macro doesn't strip out comments, so we don't emit them.
my $print_comments = 1;
my $mesh_info = {};
my $mesh_max = {'x' => 0, 'y' => 0, 'z' => 0};
my $switch_counts = {};
my $node_count = 0;
while(my $line = <>) {
    chomp($line);
    next unless ($line =~ /^cpu/);
    ++$count;
    my $node_info = get_node_info($line);
    my $catagories = build_key($node_info);
    update_node_list($print_comments,$mesh_info,$catagories,$node_info,$switch_counts,$topology);
    update_max($mesh_max,$node_info);
}

if ($mash_max{x} > 100) {
    print {STDERR} "Node maps with more than 100 nodes in X direction are not allowed.\n";
}    

#the parser now expects to find the number of nodes and the number of indices defining a node
my $ncoords = 4;
if ($topology eq "torus3") {
    $ncoords = 6;
}

printf "%d %d\n", $count, $ncoords; 
output($print_comments,$mesh_max,$mesh_info);

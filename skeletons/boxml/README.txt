Boxml
=====

Boxml is a skeleton app for the macroscale components of the Structural
Simulation Toolkit (SST).  XML files describing data "boxes" and
communication/computation events which occur during AMR computations
using the Boxlib library are used to drive macroscale architecture simulations.

Requirements
------------

o A working SST/macro installation

Building
--------

Make sure that the sstmacro compiler wrappers (sst++/sstcc) are found in your path.
All configuration information is obtained from the SST/macro installation, and a simple
invocation of 'make' should build the 'runboxml' executable.

Options
-------

-DBOXML_STD_UNORDERED can be optionally appended to CXX_FLAGS in Makefile
to enable use of std::unordered_map for Boxml's data structures.  With no
appended flag, the default is to use boost::unordered_map.  Use of
std::unordered_map requires compiling SST/macro with the C++11 standard
enabled.

Example
-------

A small example trace and macroscale SST parameter file is included in the 
examples directory.  After building, the example can be run from this 
directory with the following command:

./runboxml -f ./example/parameters.ini


TinyXML-2
=========

Boxml uses the TinyXML-2 utility for xml file parsing.  Please see
README-tinyxml2.txt for details including licensing information.

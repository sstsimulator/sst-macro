---
title: Manual for SST-Macro 9.0.x
published: true
category: SSTDocumentation
---


# SST/macro 9.0 User's Manual

![](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/sstlogo.png) 







 

# Table of Contents
   - [Chapter 1: Introduction](#sec:intro)
      - [Section 1.1: Overview](#sec:intro:overview)
   - [Chapter 2: Building and Running SST/macro](#chapter:building)
      - [Section 2.1: Build and Installation of SST-macro](#sec:buildinstall)
         - [2.1.1: Downloading](#subsec:build:downloading)
         - [2.1.2: Dependencies](#subsec:build:dependencies)
         - [2.1.3: Configuration and Building](#subsec:build:configure)
            - [Build SST core](#subsec:buildSSTCore)
            - [Build SST/macro element library](#subsec:buildElementLib)
         - [2.1.4: Post-Build](#subsec:postbuild)
         - [2.1.5: GNU pth for user-space threading: DEPRECATED](#subsubsec:pth)
         - [2.1.6: fcontext](#subsubsec:fcontext)
         - [2.1.7: Known Issues](#subsec:build:issues)
      - [Section 2.2: Building DUMPI](#sec:building:dumpi)
         - [2.2.1: Known Issues](#subsubsec:building:dumpi:issues)
      - [Section 2.3: Building Clang source-to-source support](#sec:buildingClang)
         - [2.3.1: Building Clang libTooling](#subsec:buildingClanglibTooling)
            - [The Easy Way: Mac OS X](#subsubsec:libToolingOSX)
            - [The Hard Way](#subsubsec:libTooling)
      - [Section 2.4: Building with OTF2 (Beta)](#sec:buildingOtf2)
      - [Section 2.5: Running an Application](#sec:building:running)
         - [2.5.1: SST Python Scripts](#subsec:SSTPythonScripts)
         - [2.5.2: Building Skeleton Applications](#sec:tutorial:runapp)
         - [2.5.3: Makefiles](#subsec:tutorial:makefiles)
         - [2.5.4: Command-line arguments](#subsec:tutorial:cmdline)
      - [Section 2.6: Parallel Simulations in Standalone Mode](#sec:PDES)
         - [2.6.1: Distributed Memory Parallel](#subsec:mpiparallel)
         - [2.6.2: Shared Memory Parallel](#subsec:parallelopt)
         - [2.6.3: Warnings for Parallel Simulation](#subsec:parallelwarn)
      - [Section 2.7: Debug Output](#sec:dbgoutput)
   - [Chapter 3: Basic Tutorials](#chapter:tutorials)
      - [Section 3.1: SST/macro Parameter files](#sec:parameters)
         - [3.1.1: Parameter Namespace Rules](#subsec:parameterNamespace)
         - [3.1.2: Initial Example](#subsec:initialExample)
      - [Section 3.2: Abstract Machine Models](#sec:amm)
         - [3.2.1: Common Parameters](#subsec:commonParams)
         - [3.2.2: AMM1](#subsec:ammOne)
         - [3.2.3: AMM2](#subsec:amm2)
         - [3.2.4: AMM3](#subsec:ammThree)
      - [Section 3.3: Network Topologies and Routing](#sec:tutorial:topology)
         - [3.3.1: Topology](#subsec:tutorial:topology)
         - [3.3.2: Routing](#subsec:tutorial:routing)
      - [Section 3.4: Network Model](#sec:tutorial:networkmodel)
         - [3.4.1: Analytic Models: MACRELS](#subsec:tutorial:macrels)
         - [3.4.2: Packet Models: PISCES](#subsec:tutorial:pisces)
            - [PISCES simple model](#subsubsec:tutorial:simplePisces)
            - [PISCES cut-through model](#subsubsec:tutorial:cutThroughPisces)
         - [3.4.3: Flow](#subsec:tutorial:flow)
      - [Section 3.5: Basic MPI Program](#sec:tutorial:basicmpi)
      - [Section 3.6: Launching, Allocation, and Indexing](#sec:tutorial:launchetc)
         - [3.6.1: Launch Commands](#subsec:tutorial:launch)
         - [3.6.2: Allocation Schemes](#subsec:tutorial:allocation)
            - [Indexing Schemes](#subsec:tutorial:indexing)
      - [Section 3.7: Discrete Event Simulation](#sec:tutorial:des)
      - [Section 3.8: Using DUMPI](#sec:tutorial:dumpi)
         - [3.8.1: Building DUMPI](#subset:dump:build)
         - [3.8.2: Trace Collection](#subsec:dumpi:tracecollection)
         - [3.8.3: Trace Replay](#subsec:dumpi:tracereplay)
      - [Section 3.9: Using Score-P and OTF2 (Beta)](#sec:tutorial:otf)
         - [3.9.1: Trace Collection](#subsec:otf:traceCollection)
         - [3.9.2: Trace Replay](#subsec:otf:traceReplay)
         - [3.9.3: Trace Emission](#subsec:otf:traceEmission)
      - [Section 3.10: Call Graph Visualization](#sec:tutorials:callgraph)
      - [Section 3.11: Spyplot Diagrams](#sec:tutorials:spyplot)
      - [Section 3.12: Fixed-Time Quanta Charts](#sec:tutorials:ftq)
      - [Section 3.13: Network Statistics](#sec:tutorials:packetStats)
         - [3.13.1: Message Size Histogram](#subsec:messageSizeHistogram)
         - [3.13.2: Congestion Delay Histogram](#subsec:congestionDelayHistogram)
         - [3.13.3: Congestion Spyplot and Multi-stats](#subsec:congestionSpyplot)
   - [Chapter 4: Topologies](#chapter:topologies)
      - [Section 4.1: Torus](#subsec:tutorial:hypercube)
      - [Section 4.2: Hypercube](#subsec:tutorial:hypercube)
         - [4.2.1: Allocation and indexing](#subsec:hypercube:allocation)
         - [4.2.2: Routing](#subsec:hypercube:routing)
      - [Section 4.3: Fat Tree](#sec:tutorial:fattree)
         - [4.3.1: Switch Crossbar Bandwidth Scaling](#subsec:fattree:xbarbw)
         - [4.3.2: Routing](#subsec:fattree:routing)
      - [Section 4.4: Cascade](#sec:tutorial:cascade)
         - [4.4.1: Allocation and indexing](#subsec:cascade:allocatoin)
         - [4.4.2: Routing](#subsec:cascade:routing)
   - [Chapter 5: External Applications and Skeletonization](#chap:appsAndSkeletonization)
      - [Section 5.1: Basic Application porting](#sec:skel:basic)
         - [5.1.1: Loading external skeletons with the standalone core](#subsec:externalAppStandalone)
      - [Section 5.2: Auto-skeletonization with Clang](#sec:autoSkeletonization)
         - [5.2.1: Skeletonization Issues](#subsec:skeletonIssues)
      - [Section 5.3: Process Encapsulation](#sec:processEncapsulation)
   - [Chapter 6: Clang Source-to-Source Auto-Skeletonization via Pragmas](#clangTutorial)
      - [Section 6.1: Memoization pragmas](#sec:memoization)
         - [6.1.1: pragma sst implicit\_state X(Y) ...](#subsec:implicitStates)
      - [Section 6.2: Fortran](#subsec:issues:fortran)
   - [Chapter 7: Detailed Parameter Listings](#chapter:parameters)
      - [Section 7.1: Global namespace](#sec:globalParams)
      - [Section 7.2: Namespace "topology"](#sec:topologyParams)
      - [Section 7.3: Namespace "node"](#sec:nodeParams)
         - [7.3.1: Namespace "node.nic"](#subsec:node:nic:Params)
            - [Namespace "node.nic.delay\_histogram"](#subsubsec:node:nic:delayHistogram:Params)
            - [Namespace "node.nic.congestion\_spyplot"](#subsubsec:node:nic:congestionSpyplot:Params)
            - [Namespace "node.nic.traffic\_matrix"](#subsubsec:node:nic:trafficMatrix:Params)
            - [Namespace "node.nic.local\_bytes\_sent"](#subsubsec:node:nic:localSent:Params)
            - [Namespace "node.nic.global\_bytes\_sent"](#subsubsec:node:nic:globalSent:Params)
            - [Namespace "node.nic.message\_size\_histogram"](#subsubsec:node:nic:sizeHistogram:Params)
            - [Namespace "node.nic.ejection"](#subsubsec:node:nic:ejection:Params)
            - [Namespace ``node.nic.injection"](#subsubsec:node:nic:injection:Params)
         - [7.3.2: Namespace ``node.memory"](#subsec:node:memory:Params)
         - [7.3.3: Namespace "node.os"](#subsec:node:os:Params)
            - [Namespace ``node.os.call\_graph"](#subsubsec:node:os:callGraph:Params)
            - [Namespace ``node.os.ftq"](#subsubsec:node:os:ftq:Params)
         - [7.3.4: Namespace ``node.proc"](#subsec:node:proc:Params)
      - [Section 7.4: Namespace "mpi"](#sec:mpi:Params)
         - [7.4.1: Namespace ``mpi.queue"](#subsec:mpi:queue:Params)
      - [Section 7.5: Namespace "switch"](#subsec:switch:Params)
         - [7.5.1: Namespace "switch.router"](#subsec:switch:router:Params)
         - [7.5.2: Namespace "switch.output\_buffer"](#subsec:switch:outputBuffer:Params)
            - [Namespace ``switch.output\_buffer.delay\_histogram"](#subsubsec:switch:outputBuffer:delayHistogram:Params)
            - [Namespace "switch.output\_buffer.byte\_hops"](#subsubsec:switch:outputBuffer:delayHistogram:Params)
         - [7.5.3: Namespace "switch.xbar"](#sec:switch:outputBuffer:delayHistogram:Params)
            - [Namespace ``switch.xbar.bytes\_sent"](#subsubsec:switch:xbar:bytesSent:Params)
         - [7.5.4: Namespace "switch.link"](#subsec:switch:link:Params)
         - [7.5.5: Namespace "switch.ejection"](#subsec:switch:ejection:Params)
      - [Section 7.6: Namespace "appN"](#sec:appN:Params)



## Chapter 1: Introduction<a name="sec:intro"></a>



### Section 1.1: Overview<a name="sec:intro:overview"></a>



The SST-macro software package provides a simulator for large-scale parallel computer architectures. 
It permits the coarse-grained study of distributed-memory applications. 
The simulator is driven from either a trace file or skeleton application. 
The simulator architecture is modular, allowing it to easily be extended with additional network models, 
trace file formats, software services, and processor models.

Simulation can be broadly categorized as either off-line or on-line.
Off-line simulators typically first run a full parallel application on a real machine,
recording certain communication and computation events to a simulation trace.
This event trace can then be replayed post-mortem in the simulator.
Most common are MPI traces which record all MPI events, and
SST-macro provides the DUMPI utility ([3.8](#sec:tutorial:dumpi)) for collecting and replaying MPI traces. 
Trace extrapolation can extend the usefulness of off-line simulation by estimating large or untraceable system scales without   
having to collect a trace, but it is limited.

We turn to on-line simulation when the hardware or applications parameters need to change.
On-line simulators instead run real application code, allowing native C/C++/Fortran to be compiled directly into the simulator.
SST-macro intercepts certain function calls, estimating how much time passes rather than actually executing the function.
In MPI programs, for example, calls to MPI\_Send are linked to the simulator instead of passing to the real MPI library.
If desired, SST-macro can actually be a full MPI emulator, delivering messages between ranks and replicating the behavior of a full MPI implementation.

Although SST-macro supports both on-line and off-line modes, on-line simulation is encouraged because
event traces are much less flexible, containing a fixed sequence of events.
Application inputs and number of nodes cannot be changed. 
Without a flexible control flow, it also cannot simulate dynamic behavior like load balancing or faults.
On-line simulation can explore a much broader problem space since they evolve directly in the simulator.

For large, system-level experiments with thousands of network endpoints, high-accuracy cycle-accurate simulation is not possible,
or at least not convenient.
Simulation requires coarse-grained approximations to be practical.
SST-macro is therefore designed for specific cost/accuracy tradeoffs.
It should still capture complex cause/effect behavior in applications and hardware, but be efficient enough to simulate at the system-level. 
For speeding up simulator execution, we encourage skeletonization, discussed further in Chapter [5](#chap:appsAndSkeletonization). 
A high-quality skeleton is an application model that reproduces certain characteristics with only limited computation.  
We also encourage uncertainty quantification (UQ) for validating simulator results.
Skeletonization and UQ are the two main elements in the "canonical" SST-macro workflow (Figure [1](#fig:workflow)).


![Figure 1: SST/macro workflow.](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/workflow.png) 

*Figure 1: SST/macro workflow.*



Because of its popularity, MPI is one of our main priorities in providing programming model support.  
Some MPI-3 functions and MPI one-sided functions are not implemented.
This will lead to compile errors with an obvious "not implement" compiler message.

\section{Preview of Things to Come}
Suppose you have the basic MPI application below that executes a simple send/recv operation.
One could use `mpicc` or `mpic++` to compile and run as an actual MPI program.
This requires spawning all the processes and running them in parallel.
Suppose, however, you wanted to simulate an entire MPI job launch within a single process.
This might prove very useful for debugging since you could just run GDB or Valgrind on a single process.
It might take a while, but for small runs (16 ranks or so) you could debug right on your laptop the same you do for a serial program.

````
int size = atoi(argv[1]);
if (rank == 0){
 int partner = 1;
  MPI_Send(buffer, size, MPI_INT, partner, tag, MPI_COMM_WORLD);
} else {
  int partner = 0;
  MPI_Recv(buffer, size, MPI_INT, partner, tag, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
}
MPI_Barrier(MPI_COMM_WORLD);

if (rank == 0){
  printf("Rank 0 finished at t=%8.4f ms\n", MPI_Wtime()*1e3);
}
````

This is exactly the functionality that SST/macro provides.
Instead of using `mpic++`, you compile the code with {sst++}.
This modifies your code and intercepts MPI calls, running them through the simulator instead of an actual MPI implementation.
Your code will execute and run exactly the same, and your application won't even know the difference.
You now need a parameter file with information like:

````
node {
 app1 {
  name = send_recv
  launch_cmd = aprun -n 2
  argv = 20
 }
}
````
Rather than launching your code using `mpirun` or similar, you put all your command line parameters into a `parameters.ini` file and run:

````
shell> sstmac -f parameters.ini
````
The simulator then executes your application exactly as if you had been a real system and run:

````
shell> aprun -n 2 ./send_recv 20
````
Things get more complicated when you bring skeletonization into play.
The use case above is emulation, exactly reproducing MPI functionality.
In skeletonization or simulation, SST/macro will mimic as closely as possible the original application,
but avoids as much computation and as much memory allocation as possible.
This allows packing in as many simulated (virtual) MPI ranks as possible into your single `sstmac` process.

\section{What To Expect In The User's Manual}
This user's manual is mainly designed for those who wish to perform experiments with new applications using existing hardware models.
This has been the dominant use case and we therefore classify those doing application experiments as "users" and those making new hardware models "developers."
Getting applications to run in SST/macro should be very straightforward and requires no knowledge of simulator internal code.
Making new hardware models is much more in depth and requires learning some basics of core simulator code.
Those interested in making new hardware models should consult the developer's manual in the top-level source directory.



## Chapter 2: Building and Running SST/macro<a name="chapter:building"></a>



### Section 2.1: Build and Installation of SST-macro<a name="sec:buildinstall"></a>




#### 2.1.1: Downloading<a name="subsec:build:downloading"></a>



SST-macro is available at https://github.com/sstsimulator/sst-macro.
You can download the git repository directly:

````
shell> git clone https://github.com/sstsimulator/sst-macro.git
````
or for ssh

````
shell> git clone ssh://git@github.com/sstsimulator/sst-macro.git
````
or you can download a release tarball from https://github.com/sstsimulator/sst-macro/releases.

#### 2.1.2: Dependencies<a name="subsec:build:dependencies"></a>


The most critical change is that C++11 is now a strict prerequisite. 
Workarounds had previously existed for older compilers. 
These are no longer supported.
The following are dependencies for SST-macro.


-   (optional) Git is needed in order to clone the source code repository, but you can also download a tar (Section [2.1.1](#subsec:build:downloading)).
-   Autoconf: 2.68 or later
-   Automake: 1.11.1 or later
-   Libtool: 2.4 or later
-   A C/C++ compiler is required with C++11 support.  gcc >=4.8.5 and clang >= 3.7 are known to work.
-   (optional) OTF2: 2.0 or later for OTF2 trace replay.
-   (optional) VTK 8.1 or later for creating Exodus files for traffic visualization
-   (optional) Paraview 5.0 or greater for visualizing Exodus files
-   (optional) Doxygen and Graphviz are needed to build the source code documentation.
-   (optional) KCacheGrind or QCacheGrind to display call graphs
-   (optional) Clang development libraries to enable SST source-to-source compiler

#### 2.1.3: Configuration and Building<a name="subsec:build:configure"></a>



SST/macro is an SST element library, proving a set of simulation components that run on the main SST core.  
The SST core provides the parallel discrete event simulation manager that manages time synchronization and sending events in serial, MPI parallel, multi-threaded, or MPI + threaded mode.  
The core does not provide any simulation components like node models, interconnect models, MPI trace readers, etc.  
The actual simulation models are contained in the element library.  

The SST core is a standalone executable that dynamically loads shared object files containing the element libraries.  
For many element libraries, a Python input file is created that builds and connects the various simulation components.  
For maximum flexibility, this will become the preferred mode.  
However, SST/macro has historically had a text-file input `parameters.ini` that configures the simulation.  
To preserve that mode for existing users, a wrapper Python script is provided that processes SST/macro input files.  
SST/macro can also be compiled in standalone mode that uses its own simulation core.

The workflow for installing and running on the main SST core is:

-   Build and install SST core
-   Build and install the SST/macro element library `libmacro.so`
-   Make sure paths are properly configured for `libmacro.so` to be visible to the SST core (`SST_LIB_PATH`)
-   Run the `pysstmac` wrapper Python script that runs SST/macro-specific parameters OR
-   Write a custom Python script

The workflow for installing and running on the standalone SST/macro core:

-   Build and install SST/macro standalone to generate `sstmac` executable
-   Run `sstmac` with `*.ini` parameter files

##### Build SST core<a name="subsec:buildSSTCore"></a>


The recommended mode for maximum flexibility is to run using the SST core downloadable from http://sst-simulator.org/SSTPages/SSTMainDownloads.
Building and installing sets up the discrete event simulation core required for all SST elements.

##### Build SST/macro element library<a name="subsec:buildElementLib"></a>


If using the repo (not a release tarball), go to the top-level source directory and run:
````
top-level-src> ./bootstrap.sh
````
This sets up the configure script. For builds, we recommend building outside the source tree:

````
sst-macro> mkdir build
sst-macro> cd build
sst-macro/build> ../configure --prefix=$PATH_TO_INSTALL --with-sst-core=$PATH_TO_SST_CORE CC=MPICC CXX=MPICXX
````
`PATH_TO_SST_CORE` should be the prefix install directory used when installing the core.  
The MPI compilers should be the same compilers used for building SST core.

SST/macro can still be built in standalone mode for a select set of features that have not been fully migrated to the SST core.  
The installation and running are the same for both modes - simply remove the `--with--sst-core` parameter.  
A complete list of options for building can be seen by running `../configure --help`.   Some common options for both standalone and main SST core include:


-   --(dis|en)able-graphviz : Enables the collection of simulated call graphs, which can be viewed with graphviz.
Disabled by default.
-   --(dis|en)able-custom-new : Memory is allocated in larger chunks in the simulator, which can speed up large simulations.
-   --(dis|en)able-otf2[=location]: Enable OTF2 trace replay, requires a path to OTF2 installation.
-   --with-clang[=location]: Enable Clang source-to-source tools by pointing to Clang development libraries

Once configuration has completed, printing a summary of the things it found, simply type `make`.  

#### 2.1.4: Post-Build<a name="subsec:postbuild"></a>



If the build did not succeed open an issue on the github page at https://github.com/sstsimulator/sst-macro/issues or contact SST-macro support for help (sst-macro-help@sandia.gov).

If the build was successful, it is recommended to run the range of tests to make sure nothing went wrong.  
To do this, and also install SST-macro  to the install path specified during installation, run the following commands:

````
sst-macro/build> make check
sst-macro/build> make install
sst-macro/build> make installcheck
````
Make check runs all the tests we use for development, which checks all functionality of the simulator.  
Make installcheck compiles some of the skeletons that come with SST-macro, linking against the installation.  


Important:  Applications and other code linking to SST-macro use Makefiles that use the sst++/sstcc compiler wrappers
that are installed there for convenience to figure out where headers and libraries are.  When making your skeletons and components, make sure your path is properly configured.


#### 2.1.5: GNU pth for user-space threading: DEPRECATED<a name="subsubsec:pth"></a>


By default, Linux usually ships with `ucontext` which enables user-space threading.
Mac OS X previously required an extra library be installed (GNU pth).
These are no longer required and are deprecated in favor of fcontext,
which is now integrated with the SST/macro distribution (see below).

For those still wishing to use pth, GNU pth is easy to download and install from source.
Even easier is MacPorts. 

````
shell> sudo port install pth
````

MacPorts installed all libraries to `/opt/local`. 
When configuring, simply add `--with-pth=\$PATH_TO_PTH` as an argument.
For MacPorts installation, this means configuring SST/macro with:

````
../configure --with-pth=/opt/local
````

#### 2.1.6: fcontext<a name="subsubsec:fcontext"></a>


The fcontext library for user-space threading is now integrated directly with the SST/macro distribution.
This provides much greater performance than GNU pth or standard Linux ucontext.
Users may see as much as a 20\
fcontext should be activated by default. To force activation fcontext, you can either set the environment variable:

````
SSTMAC_THREADING=fcontext
````
or in the parameter file specify (more details in Section [3.1](#sec:parameters)):

````
node {
 os {
  context = fcontext
 }
}
````

#### 2.1.7: Known Issues<a name="subsec:build:issues"></a>





-   Any build or runtime problems should be reported to sst-macro-help@sandia.gov.  We try to respond as quickly as possible.
-   make -j: When doing a parallel compile dependency problems can occur.  
There are a lot of inter-related libraries and files.  
Sometimes the Makefile dependency tracker gets ahead of itself and you will get errors about missing libraries and header files.
If this occurs, restart the compilation.  If the error vanishes, it was a parallel dependency problem.
If the error persists, then it's a real bug.
-   GNU pth: For Mac, make sure to follow directions in [2.1.5](#subsubsec:pth) to ensure pth is correctly installed.
-   Ubuntu: The Ubuntu linker performs too much optimization on dynamically linked executables.
Some call it a feature.  I call it a bug.
In the process it throws away symbols it actually needs later. The build system should automatically fix Ubuntu flags.
If still having issues, make sure that '-Wl,--no-as-needed' is given in LDFLAGS.

The problem occurs when the executable depends on libA which depends on libB.
The executable has no direct dependence on any symbols in libB.
Even if you add `-lB` to the `LDFLAGS` or `LDADD` variables,
the linker ignores them and throws the library out.
It takes a dirty hack to force the linkage.
If there are issues, contact the developers at sst-macro-help@sandia.gov and report the problem.

### Section 2.2: Building DUMPI<a name="sec:building:dumpi"></a>



By default, DUMPI is configured and built along with SST/macro with support for reading and parsing DUMPI traces, known as libundumpi.  
DUMPI binaries and libraries are also installed along with everything for SST-macro during make install.   
DUMPI can be used as its own library within the SST-macro source tree by changing to `sst-macro/sst-dumpi`, 
where you can change its configuration options.  

DUMPI can also be used as stand-alone tool (\eg~for simplicity if you're only tracing). 
To get DUMPI by itself, either copy the `sstmacro/dumpi` directory somewhere else or visit https://github.com/sstsimulator/sst-dumpi and follow similar instructions for obtaining SST-macro.

To see a list of configuration options for DUMPI, run `./configure --help`.  
If you're trying to configure DUMPI for trace collection, use `--enable-libdumpi`.
Your build process might look like this (if you're building in a separate directory from the dumpi source tree) :

````
sst-dumpi/build> ../configure --prefix=/path-to-install --enable-libdumpi
sst-dumpi/build> make
sst-dumpi/build> sudo make install
````

#### 2.2.1: Known Issues<a name="subsubsec:building:dumpi:issues"></a>




-   When compiling on platforms with compiler/linker wrappers, e.g. ftn (Fortran) and CC (C++) compilers 
at NERSC, the libtool configuration can get corrupted.  The linker flags automatically added by the 
wrapper produce bad values for the predeps/postdeps variable in the libtool script in the top 
level source folder.  When this occurs, the (unfortunately) easiest way to fix this is to manually modify
the libtool script.  Search for predeps/postdeps and set the values to empty.
This will clear all the erroneous linker flags.  The compilation/linkage should still work since 
all necessary flags are set by the wrappers.


### Section 2.3: Building Clang source-to-source support<a name="sec:buildingClang"></a>



To enable Clang source-to-source support it is not sufficient to have a Clang compiler.  You have to install a special libTooling library for Clang.

#### 2.3.1: Building Clang libTooling<a name="subsec:buildingClanglibTooling"></a>



##### The Easy Way: Mac OS X<a name="subsubsec:libToolingOSX"></a>


Using MacPorts on OS X, it is trivial to obtain a Clang installation that includes libTooling:

````
port install clang-devel
````

MacPorts will place the Clang compilers in `/opt/local/bin`.  Enable the devel version of Clang with:

````
port select --set clang mp-clang-devel
````

The Clang libraries will be placed into `/opt/local/libexec/llvm-devel/lib`, so the appropriate option to the sst-macro configure script is `--with-clang=/opt/local/libexec/llvm-devel`.

##### The Hard Way<a name="subsubsec:libTooling"></a>


For operating systems other than OS X, building Clang support has a few steps (and takes quite a while to build), but is straightforward.
Instead of having an all-in-one tarball, you will have to download several different components. You can install more if you want build libc++, but these are not required.
Obtain the following from http://releases.llvm.org/download.html.


-   LLVM source code
-   Clang source code
-   libc++ source code
-   libc++abi source code
-   compiler-rt source code
-   (optional, not recommended) OpenMP source code

Setting up the folders can be done automatically using the `setup-clang` script in `bin/tools` folder in sst-macro. Put all of downloaded tarballs in a folder, e.g. `clang-llvm`. Then run `setup-clang` in the directory. 
It will automatically place files where LLVM needs them.
LLVM is the "driver" for the entire build. Everything else is a subcomponent. 
The setup script places each tarball in the following subfolders of the main LLVM folder


-   tools/clang
-   projects/compiler-rt
-   projects/libcxx
-   projects/libcxxabi
-   projects/openmp

Using CMake (assuming you are in a build subdirectory of the LLVM tree), you would run the script below to configure.
You no longer need to use Clang to build Clang. 
For the most stable results, though, you should a pre-existing Clang compiler to build the Clang development libraries.

````
cmake ../llvm \
  -DCMAKE_CXX_COMPILER=clang++ \
  -DCMAKE_C_COMPILER=clang \
  -DCMAKE_CXX_FLAGS="-O3" \
  -DCMAKE_C_FLAGS="-O3" \
  -DCMAKE_INSTALL_PREFIX=$install
````

To build a complete LLVM/Clang run:

````
cmake ../llvm \
  -DCMAKE_CXX_COMPILER=clang++ \
  -DCMAKE_C_COMPILER=clang \
  -DCMAKE_CXX_FLAGS="-O3" \
  -DCMAKE_C_FLAGS="-O3" \
  -DLLVM_ENABLE_LIBCXX=ON \
  -DLLVM_TOOL_COMPILER_RT_BUILD=ON \
  -DLLVM_TOOL_LIBCXXABI_BUILD=ON \
  -DLLVM_TOOL_LIBCXX_BUILD=ON \
  -DCMAKE_INSTALL_PREFIX=$install
````

On some systems, linking Clang might blow out your memory. If that is the case, you have to set `LD=ld.gold` for the linker.
Run `make install`. The libTooling library will now be available at the `\$install` location.

Any compiler used for SST (g++, icpc, clang++) can generally be mixed with most versions of the libtooling source-to-source library.
NOTE: The same compiler used to build SST must have been used to build the libtooling library.
However, the table below contains versions that are recommended or approved and which combinations are untested (but may work).


| Compiler to build SST | Libtooling version |
|-----------------------|--------------------|
| Clang 4,5,6 | 4,5,6 |
| Clang 7,8 | 7,8 |
| GCC 4.8-6 | 4-7 |
| GCC 7- | ? |
| ? | 8 |

\subsection{Building SST/macro with Clang}
Now that clang is installed, you only need to add the configure flag `--with-clang` pointing it to the install location from above.
You must use the same Clang compiler to build SST that you used to build libTooling.

````
../configure CXX=clang++ CC=clang --with-clang=$install
````

Clang source-to-source support will now be built into the `sst++` compiler. 
If Clang development libraries are available in the default system path (as is often the case with LLVM models, e..g `module load llvm`),
then you can just put `--with-clang`.

### Section 2.4: Building with OTF2 (Beta)<a name="sec:buildingOtf2"></a>


OTF2 is a general purpose trace format with specialized callbacks for the MPI API. OTF2 traces are generated by programs compiled with Score-P compiler wrappers. SST/macro 8.1 supports OTF2 trace replay and OTF2 trace generation when configured with 

````
./configure --enable-otf2=<OTF2-root>
````
where the OTF2 root is the installation prefix for a valid OTF2 build. OTF2 can be obtained from the Score-P project at {http://www.vi-hps.org/projects/score-p}.
Detailed build and usage instructions can be found on the website.




### Section 2.5: Running an Application<a name="sec:building:running"></a>


#### 2.5.1: SST Python Scripts<a name="subsec:SSTPythonScripts"></a>



Full details on building SST Python scripts can be found at http://sst-simulator.org.  To preserve the old parameter format in the near-term, SST/macro provides the `pysstmac` script:

````
export SST_LIB_PATH=$SST_LIB_PATH:$SSTMAC_PREFIX/lib

options="$@"
$SST_PREFIX/bin/sst$SSTMAC_PREFIX/include/python/default.py --model-options="$options"
````

The script configures the necessary paths and then launches with a Python script `default.py`.  Interested users can look at the details of the Python file to understand how SST/macro converts parameter files into a Python config graph compatible with SST core.
Assuming the path is configured properly, users can run

````
shell>pysstmac -f parameters.ini
````
with a properly formatted parameter file. If running in standalone mode, the command would be similar (but different).

````
shell>sstmac -f parameters.ini
````
since there is no Python setup involved.

#### 2.5.2: Building Skeleton Applications<a name="sec:tutorial:runapp"></a>



To demonstrate how an external skeleton application is run in SST-macro, we'll use a very simple send-recv program located in `skeletons/sendrecv`.
We will take a closer look at the actual code in Section [3.5](#sec:tutorial:basicmpi).
After SST-macro has been installed and your PATH variable set correctly, for standalone core users can run:

````
sst-macro> cd skeletons/sendrecv
sst-macro/skeletons/sendrecv> make
sst-macro/skeleton/sendrecv> sstmac -f parameters.ini --exe=./runsstmac
````

You should see some output that tells you 1) the estimated total (simulated) runtime of the simulation, and 
2) the wall-time that it took for the simulation to run.  
Both of these numbers should be small since it's a trivial program. 
This is how simulations generally work in SST-macro: you build skeleton code and link it with the simulator to produce an importable library.  
The importable library contains hooks for loading a skeleton app into the simulator.
NOTE: `runsstmac` appears to be an executable, but is actually built as a shared library. 
If using a regular compiler (e.g. gcc), the Makefile would produce an executable `runsstmac`.
To ensure that building apps for SST require no modification to an existing build system,
SST simple builds a shared library `runsstmac` rather than requiring renaming to the standard convention
`librunsstmac.so`.

#### 2.5.3: Makefiles<a name="subsec:tutorial:makefiles"></a>


We recommend structuring the Makefile for your project like the one seen in `skeletons/sendrecv/Makefile` :

````
TARGET := runsstmac
SRC := $(shell ls *.c) 

CXX :=      $(PATH_TO_SST)/bin/sst++
CC :=        $(PATH_TO_SST)/bin/sstcc
CXXFLAGS := ...
CPPFLAGS := ...
LIBDIR :=  ...
PREFIX :=   ...
LDFLAGS :=  -Wl,-rpath,$(PREFIX)/lib
...
````
The SST compiler wrappers `sst++` and `sstcc` automagically configure and map the code for simulation. 

#### 2.5.4: Command-line arguments<a name="subsec:tutorial:cmdline"></a>



There are few common command-line arguments with SST-macro, listed below


-   -h/--help: Print some typical help info
-   -f [parameter file]: The parameter file to use for the simulation.  
This can be relative to the current directory, an absolute path, or the name of a pre-set file that is in sstmacro/configurations 
(which installs to /path-to-install/include/configurations, and gets searched along with current directory).
-   --dumpi: If you are in a folder with all the DUMPI traces, you can invoke the main `sstmac` executable with this option.  This replays the trace in a special debug mode for quickly validating the correctness of a trace.
-   --otf2: If you are in a folder with all the OTF2 traces, you can invoke the main `sstmac` executable with this option.  This replays the trace in a special debug mode for quickly validating the correctness of a trace.
-   -d [debug flags]: A list of debug flags to activate as a comma-separated list (no spaces) - see Section [2.7](#sec:dbgoutput)
-   -p [parameter]=[value]: Setting a parameter value (overrides what is in the parameter file)
-   -t [value]: Stop the simulation at simulated time [value]
-   -c: If multithreaded, give a comma-separated list (no spaces) of the core affinities to use - see Section [2.6.2](#subsec:parallelopt)

### Section 2.6: Parallel Simulations in Standalone Mode<a name="sec:PDES"></a>



SST-macro supports running parallel discrete event simulation (PDES) in distributed memory (MPI), threaded shared memory (pthreads) and hybrid (MPI+pthreads) modes.  Running these in standalone mode will be discouraged as parallel simulations should use the unified SST core. However, near-term, hybrid modes and other optimizations are not fully supported in the unified SST core. The standalone core may still be required for certain cases.

#### 2.6.1: Distributed Memory Parallel<a name="subsec:mpiparallel"></a>


Configure will automatically check for MPI.
Your configure should look something like:

````
sst-macro/build> ../configure CXX=mpicxx CC=mpicc ...
````
With the above options, you can just compile and go.
SST-macro is run exactly like the serial version, but is spawned like any other MPI parallel program.
Use your favorite MPI launcher to run, e.g. for OpenMPI

````
mysim> mpirun -n 4 sstmac -f parameters.ini
````
or for MPICH

````
mysim> mpiexec -n 4 sstmac -f parameters.ini
````

Even if you compile for MPI parallelism, the code can still be run in serial with the same configuration options.
SST-macro will notice the total number of ranks is 1 and ignore any parallel options.
When launched with multiple MPI ranks, SST-macro will automatically figure out how many partitions (MPI processes) 
you are using, partition the network topology into contiguous blocks, and start running in parallel.   

#### 2.6.2: Shared Memory Parallel<a name="subsec:parallelopt"></a>


In order to run shared memory parallel, you must configure the simulator with the `--enable-multithread` flag.
Partitioning for threads is currently always done using block partitioning and there is no need to set an input parameter.
Including the integer parameter `sst_nthread` specifies the number of threads to be used (per rank in MPI+pthreads mode) in the simulation.
The following configuration options may provide better threaded performance.

-   `--enable-spinlock` replaces pthread mutexes with spinlocks.  Higher performance and recommended when supported.
-   `--enable-cpu-affinity` causes SST-macro to pin threads to specific cpu cores.  When enabled, SST-macro will require the
`cpu_affinity` parameter, which is a comma separated list of cpu affinities for each MPI task on a node.  SST-macro will sequentially
pin each thread spawned by a task to the next next higher core number.  For example, with two MPI tasks per node and four threads per MPI task,
`cpu_affinity = 0,4` will result in MPI tasks pinned to cores 0 and 4, with pthreads pinned to cores 1-3 and 5-7.
For a threaded only simulation `cpu_affinity = 4` would pin the main process to core 4 and any threads to cores 5 and up.
The affinities can also be specified on the command line using the `-c` option.
Job launchers may in some cases provide duplicate functionality and either method can be used.

#### 2.6.3: Warnings for Parallel Simulation<a name="subsec:parallelwarn"></a>



-   If the number of simulated processes specified by e.g. `aprun -n 100` does not match the number of nodes in the topology (i.e. you are not space-filling the whole simulated machine), parallel performance will suffer. SST-macro partitions nodes, not MPI ranks.


Parallel simulation speedups are likely to be modest for small runs.
Speeds are best with serious congestion or heavy interconnect traffic.
Weak scaling is usually achievable with 100-500 simulated MPI ranks per logical process.
Even without speedup, parallel simulation can certainly be useful in overcoming memory constraints.


### Section 2.7: Debug Output<a name="sec:dbgoutput"></a>


SST-macro defines a set of debug flags that can be specified in the parameter file to control debug output printed by the simulator.
To list the set of all valid flags with documentation, the user can run

````
bin> ./sstmac --debug-flags
````

which will output something like

````
mpi
        print all the basic operations that occur on each rank - only API calls are
        logged, not any implementation details
    router
        print all operations occurring in the router
     ....
````


To turn on debug output, add the following to the input file

````
debug = mpi
````
listing all flags you want separated by spaces.




## Chapter 3: Basic Tutorials<a name="chapter:tutorials"></a>





### Section 3.1: SST/macro Parameter files<a name="sec:parameters"></a>


A minimal parameter file setting up a 2D-torus topology is shown below. 
A detailed listing of parameter namespaces and keywords is given in Section [7](#chapter:parameters).
The preferred input files now use namespaces.

````
amm_model = amm1
congestion_model = LogP
node {
 #run a single mpi test
 app1 {
  indexing = block
  allocation = first_available
  launch_cmd = aprun -n8 -N1
  name = sstmac_mpi_testall
  argv =
  sendrecvMessage_size = 128
 }
 ncores = 1
 memory {
  model = simple
  bandwidth = 1GB/s
  latency = 10ns
 }
 proc {
  frequency = 1GHz
 }
 nic {
  injection {
   bandwidth = 1GB/s
   latency = 1us
  }
  model = simple
 }
}
switch {
 ejection {
  bandwidth = 1GB/s
  latency = 100ns
 }
 link {
  bandwidth = 1.0GB/s
   latency = 100ns
 }
}

topology {
 name = torus
 geometry = 4,4
}
````
The input file follows a basic syntax of `parameter = value`.  
Parameter names follow C++ variable rules (letters, numbers, underscore) while parameter values can contain spaces.  Trailing and leading whitespaces are stripped from parameters.
Comments can be included on lines starting with \#.


#### 3.1.1: Parameter Namespace Rules<a name="subsec:parameterNamespace"></a>


Periods denote nesting of parameter namespaces.
The parameter `node.memory.model` will be nested in namespace `memory` inside namespace `node`.
If inside a given namespace, SST-macro looks within that namespace first.
If unable to find it, the input parser moves through the namespace nesting to find the value.
This exactly follows C++ namespace rules with one important exception. 
The "global" namespace is reserved for special keywords.
Keywords in the global namespace are not visible to any components.


For example, consider the following:
````
node.model = simple
node.memory.model = pisces
````
If I am building the node's memory system, the initialization will look for `memory::model` inside namespace `node` first, returning the value `pisces`. 

The preferred syntax more closely resembles C++ namespace declarations. 
Namespaces are scoped using brackets \{\}:

````
node {
 model = simple
 memory {
   model = simple
   bandwidth = 1GB/s
   latency = 10ns
 }
}
````
Any line containing a single string with an opening \{ starts a new namespace.
A line containing only a closing \} ends the innermost namespace.
The syntax is not as flexible as C++ since the opening \{ must appear on the same line as the namespace and the closing \} must be on a line of its own.
A detailed listing of parameter namespaces and keywords is given in Section [7](#chapter:parameters).

#### 3.1.2: Initial Example<a name="subsec:initialExample"></a>


Continuing with the example above, we see the input file is broken into namespace sections. 
First, application launch parameters for each node must be chosen determining what application will launch, 
how nodes will be allocated, how ranks will be indexed, and finally what application will be run.  
Additionally, you must specify how many processes to launch and how many to spawn per node.  
We currently recommend using aprun syntax (the launcher for Cray machines), 
although support is being added for other process management systems.
SST-macro can simulate command line parameters by giving a value for `node.app1.argv`.

A network must also be chosen.  
In the simplest possible case, the network is modeled via a simple latency/bandwidth formula.  
For more complicated network models, many more than two parameters will be required. 
See [3.4](#sec:tutorial:networkmodel) for a brief explanation of SST-macro network congestion models. 
A topology is also needed for constructing the network.  
In this case we choose a 2-D 4X4 torus (16 switches).  The `topology.geometry` 
parameter takes an arbitrarily long list of numbers as the dimensions to the torus.

Finally, we must construct a node model.  
In this case, again, we use the simplest possible models for the node, 
network interface controller (NIC), and memory.  

Parameter files can be constructed in a more modular way through the `include` statement.  
An alternative parameter file would be:

````
include machine.ini
# Launch parameters
node {
 app1 {
  indexing = block
  allocation = first_available
  launch_cmd = aprun -n2 -N1
  name = user_mpiapp_cxx
  argv = 
  # Application parameters
  sendrecvMessage_size = 128
 }
}
````
where in the first line we include the file `machine.ini`.  
All network, topology, and node parameters would be placed into a `machine.ini` file.  
In this way, multiple experiments can be linked to a common machine.  
Alternatively, multiple machines could be linked to the same application by creating and including an `application.ini`.

Using the deprecated (non-namespace) parameters the file would be:

````
# Launch parameters
launch_indexing = block
launch_allocation = first_available
launch_cmd_app1 = aprun -n2 -N1
launch_app1 = user_mpiapp_cxx
launch_app1_argv = 
# Network parameters
network_bandwidth = 1.0GB/s
network_hop_latency = 100ns
# Topology - Ring of 4 nodes
topology_name = torus
topology_geometry = 4,4
# Node parameters
node_cores = 1
node_name = null
node_MemoryModel = null
nic_name = null
# Application parameters
sendrecvMessage_size = 128
````

All of these are special keywords in the global namespace that get expanded into parameters in a specific namespace. We really, really do not recommend using these deprecated parameters anymore.




### Section 3.2: Abstract Machine Models<a name="sec:amm"></a>



The preferred mode for usage of SST-macro will be through specifying parameters for well-defined abstract machine models.
This represents an intermediate-level mode that should cover the vast majority of use cases.
The highly configurable, detailed parameter files will remain valid but will represent advanced usage mode for corner cases.
The primary advantage of the abstract machine models is a uniform set of parameters regardless of the underlying congestion model or accuracy level (Pisces or LogGOPSim).
Each input file requires the usual set of software parameters given in [3.1](#sec:parameters).
For hardware parameters, two initial parameters are required and one is optional.

````
congestion_model = pisces
amm_model = amm1
accuracy_parameter = 1024
```` 

Here we indicate the congestion model to be used (the packet-flow) and the overall machine model (abstract machine model \#1).
Currently valid values for the congestion model are `pisces` (most accurate, slowest) and `simple` (least accurate, fastest),
but more congestion models should be supported in future versions.
Currently valid values for the abstract machine model are `amm1`, `amm2`, `amm3`, see details below. 
Another model, `amm4`, that adds extra detail to the NIC and switches is pending and should be available soon.
The details of individual abstract machine models are given in the following sections.
The optional accuracy parameter is less well-defined and the exact meaning varies considerably between congestion models.
In general, the accuracy parameter represents how coarse-grained the simulation is in bytes.
It basically corresponds to a packet-size. How many bytes are modeled moving through the machine separately at a time?
If the parameter is set to 8 bytes, e.g., that is essentially flit-level modeling.
If the parameter is set to 8192 bytes, e.g. that is very coarse-grained modeling which only really affects large messages.
If the parameter is set to 100-1000 bytes, e.g., that is fine-grained modeling on real packet sizes, but ignoring flit-level details.

#### 3.2.1: Common Parameters<a name="subsec:commonParams"></a>


The following parameters define the CPU and compute power of the node (independent of memory subsystem).
They are universal and are valid for all abstract machine models. 

````
node {
 model = simple
 nsockets = 4
 proc {
  frequency = 2.1ghz
  ncores = 24
 }
}
````

#### 3.2.2: AMM1<a name="subsec:ammOne"></a>




![Figure 2: AMM1: Used when focusing on network traffic only](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/amm/AMM1.png) 

*Figure 2: AMM1: Used when focusing on network traffic only*



This is simplest abstract machine model and incorporates three basic components (i.e. congestion points).
Each node has a memory subsystem and NIC (injection/ejection).
Once packets are injected, they traverse a series of network switches.
The memory, injection, and network are all defined by a bandwidth/latency parameter pair.

````
switch {
 link {
  bandwidth = 6GB/s
  latency = 100ns
 }
}

node {
 nic {
  injection {
   bandwidth = 10GB/s
   latency = 1us  
  }
 }
 memory {
  bandwidth = 10GB/s
  latency = 15ns
 }
}
````


The link latency is the latency required for a single packet to traverse one switch and hop to the next one in the network.
Thus, even in the most basic of network models, there is a still a notion of topology that affects the number of hops and therefore the latency.
To compute the total network network latency as one would observe in an MPI ping-ping benchmark, one would compute using the hop latency and the injection latency

lat = n(hops) * lat(hop) + 2*lat(inj)



This abstract machine model is a good place to start for getting a "lay of the land" for simulations - and the simplest to configure.
However, it has a few deficiencies that can cause problems when there is serious memory or network congestion.
More details (and their fixes) are given in the next abstract machine models. 	

#### 3.2.3: AMM2<a name="subsec:amm2"></a>




![Figure 3: AMM2: Adds extra memory model details to AMM1](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/amm/amm2_membus.png) 

*Figure 3: AMM2: Adds extra memory model details to AMM1*



A major deficiency of AMM1 is that it grants exclusive access to memory resources.
Two CPUs or the NIC cannot be using the memory subsystem in parallel.
This is particularly problematic for large memory transfers (1 MB or greater).
The memory system might be blocked for approx 1 ms,
creating unphysical delays while other resources wait for access.
A more realistic model allows multiple resources to access the memory,
albeit with reduced bandwidth when congestion is observed.
In many cases, multiple memory links or management units are connect to a shared bus.
The bus determines to the total, aggregate memory bandwidth.
However, the individual links determine the maximum observed bandwidth by any single component.
AMM2 has all the same parameters as AMM1, but now allows an additional parameter for memory.
These are special parameters used by the AMM configurations.
They can be by-passed by directly using fully namespaced parameters (not shown).

````
node {
 memory {
  max_single_bandwidth = 5GB/s
  bandwidth = 10GB/s
  latency = 15ns
 }
}
````
The new parameter `max_single_bandwidth` now defines the maximum bandwidth any single component is allowed.
Thus, even if the CPU is doing something memory intensive, 5 GB/s is still available to the NIC for network transfers.
We remark here that the memory parameters might be named something slightly more descriptive.
However, as a rule, we want the AMM1 parameters to be a proper subset of the AMM2 parameters.
Thus parameter names should not change - only new parameters should be added.

#### 3.2.4: AMM3<a name="subsec:ammThree"></a>




![Figure 4: AMM3: Adds extra router (switch) details to AMM2](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/amm/amm3_switch.png) 

*Figure 4: AMM3: Adds extra router (switch) details to AMM2*



A major deficiency of AMM2 is its inability to distinguish between the network link bandwidth (associated with the outport port serializer/deserializer) and the switch bandwidth (associated with the crossbar that arbitrates packets).  
Only packets traveling the same path cause congestion on the network links in AMM1 and AMM2.
However, packets "intersecting" at a switch - even if following separate paths - can cause congestion through sharing the switching fabric.
AMM3 generalize the network parameters by adding a switch bandwidth.
We note again here that AMM3 has all the same parameters as AMM2, plus the additional switch bandwidth parameter.
Thus, higher-numbered abstract machine models always add more detail.
These are special parameters used by the AMM configurations.
They can be by-passed by directly using fully namespaced parameters (not shown) for more detailed configurations.

````
switch {
  xbar {
   bandwidth = 12GB/s  
  }
  link {
   bandwidth = 6GB/s
   latency = 100ns
  }
}
````



### Section 3.3: Network Topologies and Routing<a name="sec:tutorial:topology"></a>


We here give a brief introduction to specifying different topologies and routing strategies.  
We will only discuss one basic example (torus).  
A more thorough introduction covering all topologies is planned for future releases.
Excellent resources are "Principles and Practices of Interconnection Networks" by Brian Towles and William Dally published by Morgan Kaufman and "High Performance Datacenter Networks" by Dennis Abts and John Kim published by Morgan and Claypool.

#### 3.3.1: Topology<a name="subsec:tutorial:topology"></a>



Topologies are determined by two mandatory parameters.

````
topology.name = torus
topology.geometry = 4 4
````
Here we choose a 2D-torus topology with extent 4 in both the X and Y dimensions for a total of 16 nodes (Figure [5](#fig:torus:basic))
The topology is laid out in a regular grid with network links connecting nearest neighbors.  
Additionally, wrap-around links connect the nodes on each boundary.  

![Figure 5: 4 x 4 2D Torus](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/tikz/torus/torus.png) 

*Figure 5: 4 x 4 2D Torus*




The figure is actually an oversimplification.  
The `topology.geometry` parameter actually specifies the topology of the network switches, not the compute nodes. 
A torus is an example of a direct network in which each switch has one or more nodes "directly" connected to it.  
A more accurate picture of the network is given in Figure [6](#fig:torus:withnodes).

![Figure 6: 4 x 4 2D Torus of Network Switches with Compute Nodes](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/tikz/torus/withnodes.png) 

*Figure 6: 4 x 4 2D Torus of Network Switches with Compute Nodes*


While in many previous architectures there was generally a one-to-one correspondence between compute nodes and switches, more recent architectures have multiple compute nodes per switch (e.g. Cray Gemini with two nodes, Cray Aries with four nodes).  
Multiple nodes per switch can be specified via a concentration parameter:

````
topology {
 name = torus
 geometry = 4 4
 concentration = 2
}
````
which would now generate a torus topology with 16 switches and 32 compute nodes.

Another subtle modification of torus (and other networks) can be controlled by giving the X, Y, and Z directions different bandwidth.  
The above network could be modified as

````
topology {
 name = torus
 geometry = 4 4
 redundant = 2 1
}
````
giving the the X-dimension twice the bandwidth of the Y-dimension.  
This pattern DOES exist in some interconnects as a load-balancing strategy.  
A very subtle point arises here. Consider two different networks:

````
topology {
 name = torus
 geometry = 4 4
 redundant = 1 1
}
switch.link.bandwidth = 2GB/s
````
````
topology {
 name = torus
 geometry = 4 4
 redundant = 2 2
}
switch.link.bandwidth = 1GB/s
````
For some coarse-grained models, these two networks are exactly equivalent.  
In more fine-grained models, however, these are actually two different networks.  
The first network has ONE link carrying 2 GB/s. The second network has TWO links each carrying 1 GB/s.

#### 3.3.2: Routing<a name="subsec:tutorial:routing"></a>


By default, SST-macro uses the simplest possible routing algorithm: dimension-order minimal routing (Figure [7](#fig:torus:basicrouting)).

![Figure 7: Dimension-Order Minimal Routing on a 2D Torus](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/tikz/torus/minroutetorus.png) 

*Figure 7: Dimension-Order Minimal Routing on a 2D Torus*


In going from source to destination, the message first travels along the X-dimension and then travels along the Y-dimension.
The above scheme is entirely static, making no adjustments to avoid congestion in the network.  
SST-macro supports a variety of adaptive routing algorithms.  This can be specified:

````
switch {
 router {
  name = min_ad
 }
}
````
which specifies minimal adaptive routing. 
There are now multiple valid paths between network endpoints, one of which is illustrated in Figure [8](#fig:torus:minadrouting).

![Figure 8: Adaptive Minimal Routing on a 2D Torus](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/tikz/torus/minadroutetorus.png) 

*Figure 8: Adaptive Minimal Routing on a 2D Torus*


At each network hop, the router chooses the productive path with least congestion.  
In some cases, however, there is only one minimal path (node (0,0) sending to (2,0) with only X different).
For these messages, minimal adaptive is exactly equivalent to dimension-order routing.  
Other supported routing schemes are valiant and UGAL.  More routing schemes are scheduled to be added in future versions.  
A full description of more complicated routing schemes will be given in its own chapter in future versions. 
For now, we direct users to existing resources such as "High Performance Datacenter Networks" by Dennis Abts and John Kim.



### Section 3.4: Network Model<a name="sec:tutorial:networkmodel"></a>



Network models can be divided into several categories.  SST/macro supports analytic models, which estimate network delays via basic latency/bandwidth formulas, and packet models, which model step-by-step the transit of individuals through the interconnect.
A third class of models (flow models), was previously supported but are now discontinued due to the much better scalability of packet models.

#### 3.4.1: Analytic Models: MACRELS<a name="subsec:tutorial:macrels"></a>



The analytic models in SST/macro are colloqiually referred to as MACRELS (MTL for AnalytiC REally Lightweight Simulation).
The MTL (message transfer layer) moves entire network flows from point-to-point without packetizing them into smaller chunks.
Thus an entire 1 MB MPI message is transported as a single chunk of data.  
The majority of MACRELS models are based on the LogP set of approximations:


&Delta; t = &alpha; + &beta; N

where &Delta; t is the time delay, &alpha; is the minimum latency of the communication, &beta; is the inverse bandwidth (s/B), and N is the number of bytes.
In abstract machine models, these methods are selected as:

````
congestion_model = logP
````
Details are shown for traffic moving from source to destination in Figure [9](#fig:macrelsOverview).
Modeling occurs on entire flows, rather than individual packets. 

\begin{enumerate}
\item Flows queue waiting for NIC injection link to become available. Flow is forwarded to destination NIC based after computed delay. 
\item Flows queue waiting for NIC ejection link to become available. Flow finishes after ejection link becomes available.
\end{enumerate}


![Figure 9: MACRELS (Messages with AnalytiC REally Lightweight Simulation) skips congestion modeling and approximates send delays using a simple latency/bandwidth estimate, similar to the LogGOP model. Modeling occurs on entire flows, rather than individual packets. For details on numbered steps, see text.](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/macrels.pdf) 

*Figure 9: MACRELS (Messages with AnalytiC REally Lightweight Simulation) skips congestion modeling and approximates send delays using a simple latency/bandwidth estimate, similar to the LogGOP model. Modeling occurs on entire flows, rather than individual packets. For details on numbered steps, see text.*




#### 3.4.2: Packet Models: PISCES<a name="subsec:tutorial:pisces"></a>



PISCES (Packet-flow Interconnect Simulation for Congestion at Extreme Scale) breaks network flows (MPI messages) into individual packets and models each packet individually.
In abstract machine models, PISCES can be selected as:

````
congestion_model = pisces
````
In reality, packets are further subdivided into flits (flow-control units).
Flit-level detail would be way too computationally intense for large-scale simulation.
All routing decisions are made on packets as a while. 
Two flits in the same packet cannot take different paths through the network.
However, they may not travel together.

PISCES (Packet-flow Interconnect Simulation for Congestion at Extreme-Scale) models individual packets moving through the network. Flits (flow-control units) are approximately modeled using flow-like approximations. Packets can have partial occupancies in several different buffers, approximating wormhole routing. However, arbitration is modeled on whole packets, not individual flits (see Figure [10](#fig:piscesOverview))
\begin{enumerate}
\item A message (flow) is broken up into packets. Depending on available space in the Tx buffer, a limited number of packets may be able to queue up in the buffer. If credits are available in the Rx buffer for the link and the link is idle, the packet moves into the next Rx buffer after a computed delay.
\item The router selects a path for the packet and the packet requests to the crossbar to transmit to the corresponding output port. If credits are available for the Rx buffer, the crossbar may select the packet in arbitration and move it to the output buffer. After moving, the Rx buffer returns credits to the previous Tx buffer for that packet.
\item Step 1 is repeated for the next Rx buffer, waiting for credits and link availability.
\item Repeat Step 2
\item Repeat Step 3
\item Packet arrives in NIC Rx queue and queues waiting to inject into local memory. After injection, the Rx buffer returns credits to the corresponding Tx buffer.
\end{enumerate}


![Figure 10](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/pisces_overview.pdf) 

*Figure 10*




PISCES provides two mechanisms for treating flit-level flow control discussed next.

##### PISCES simple model<a name="subsubsec:tutorial:simplePisces"></a>


In the simple model, each router uses a basic store-and-forward mechanism.
Flits are not allowed to "separate" and always travel as a single unit.
The entire packet has to be stored within a router before it can be forwarded to the next router.
The simple model affects the arbitrator that decided when and how to transmit flits.
To select a simple model:

````
arbitrator = simple
````
The simple model is the least computationally expensive. 
However, for large packet sizes, it can produce erroneously high latencies.
To tune the packet size for abstract machine models, set:

````
accuracy_parameter = 1024B
````
or equivalently 

````
mtu = 1024B
````
which sets the packet size to 1024B. 
For the simple model, packet sizes larger than 256-512B are not recommended.
Packet sizes on production supercomputers are often small (96-128B).
Small packet sizes with the simple model can be a good compromise for having more fine-grained routing but cheaper congestion modeling in the arbitrator.
More details are given in Figure [10](#fig:piscesOverview).

##### PISCES cut-through model<a name="subsubsec:tutorial:cutThroughPisces"></a>




![Figure 11: Timeline of four different packets passing through a PISCES cut-through bandwidth arbitrator. The incoming bandwidth (I) and outgoing bandwidth (O) are shown for each packet.  Time is the horizontal axis. Bandwidth consumed by a packet is shown by the vertical extent of each packet. The individual events are 1) First packet arrives 2) Second packet arrives with reduced bandwidth but no available bandwidth 3) First packet finishes. Second packet can begin sending. 4) Third packet arrives and begins sending with remaining bandwidth. 5) Fourth packet arrives, but no available bandwidth. 6) Second packet finishes. Third packet increases bandwidth. Fourth packet can begin sending. 7) Third packet finishes. Fourth packet increases bandwidth. 8) Fourth packet finishes.
Full details are given in the text.](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/pisces) 

*Figure 11: Timeline of four different packets passing through a PISCES cut-through bandwidth arbitrator. The incoming bandwidth (I) and outgoing bandwidth (O) are shown for each packet.  Time is the horizontal axis. Bandwidth consumed by a packet is shown by the vertical extent of each packet. The individual events are 1) First packet arrives 2) Second packet arrives with reduced bandwidth but no available bandwidth 3) First packet finishes. Second packet can begin sending. 4) Third packet arrives and begins sending with remaining bandwidth. 5) Fourth packet arrives, but no available bandwidth. 6) Second packet finishes. Third packet increases bandwidth. Fourth packet can begin sending. 7) Third packet finishes. Fourth packet increases bandwidth. 8) Fourth packet finishes.
Full details are given in the text.*



In the cut-through model, routing decisions still occur at the packet-level.
However, some attempt is made to account for pipelining of flits across different router stages.
Somewhat similar to the LogP models used above, latency/bandwidth formulas are used to estimate packet delays.
However, the cut-through model adds more details.
It's requested as:

````
arbitrator = cut_through
````
Figure [11](#fig:pisces) shows a timeline for the data being transmitted through a crossbar, SerDes, or other network component with a "fixed bandwidth." 
Each component is essentially a pipe with some flow bandwidth.
The arbitrator divides its limited bandwidth amongst incoming packets.
Packets fill the pipeline, consuming bandwidth.
In contrast to the completely discrete simple model, packets can "multiplex" in the component sharing an arbitrary bandwidth partition.
Modeling a packet delay starts with two input parameters and computes three output parameters.


-   A: Packet head arrival (input)
-   I: Packet incoming bandwidth (input)
-   H: Packet head departure (output)
-   T: Packet tail departure (output)
-   O: Packet outgoing bandwidth (output)

In the simple model, a packet either consumes all the bandwidth or none of the bandwidth.
To account for flit-level pipelining, the cut-through model allows packets to consume partial bandwidths.
Consider an aribitrator that has a maximum bandwidth of 1.0.
The first packet (purple, Figure [11](#fig:pisces)) arrives with a full incoming bandwidth of 1.0 and head arrival of t=0.0.
It therefore consumes all the available bandwidth. 
The head of the packet can actually leave immediately (as it must to properly pipeline or cut-through).
The tail leaves after all bytes have sent at t=1.0.
Thus for the first packet we have H=0.0, T=1.0, and O=1.0.

The second packet (orange) arrives at t=0.5. 
Upon arrival there is no bandwidth available as the first packet is consuming the maximum.
Only after the first packet finishes can the second packet begin.
The second packet arrives and leaves with a reduced bandwidth of 0.5. 
Thus we have H=1.0, T=3.0, and O=0.5.

The third packet (green) arrives at t=1.75.
Upon arrival there is some bandwidth, but not enough to match the incoming bandwidth of 0.57.
Thus the third packet is slowed initially.
After the second packet finished, the third packet can send at increased bandwidth.
The computation here is a bit more complicated.
Packet 3 can actually consume MORE than 0.6 bandwidth units.
Between steps 4 and 6, packet 3 has accumulated in a local buffer in the router.
Thus even though the incoming bandwidth is only 0.6, there are several flits that are available to send immediately at full bandwidth waiting in the buffer.
Thus results in an effective bandwidth of 0.75 for the remainder of the packet's time in the arbitrator.
Thus we end up with H=1.75, T=3.5, and O=0.57.
Even though the packet is initially delayed, the buffers compensate for the delay and allow the outgoing bandwidth to "catch up" with the incoming bandwidth.

Finally, the fourth packet (blue) arrives at t=3.0. 
There is some available bandwidth. After the third packet finishes, the fourth packet can now send at maximum.
Because of the initial delay, the outgoing bandwidth is somewhat reduced.
We have H=3.0, T=4.38, and O=0.73.

\subsection{SCULPIN}
Under current architectural trends, switches have ample buffer space and crossbar bandwidth, making the mostly likely bottleneck edge bandwidth through the output ports.
SCULPIN (Simple Congestion Unbuffered Latency Packet Interconnection Network) models the main source of contention in today's networks occurring on the output port ser/des. Unlike PISCES, individual flits are not able to wormhole route across links interspersed with flits from other packets.
\begin{enumerate}
\item A message (flow) is broken up into packets. Each packet waits in the queue to send based on link availability and QoS.
\item After being selected, the packets are forwarded to the switch. Packets are immediately routed to the correct output port, skipping crossbar arbitration. Packets wait in unbounded queues, thereby assuming sufficient buffer space is always available.
\item Repeat Step 1. Packet waits in queue until link becomes available based on QoS. Packet is immediately forwarded to next output port, skipping arbitration
\item Repeat Step 1.
\item Packet arrives in NIC Rx queue (no credits, buffer assumed to always have space). Packets queue waiting to inject into local memory.
\end{enumerate}


![Figure 12](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/sculpin.pdf) 

*Figure 12*



#### 3.4.3: Flow<a name="subsec:tutorial:flow"></a>


The flow model, in simple cases, corrects the most severe problems of the packet model.
Instead of discrete chunks, messages are modeled as fluid flows moving through the network.
Congestion is treated as a fluid dynamics problem, sharing bandwidth between competing flows.
In contrast to LogP models, flow models can account fairly well for congestion.
Without congestion, a flow only requires a FLOW START and FLOW STOP event to be modeled (see tutorial on discrete event simulation in [3.7](#sec:tutorial:des)).
While the packet model would require many, many events to simulate a 1 MB message, the flow model might only require two.
With congestion, flow update events must be scheduled whenever congestion changes on a network link.  
For limited congestion, only a few update events must occur.
The flow model also corrects the latency and multiplexing problems in the PISCES simple model, providing higher-accuracy for coarse-grained simulation.

The flow model starts to break down for large systems or under heavy congestion.
In the packet model, all congestion events are "local" to a given router.  
The number of events is also constant in packet models regardless of congestion since we are modeling a fixed number of discrete units.
In flow models, flow update events can be "non-local," propagating across the system and causing flow update events on other routers.
When congestion occurs, this "ripple effect" can cause the number of events to explode, overwhelming the simulator.
For large systems or heavy congestion, the flow model is actually much slower than the packet model. Support for this model has been completely removed.





### Section 3.5: Basic MPI Program<a name="sec:tutorial:basicmpi"></a>


Let us go back to the simple send/recv skeleton and actually look at the code.  
This code should be compiled with SST compiler wrappers installed in the `bin` folder.

````
#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

#define sstmac_app_name simple_test

int main(int argc, char **argv) 
{
  int message_size = 128;
  int me, nproc;
  int tag = 0;
  int dst = 1;
  int src = 0;
  MPI_Status stat;

  MPI_Init(&argc,&argv);
  MPI_Comm world = MPI_COMM_WORLD;
  MPI_Comm_rank(world,&me);
  MPI_Comm_size(world,&nproc);
````
The starting point is creating a main routine for the application.
The simulator itself already provides a `main` routine.
The SST compiler automatically changes the function name to `userSkeletonMain`,
which provides an entry point for the application to actually begin.
When SST-macro launches, it will invoke this routine and pass in any command line arguments specified via the `app1.argv` parameter.  Upon entering the main routine, 
the code is now indistinguishable from regular MPI C++ code.  
In the parameter file to be used with the simulation, you must set

````
node.app1.name = simple_test
````

The name associated to the application is given by the `sstmac_app_name` macro.
This macro must be defined to a unique string name in the source file containing `main`.
SST-macro will automatically associate the given main routine with the string internally.
That application can then be selected in the input file with `app1.name`.

At the very top of the file, the `mpi.h` header is actually mapped by the SST compiler to an SST-macro header file.
This header provides the MPI API and configures MPI function calls to link to SST-macro instead of the real MPI library.  
The code now proceeds:

````
if (nproc != 2) {
    fprintf(stderr, "sendrecv only runs with two processors\n");
      abort();
  }
  if (me == 0) {
    MPI_Send(NULL, message_size, MPI_INT, dst, tag, world);
    printf("rank %i sending a message\n", me);
  }
  else {
    MPI_Recv(NULL, message_size, MPI_INT, src, tag, world, &stat);
    printf("rank %i receiving a message\n", me);
  }
  MPI_Finalize();
  return 0;
}
````
Here the code just checks the MPI rank and sends (rank 0) or receives (rank 1) a message.



### Section 3.6: Launching, Allocation, and Indexing<a name="sec:tutorial:launchetc"></a>



#### 3.6.1: Launch Commands<a name="subsec:tutorial:launch"></a>


Just as jobs must be launched on a shared supercomputer using Slurm or aprun, 
SST/macro requires the user to specify a launch command for the application.
Currently, we encourage the user to use aprun from Cray, for which documentation can easily be found online.
In the parameter file you specify, e.g.

````
node {
 app1 {
  name = user_mpiapp_cxx
  launch_cmd = aprun -n 8 -N 2
 }
}
````
which launches an external user C++ application with eight ranks and two ranks per node.
The aprun command has many command line options (see online documentation), some of which may be supported in future versions of SST/macro.  In particular, we are in the process of adding support for thread affinity, OpenMP thread allocation, and NUMA containment flags.  Most flags, if included, will simply be ignored.

#### 3.6.2: Allocation Schemes<a name="subsec:tutorial:allocation"></a>


In order for a job to launch, it must first allocate nodes to run on. Here we choose a simple 2D torus

````
topology.name = torus
topology.geometry = 3 3
topology.concentration = 1
````
which has 9 nodes arranged in a 3x3 mesh.  
For the launch command `aprun -n 8 -N 2`, we must allocate 4 compute nodes from the pool of 9.
Our first option is to specify the first available allocation scheme (Figure [13](#fig:allocation:first_available))

````
node.app1.allocation = first_available
````

![Figure 13: First available Allocation of 4 Compute Codes on a 3x3 2D Torus](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/tikz/allocation/firstavailable.png) 

*Figure 13: First available Allocation of 4 Compute Codes on a 3x3 2D Torus*


In first available, the allocator simply loops through the list of available nodes as they are numbered by the topology object.
In the case of a 2D torus, the topology numbers by looping through columns in a row.
In general, first available will give a contiguous allocation, but it won't necessarily be ideally structured.

To give more structure to the allocation, a Cartesian allocator can be used (Figure [14](#fig:allocation:cartesian)).

````
app1 {
 allocation = cartesian
 cart_sizes = [2,2]
 cart_offsets = [0,0]
}
````

![Figure 14: Cartesian Allocation of 4 Compute Codes on a 3x3 2D Torus](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/tikz/allocation/cartesian.png) 

*Figure 14: Cartesian Allocation of 4 Compute Codes on a 3x3 2D Torus*


Rather than just looping through the list of available nodes, we explicitly allocate a 2x2 block from the torus.
If testing how "topology agnostic" your application is, you can also choose a random allocation.

````
node.app1.allocation = random
````

![Figure 15: Random Allocation of 4 Compute Codes on a 3x3 2D Torus](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/tikz/allocation/random.png) 

*Figure 15: Random Allocation of 4 Compute Codes on a 3x3 2D Torus*



In many use cases, the number of allocated nodes equals the total number of nodes in the machine.
In this case, all allocation strategies allocate the same set of nodes, i.e. the whole machine.
However, results may still differ slightly since the allocation strategies still assign an initial numbering of the node,
which means a random allocation will give different results from Cartesian and first available.


##### Indexing Schemes<a name="subsec:tutorial:indexing"></a>


Once nodes are allocated, the MPI ranks (or equivalent) must be assigned to physical nodes, i.e. indexed.
The simplest strategies are block and round-robin.  If only running one MPI rank per node, the two strategies are equivalent,
indexing MPI ranks in the order received from the allocation list.
If running multiple MPI ranks per node, block indexing tries to keep consecutive MPI ranks on the same node (Figure [16](#fig:indexing:block)).

````
node.app1.indexing = block
````

![Figure 16: Block Indexing of 8 MPI Ranks on 4 Compute Nodes](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/tikz/indexing/block.png) 

*Figure 16: Block Indexing of 8 MPI Ranks on 4 Compute Nodes*


In contrast, round-robin spreads out MPI ranks by assigning consecutive MPI ranks on different nodes (Figure [17](#fig:indexing:round_robin)).

````
node.app1.indexing = round_robin
````

![Figure 17: Round-Robin Indexing of 8 MPI Ranks on 4 Compute Nodes](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/tikz/indexing/roundrobin.png) 

*Figure 17: Round-Robin Indexing of 8 MPI Ranks on 4 Compute Nodes*


Finally, one may also choose

````
node.app1.indexing = random
````
Random allocation with random indexing is somewhat redundant.  
Random allocation with block indexing is not similar to Cartesian allocation with random indexing.
Random indexing on a Cartesian allocation still gives a contiguous block of nodes,
even if consecutive MPI ranks are scattered around.
A random allocation (unless allocating the whole machine) will not give a contiguous set of nodes.































































### Section 3.7: Discrete Event Simulation<a name="sec:tutorial:des"></a>


Although not necessary for using the simulator, a basic understanding of discrete event simulation can be helpful in giving users an intuition for network models and parameters.
Here we walk through a basic program that executes a single send/recv pair.
SST-macro simulates many parallel processes, but itself runs as a single process with only one address space (SST-macro can actually run in parallel mode, but we ignore that complication here).
SST-macro manages each parallel process as a user-space thread (application thread), allocating a thread stack and frame of execution.
User-space threading is necessary for large simulations since otherwise the kernel would be overwhelmed scheduling thousands of threads.

SST-macro is driven by a simulation thread which manages the user-space thread scheduling (Figure [18](#fig:des)).
In the most common (and simplest) use case, all user-space threads are serialized, running one at a time.
The main simulation thread must manage all synchronizations, yielding execution to process threads at the appropriate times.
The main simulation thread is usually abbreviated as the DES (discrete event simulation) thread.
The simulation progresses by scheduling future events.  
For example, if a message is estimated to take 5 &mu;s to arrive,
the simulator will schedule a MESSAGE ARRIVED event 5 &mu;s ahead of the current time stamp.
Every simulation starts by scheduling the same set of events: launch process 0, launch process 1, etc.


![Figure 18: Progression of Discrete Event Simulation for Simple Send/Recv Example](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/tikz/des/events.png) 

*Figure 18: Progression of Discrete Event Simulation for Simple Send/Recv Example*



The simulation begins at time t=0&mu; s.  
The simulation thread runs the first event, launching process 0.
The context of process 0 is switched in, and SST-macro proceeds running code as if it were actually process 0.
Process 0 starts a blocking send in Event 1.
For process 0 to perform a send in the simulator, it must schedule the necessary events to simulate the send.
Most users of SST-macro will never need to explicitly schedule events.
Discrete event details are always hidden by the API and executed inside library functions.
In this simple case, the simulator estimates the blocking send will take 1 &mu;s.
It therefore schedules a SEND DONE (Event 4) 1 &mu;s into the future before blocking.
When process 0 blocks, it yields execution back to the main simulation.

At this point, no time has yet progressed in the simulator.
The DES thread runs the next event, launching process 1, which executes a blocking receive (Event 3).
Unlike the blocking send case, the blocking receive does not schedule any events.
It cannot know when the message will arrive and therefore blocks without scheduling a RECV DONE event.
Process 1 just registers the receive and yields back to the DES thread.

At this point, the simulator has no events left at t=0 &mu;s and so it must progress its time stamp.
The next event (Event 4) is SEND DONE at t=1 &mu;s. The event does two things.
First, now that the message has been injected into the network, the simulator estimates when it will arrive at the NIC of process 1.
In this case, it estimates 1 &mu;s and therefore schedules a MESSAGE ARRIVED event in the future at t=2 &mu;s (Event 7).
Second, the DES thread unblocks process 0, resuming execution of its thread context.
Process 0 now posts a blocking receive, waiting for process 1 to acknowledge receipt of its message.

The simulator is now out of events at t=1 &mu;s and therefore progresses its time stamp to t=2 &mu;s.
The message arrives (Event 7), allowing process 1 to complete its receive and unblock.
The DES thread yields execution back to process 1, which now executes a blocking send to ack receipt of the message.
It therefore schedules a SEND DONE event 1 &mu;s in the future (Event 10) and blocks, yielding back to the DES thread.
This flow of events continues until all the application threads have terminated.
The DES thread will run out of events, bringing the simulation to an end. 





### Section 3.8: Using DUMPI<a name="sec:tutorial:dumpi"></a>



#### 3.8.1: Building DUMPI<a name="subset:dump:build"></a>


As noted in the introduction, SST-macro is primarily intended to be an on-line simulator. Real application code runs, but SST-macro  intercepts calls to communication (MPI) and computation functions to simulate time passing.  However, SST-macro can also run off-line, replaying application traces collected from real production runs.  This trace collection and trace replay library is called DUMPI.

Although DUMPI is automatically included as a subproject in the SST-macro download, trace collection can be easier if DUMPI is built independently from SST-macro.  The code can be downloaded from https://bitbucket.org/sst-ca/dumpi. If downloaded through Mercurial, one must initialize the build system and create the configure script.

````
dumpi> ./bootstrap.sh
````

DUMPI must be built with an MPI compiler.

````
dumpi/build> ../configure CC=mpicc CXX=mpicxx \ 
	              --enable-libdumpi --prefix=$DUMPI_PATH
````
The `--enable-libdumpi` flag is needed to configure the trace collection library.
After compiling and installing, a `libdumpi` will be added to `\$DUMPI_PATH/lib`.

Collecting application traces requires only a trivial modification to the standard MPI build.
Using the same compiler, simply add the DUMPI library path and library name to your project's `LDFLAGS`.

````
your_project/build> ../configure CC=mpicc CXX=mpicxx \
                                  LDFLAGS="-L$DUMPI_PATH/lib -ldumpi"
````

#### 3.8.2: Trace Collection<a name="subsec:dumpi:tracecollection"></a>


DUMPI works by overriding weak symbols in the MPI library.
In all MPI libraries, functions such as `MPI_Send` are only weak symbol wrappers to the actual function `PMPI_Send`.
DUMPI overrides the weak symbols by implementing functions with the symbol `MPI_Send`. 
If a linker encounters a weak symbol and regular symbol with the same name, it ignores the weak symbol.
DUMPI functions look like

````
int MPI_Send(...)
{
  /** Start profiling work */
  ...
  int rc = PMPI_Send(...);
  /** Finish profiling work */
  ...
  return rc;
}
````
collecting profile information and then directly calling the PMPI functions.

We examine DUMPI using a very basic example program.

````
#include <mpi.h>
int main(int argc, char** argv)
{
  MPI_Init(&argc, &argv);
  MPI_Finalize();
  return 0;
}
````
After compiling the program named `test` with DUMPI, we run MPI in the standard way.

````
example> mpiexec -n 2 ./test
````
After running, there are now three new files in the directory.

````
example> ls dumpi*
dumpi-2013.09.26.10.55.53-0000.bin	
dumpi-2013.09.26.10.55.53-0001.bin	
dumpi-2013.09.26.10.55.53.meta
````
DUMPI automatically assigns a unique name to the files from a timestamp.
The first two files are the DUMPI binary files storing separate traces for MPI rank 0 and rank 1.
The contents of the binary files can be displayed in human-readable form by running the `dumpi2ascii`
program, which should have been installed in `\$DUMPI_PATH/bin`.

````
example> dumpi2ascii dumpi-2013.09.26.10.55.53-0000.bin
````
This produces the output

````
MPI_Init entering at walltime 8153.0493, cputime 0.0044 seconds in thread 0.
MPI_Init returning at walltime 8153.0493, cputime 0.0044 seconds in thread 0.
MPI_Finalize entering at walltime 8153.0493, cputime 0.0045 seconds in thread 0.
MPI_Finalize returning at walltime 8153.0498, cputime 0.0049 seconds in thread 0.
````
The third file is just a small metadata file DUMPI used to configure trace replay.

````
hostname=deepthought.magrathea.gov
numprocs=2
username=slartibartfast
startime=1380218153
fileprefix=dumpi-2013.09.26.10.55.53
version=1
subversion=1
subsubversion=0
````

#### 3.8.3: Trace Replay<a name="subsec:dumpi:tracereplay"></a>


It is often useful to validate the correctness of a trace.  Sometimes there can be problems with trace collection. 
There are also a few nooks and crannies of the MPI standard left unimplemented.
To validate the trace, you can run in a special debug mode that runs the simulation with a very coarse-grained model
to ensure as quickly as possible that all functions execute correctly.
This can be done straightforwardly by running the executable with the dumpi flag: `sstmac --dumpi`.

To replay a trace in the simulator, a small modification is required to the example input file in [3.1](#sec:parameters).
We have two choices for the trace replay.  First, we can attempt to exactly replay the trace as it ran on the host machine.
Second, we could replay the trace on a new machine or different layout.

For exact replay, the key issue is specifying the machine topology.
For some architectures, topology information can be directly encoded into the trace.
This is generally true on Blue Gene, but not Cray.
When topology information is recorded, trace replay is much easier.
The parameter file then becomes, e.g.

````
node {
 app1 {
  indexing = dumpi
  allocation = dumpi
  name = parsedumpi
  dumpi_metaname = testbgp.meta
 }
}
````
We set indexing and allocation parameters to read from the DUMPI trace.
The application name is a special app that parses the DUMPI trace.
Finally, we direct SST-macro to the DUMPI metafile produced when the trace was collected.
To extract the topology information, locate the `.bin` file corresponding to MPI rank 0.
To print topology info, run

````
traces> dumpi2ascii -H testbgp-0000.bin
````
which produces the output

````
version=1.1.0
starttime=Fri Nov 22 13:53:58 2013
hostname=R00-M1-N01-J01.challenger
username=<none>
meshdim=3
meshsize=[4, 2, 2]
meshcrd=[0, 0, 0]
````
Here we see that the topology is 3D with extent 4,2,2 in the X,Y,Z directions.
At present, the user must still specify the topology in the parameter file.
Even though SST-macro can read the topology dimensions from the trace file,
it cannot read the topology type.  It could be a torus, dragonfly, or fat tree.
The parameter file therefore needs

````
topology {
 name = torus
 geometry = 4 2 2
}
````
Beyond the topology, the user must also specify the machine model with bandwidth and latency parameters.
Again, this is information that cannot be automatically encoded in the trace.
It must be determined via small benchmarks like ping-pong.
An example file can be found in the test suite in `tests/test_configs/testdumpibgp.ini`.

If no topology info could be recorded in the trace, more work is needed.
The only information recorded in the trace is the hostname of each MPI rank.
The parameters are almost the same, but with allocation now set to `hostname`.
Since no topology info is contained in the trace, 
a hostname map must be put into a text file that maps a hostname to the topology coordinates.
The new parameter file, for a fictional machine called deep thought

````
# Launch parameters
node {
 app1 {
  indexing = dumpi
  allocation = hostname
  name = parsedumpi
  dumpi_metaname = dumpi-2013.09.26.10.55.53.meta
  dumpi_mapname = deepthought.map
 }
}
# Machine parameters
topology {
 name = torus
 geometry = 2 2
}
````


In this case, we assume a 2D torus with four nodes.
Again, DUMPI records the hostname of each MPI rank during trace collection.
In order to replay the trace, the mapping of hostname to coordinates must be given in a node map file,
specified by the parameter `launch_dumpi_mapname`.
The node map file has the format

````
4 2
nid0 0 0
nid1 0 1
nid2 1 0
nid3 1 1
````
where the first line gives the number of nodes and number of coordinates, respectively.
Each hostname and its topology coordinates must then be specified.
More details on building hostname maps are given below.

We can also use the trace to experiment with new topologies to see performance changes.
Suppose we want to test a crossbar topology.

````
# Launch parameters
node {
 app1 {
  indexing = block
  allocation = first_available
  dumpi_metaname = dumpi-2013.09.26.10.55.53.meta
  name = parsedumpi
  size = 2
 }
}
# Machine parameters
topology {
 name = crossbar
 geometry = 4
}
````
We no longer use the DUMPI allocation and indexing. 
We also no longer require a hostname map.
The trace is only used to generate MPI events and no topology or hostname data is used.
The MPI ranks are mapped to physical nodes entirely independent of the trace.




### Section 3.9: Using Score-P and OTF2 (Beta)<a name="sec:tutorial:otf"></a>



OTF2 is part of Score-P. Sources for both can be found here 
````
http://www.vi-hps.org/projects/score-p
````


Trace collection requires both Score-P and OTF2 installations. Trace replay with SST/macro requires only OTF2.


#### 3.9.1: Trace Collection<a name="subsec:otf:traceCollection"></a>


Score-P's default collection strategy will include every function call in the trace, making even small programs produce untenably large traces. Score-P supports collection filters, which can restrict collection at a minimum to MPI and OMP function calls. At the end of the program's runtime, traces from each rank are put in a common directory.  An MPI program must be compiled with Score-P to produce traces:

````
scorep-mpicxx -o test.exe test.cc
````



To limit the size of the traces, run the program with:

````
# these environment variables are picked up by Score-P at runtime
export SCOREP_ENABLE_TRACING=true
export SCOREP_TOTAL_MEMORY=1G
export SCOREP_FILTERING_FILE='scorep.filter'

mpirun -n 2 test.exe
````

The file `scorep.filter` should contain:
````
SCOREP_REGION_NAMES_BEGIN EXCLUDE *
````


To view a plain-text representation of the trace after running, use the otf2-print tool.

````
otf2-print scorep-*/traces.otf2
````

#### 3.9.2: Trace Replay<a name="subsec:otf:traceReplay"></a>


SST/macro will use a trace replay skeleton for OTF2 in much the same way as it does for dumpi. SST/macro trace replays configured using *.ini files. 

````
...

node {
 app1 {
  otf2_timescale = 1.0
  name = otf2_trace_replay_app
  size = N
  otf2_metafile = <trace-root>/scorep-20170309_1421_27095992608015568/traces.otf2
 # debugging output
  otf2_print_mpi_calls=false
  otf2_print_trace_events=false
  otf2_print_time_deltas=false
  otf2_print_unknown_callback=false
 }
}
````

#### 3.9.3: Trace Emission<a name="subsec:otf:traceEmission"></a>


SST/macro can emmit OTF2 traces from MPI simulations. This gets activated by:

````
...

node {
 app1 {
  ...
    otf2 {
      fileroot = dump
    }
  ...
 }
}
````




### Section 3.10: Call Graph Visualization<a name="sec:tutorials:callgraph"></a>


Generating call graphs requires a special build of SST-macro.

````
build> ../configure --prefix=$INSTALL_PATH --enable-graphviz
````
The `--enable-graphviz` flag defines an instrumentation macro throughout the SST-macro code.
This instrumentation must be compiled into SST-macro.
In the default build, the instrumentation is not added since the instrumentation has a high overhead.
However, SST-macro only instruments a select group of the most important functions so the overhead should only be 10-50\
After installing the instrumented version of SST-macro, a call graph must be activated as part of the OS component.

````
node {
  os {
    callGraph {
      fileroot = <fileroot>
    }
  }
}
````
After running, a `<fileroot>.callgrind.out` file should appear in the folder.

To visualize the call graph, you must download KCachegrind: http://kcachegrind.sourceforge.net/html/Download.html.
KCachegrind is built on the KDE environment, which is simple to build for Linux but can be very tedious for Mac.
The download also includes a QCachegrind subfolder, providing the same functionality built on top of Qt.  
This is highly recommended for Mac users.


![Figure 19: QCachegrind GUI](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/graphviz/gui.png) 

*Figure 19: QCachegrind GUI*



The basic QCachegrind GUI is shown in Figure [19](#fig:qcgui).  
On the left, a sidebar contains the list of all functions instrumented with the percent of total execution time spent in the function.
In the center pane, the call graph is shown.  
To navigate the call graph, a small window in the bottom right corner can be used to change the view pane.
Zooming into one region (Figure [20](#fig:qcgraphone)), we see a set of MPI functions (Barrier, Scan, Allgatherv).
Each of the functions enters a polling loop, which dominates the total execution time.  
A small portion of the polling loop calls the ``Handle Socket Header" function.
Double-clicking this node unrolls more details in the call graph (Figure [21](#fig:qcgraphtwo)).
Here we see the function splits execution time between buffering messages (memcpy) and posting headers (Compute Time).


![Figure 20: QCachegrind Call Graph of MPI Functions](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/graphviz/callgraph1.png) 

*Figure 20: QCachegrind Call Graph of MPI Functions*




![Figure 21: QCachegrind Expanded Call Graph of Eager 0 Function](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/graphviz/callgraph2.png) 

*Figure 21: QCachegrind Expanded Call Graph of Eager 0 Function*






### Section 3.11: Spyplot Diagrams<a name="sec:tutorials:spyplot"></a>



Spyplots visualize communication matrices, showing either the number of messages or number of bytes sent between two network endpoints.
They are essentially contour diagrams, where instead of a continuous function F(x,y) we are plotting the communication matrix M(i,j).
An example spyplot is shown for a simple application that only executes an MPI\_Allreduce (Figure [22](#fig:spyplot)).
Larger amounts of data (red) are sent to nearest neighbors while decreasing amounts (blue) are sent to MPI ranks further away.


![Figure 22: Spyplot of Bytes Transferred Between MPI Ranks for MPI\_Allreduce](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/spyplot/mpi_spyplot.png) 

*Figure 22: Spyplot of Bytes Transferred Between MPI Ranks for MPI\_Allreduce*



Various spyplots can be activated by boolean parameters in the input file.
The most commonly used are the MPI spyplots, for which you activate the spyplot as part of the MPI subcomponent.

````
node {
  app1 {
    mpi {
      traffic_matrix {
        fileroot = test
      }
    }
  }
}
````

After running there will be a .csv and .png file in the folder, with e.g. `fileroot = test`

````
example> ls 
test.png
test.csv
````
`test.png` shows the number of bytes exchanged between MPI ranks.
To extend the analysis you can instead specify as part of the NIC subcomponent

````
node {
  nic {
    traffic_matrix {
      fileroot = test
    }
  }
}
````
A new csv/png will appear showing the number of bytes exchanged between physical nodes, 
accumulating together all MPI ranks sharing the same node.
This gives a better sense of spatial locality when many MPI ranks are on the same node.






### Section 3.12: Fixed-Time Quanta Charts<a name="sec:tutorials:ftq"></a>




![Figure 23: Application Activity (Fixed-Time Quanta; FTQ) histogram](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/matplotlib/ftq/pic1024.png) 

*Figure 23: Application Activity (Fixed-Time Quanta; FTQ) histogram*



Another way of visualizing application activity is a fixed-time quanta (FTQ) chart.
While the call graph gives a very detailed profile of what critical code regions, they lack temporal information. 
Figure [23](#fig:ftq) displays the proportion of time spent by ranks in MPI communication and computation in a PIC trace replay with respect to simulation time.
After running, two new files appear in the folder: `<fileroot>_app1.py` and `<fileroot>_app1.dat` that can use Python's matplotlib to generate plots.
Previously, plots were generated using Gnuplot, but this has been deprecated in favor of much more aesthetically pleasing maplotlib output.

````
your_project # python output_app1.py --help
usage: output_app1.py [-h] [--show] [--title TITLE] [--eps] [--pdf] [--png]
                      [--svg]

optional arguments:
  -h, --help     show this help message and exit
  --show         display the plot on screen
  --title TITLE  set the title
  --eps          output .eps file
  --pdf          output .pdf file
  --png          output .png file
  --svg          output .svg file
````

Generating the histogram requires matplotlib, and visualizing the histogram interactively with `--show` requires a screen or X11 forwarding.
FTQ aggregates tags into tunable time "epocs".
An epoc states the ratio of each tag represented at a point in time.
Larger epocs will smooth the graph and decrease the quantity of data required to render a plot; while a smaller epoc will add more detail, at the risk of making the plot excessively volatile.


SST/macro activates FTQ when the following two parameters are found in the ".ini" file:

````
node.os.ftq.fileroot=<fileroot>
node.os.ftq.epoch=5us
````
where the `fileroot` a path and a file name prefix.




### Section 3.13: Network Statistics<a name="sec:tutorials:packetStats"></a>



Here we describe a few of the network statistics that can be collected and the basic mechanism for activating them.
These statistics are usually collected on either the NIC, switch crossbar, or switch output buffers.

#### 3.13.1: Message Size Histogram<a name="subsec:messageSizeHistogram"></a>


To active a message size histogram on the NIC to determine the distribution of message sizes, the parameter file should include, for example:

````
node {
 nic {
  message_size_histogram {
   fileroot = histogram
   bin_size = 1
   logarithmic = true
  }
 } 
}
````
The statistics are activated when the parameter reader sees the namespace `message_size_histogram`.
In this case, we ask for a logarithmic distribution.
The bin size here is in logarithmic units, i.e. group results in bins corresponding to an exponent range of size 1.
An example generated for Nekbone with 1024 processors is in Figure [24](#fig:nekboneSizeHistogram).


![Figure 24: Logarithmic histogram of message sizes sent by Nekbone application](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/messageSizeHistogramNekbone) 

*Figure 24: Logarithmic histogram of message sizes sent by Nekbone application*



#### 3.13.2: Congestion Delay Histogram<a name="subsec:congestionDelayHistogram"></a>



A more involved example looks at congestion delays in the application.
We want to generate a histogram showing the aggregate delay (relative to zero-congestion baseline) that a packet experiences
going from source to destination.
By default, packets do not carry fields for measuring congestion. 
Thus, the packet allocator must be changed.
The NIC parameters now become:

````
node {
 nic {
  ejection {
   stats = delay_histogram
   delay_histogram {
    fileroot = delay
    bin_size = 0.5us
   }
  }
 }
}
````
The delay histogram goes in the ejection namespace since we want to measure packet congestion after it leaves the network.
The histogram is not logarithmic and we want to bin files in units of 0.5&mu;s.
After running, two files will be generated: a gnuplot script`delay.p` and a corresponding data file `delay.dat`.
After running

````
shell>gnuplot delay.p
````
a PNG file `delay.png` is generated.
The generated histogram is shown in Figure [25](#fig:nekboneDelayHistogram). 
Delays, when occurring in this case, are usually on the order of a few &mu;s.


![Figure 25: Histogram of message sizes sent by Nekbone application](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/delayHistogramNekbone) 

*Figure 25: Histogram of message sizes sent by Nekbone application*



A few more things are actually required to complete the input file.
The input file above only tells the NIC to create a histogram.
We still have to tell the crossbars and buffers on the network to accumulate the delays on the packet.
Otherwise the NIC will just see a zero delay.
The following extra parameters are required:

````
switch {
 xbar {
   stats = congestion_delay
 }
 output_buffer {
  stats = congestion_delay
 }
}
````

#### 3.13.3: Congestion Spyplot and Multi-stats<a name="subsec:congestionSpyplot"></a>



Another way to look for congestion is to create a spyplot.
Each row/column is a source/destination pair.
In contrast to previous spyplots that show the amount of traffic,
a congestion spyplot shows the amount of network congestion experienced sending between two points.
Most of the same parameters from the delay histogram are required.
However, we now want to collect both a spyplot and the histogram.

````
node {
 nic {
  ejection {
   stats = multi
   callbacks = congestion_spyplot delay_histogram
   congestion_spyplot {
    fileroot = spyplot
    type = png
    normalization = 100000
   }
   delay_histogram {
    fileroot = delay
    bin_size = 0.5us
   }
  }
 }
}
````

We set the stats collector to `multi`.
We then supply the list of desired stats to the `callbacks` parameter.
Zooming into the Nekbone example, we can see congestion hotspots.
For the most part, very little congestion appears in Figure [26](#fig:nekboneCongestionSpyplot).
However (at least for the Torus topology used here),
there are a few off-diagonal regions that show some congestion.


![Figure 26: Spyplot showing congestion hotspots for certain source/destination pairs for Nekbone.](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/congestionSpyplotNekbone) 

*Figure 26: Spyplot showing congestion hotspots for certain source/destination pairs for Nekbone.*











## Chapter 4: Topologies<a name="chapter:topologies"></a>




The torus topology is straightforward and easy to understand.
Here we introduce the basics of other topologies within SST that are more complex and require extra documentation to configure properly.
These are generally higher-radix or path-diverse topologies like fat tree, dragonfly, and flattened butterfly.  
As noted in [3.3](#sec:tutorial:topology), a more thorough and excellent discussions of these topologies is given in "High Performance Datacenter Networks" by Dennis Abts and John Kim.


















































### Section 4.1: Torus<a name="subsec:tutorial:hypercube"></a>



The torus is the simplest topology and fairly easy to understand.
We have already discussed basic indexing and allocation as well as routing.
More complicated allocation schemes with greater fine-grained control can be used such as the
coordinate allocation scheme (see hypercube below for examples) and the node ID allocation scheme (see fat tree below for examples).
More complicated Valiant and UGAL routing schemes are shown below for hypercube and Cascade,
but apply equally well to torus.

For torus we illustrate here the Cartesian allocation for generating regular Cartesian subsets.
For this, the input file would look like 

````
topology {
 name = torus
 geometry = 4 4 4
}
node {
 app1 {
  launch_cmd = aprun -n 8
  indexing = block
  allocation = cartesian
  cart_sizes = [2,2,2]
  cart_offsets = [0,0,0]
 }
}
````

This allocates a 3D torus of size 4x4x4.
Suppose we want to allocate all 8 MPI ranks in a single octant?
We can place them all in a 2x2x2 3D sub-torus by specifying the size of the sublock 
(`cart_sizes`) and which octant (`cart_offsets`).
This applies equally well to higher dimensional analogs.
This is particularly useful for allocation on Blue Gene machines
which always maintain contiguous allocations on a subset of nodes.

This allocation is slightly more complicated if we have multiple nodes per switch.
Even though we have a 3D torus, 
we treat the geometry as a 4D coordinate space with the 4th dimension referring to nodes connected to the same switch, 
i.e. if two nodes have the 4D coordinates [1 2 3 0] and [1 2 3 1] they are both connected to the same switch.
Consider the example below:

````
topology {
 name = torus
 geometry = 4 4 4
 concentration = 2
}
app1 {
 launch_cmd = aprun -n 8
 indexing = block
 allocation = cartesian
 cart_sizes = [2,2,1,2]
 cart_offsets = [0,0,0,0]
}
````

We allocate a set of switches across an XY plane (2 in X, 2 in Y, 1 in Z for a single plane).
The last entry in `cart_sizes` indicates that both nodes on each switch should be used.




### Section 4.2: Hypercube<a name="subsec:tutorial:hypercube"></a>



Although never used at scale in a production system, the generalized hypercube is an important topology to understand, particularly for flattened butterfly and Cascade.
The (k,n) generalized hypercube is geometrically an N-dimensional torus with each dimension having size k (although dimension sizes need not be equal).
Here we show a (4,2) generalized hypercube (Figure [27](#fig:topologies:hypercubeConnected)).  This would be specified in SST as:

````
topology.name = hypercube
topology.geometry = 4 4
````
indicating size 4 in two dimensions. 

While a torus only has nearest-neighbor connections, a hypercube has full connectivity within a row and column (Figure [27](#fig:topologies:hypercubeConnected)).
Any switches in the same row or same column can send packets with only a single hop.


![Figure 27: Hypercube with links and connections within a row/column](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/tikz/hypercube/hypercube_connected.png) 

*Figure 27: Hypercube with links and connections within a row/column*



This extra connectivity leads to greater path diversity and higher radix switches.
The cost tradeoff is that each link has lower bandwidth than a torus. 
Whereas a torus has a few fat links connecting switches, a hypercube has many thin links.
A hypercube can have more dimensions and be asymmetric, e.g.

````
topology.name = hypercube
topology.geometry = 4 5 6
````

where now we have full connections within horizontal rows, horizontal columns, and vertical columns.
Here each switch has radix 12 (3 connections in X, 4 connections in Y, 5 connections in Z). 

#### 4.2.1: Allocation and indexing<a name="subsec:hypercube:allocation"></a>



A hypercube has the same coordinate system as a torus. For example, to create a very specific, irregular allocation on a hyerpcube:

````
node {
 app1 {
  launch_cmd = aprun -n 5
  indexing = coordinate
  allocation = coordinate
  coordinate_file = coords.txt
 }
}
````
and then a coordinate file named `coords.txt`

````
5 2
0 0
0 1
1 1
2 0
3 3
````
The first line indicates 5 entries each with 2 coordinates.
Each line then defines where MPI ranks 0-4 will be placed


![Figure 28: Hypercube allocation for given set of coordinates](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/tikz/hypercube/hypercube_allocation.png) 

*Figure 28: Hypercube allocation for given set of coordinates*



#### 4.2.2: Routing<a name="subsec:hypercube:routing"></a>



Hypercubes allow very path-diverse routing because of its extra connections.
In the case of minimal routing (Figure [29](#fig:topologies:hypercubePath)), two different minimal paths from blue to red are shown.
While dimension order routing would rigorously go X then Y, you can still route minimally over two paths either randomly selecting to balance load or routing based on congestion.


![Figure 29: Minimal routing within a hypercube showing path diversity. Packet travels from blue to red, passing through green intermediate switches.](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/tikz/hypercube/hypercube_path.png) 

*Figure 29: Minimal routing within a hypercube showing path diversity. Packet travels from blue to red, passing through green intermediate switches.*



To fully maximize path diversity on adversarial traffic patterns, though, path-diverse topologies can benefit from Valiant routing.
Here, rather than directly routing to the final destination, packets first route to random intermediate switches on a minimal path.
Then they route again from the intermediate switch to the final destination also on a minimal path (Figure [30](#fig:topologies:hypercubeValiant)).
Although it increases the hop count and therefore the point-to-point latency, it utilizes more paths and therefore increases the effective point-to-point bandwidth.


![Figure 30: Valiant routing within a hypercube.  Packet travels from blue to red via a random intermediate destination shown in gray. Additional intermediate switches are shown in green.](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/tikz/hypercube/hypercube_valiant.png) 

*Figure 30: Valiant routing within a hypercube.  Packet travels from blue to red via a random intermediate destination shown in gray. Additional intermediate switches are shown in green.*





### Section 4.3: Fat Tree<a name="sec:tutorial:fattree"></a>



SST provides a very flexible fat-tree topology which allows both full bandwidth and tapered bandwidth configurations using either uniform or non-uniform switches.  
This flexibility requires a farily complicated set of input parameters which are best introduced by examining a couple of example configurations.  Consider the full-bandwidth topology in Figure~[31](#fig:topologies:fullfattree) which uses uniform 8-port switches throughout.


![Figure 31: Full-bandwidth fat-tree topology using uniform 8-port switches.](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/topologies/fattree4.pdf) 

*Figure 31: Full-bandwidth fat-tree topology using uniform 8-port switches.*



The SST fat-tree is strictly a 3-level topology, with the switch levels refered to as leaf (bottom), aggregation (middle), and core (top).
Interconnected leaf and aggregation switches form an aggregation subtree, which forms the basic unit of a fat-tree topology.
The structure of the aggregation subtree is, itself, flexible and places few constraints on the number of subtrees or the way they are connected to the core level.
In Figure~[31](#fig:topologies:fullfattree), there are 4 leaf switches and 4 aggregation switches per subtree, and each leaf switch has a concentration of four nodes per switch.
Balancing bandwidth, there are 4 ports going up from each leaf switch and 4 ports going down from each aggregation switch.
This subtree can be specified as follows:

````
topology.leaf_switches_per_subtree = 4
topology.agg_switches_per_subtree = 4
topology.concentration = 4
topology.up_ports_per_leaf_switch = 4
topology.down_ports_per_agg_switch = 4
````

In this example we have 2 aggregation subtrees.
There are four ports going up from each aggregation switch.
All of the ports on the core switches go down, so the number of core switches required (4) is only half the number of total aggregation switches (8).
This core configuration can be specified as follows:

````
topologies.num_agg_subtrees = 2
topologies.num_core_switches = 4
topologies.up_ports_per_agg_switch = 4
topologies.down_ports_per_core_switch = 8
````

Putting it all together with the topology name results in:

````
topology.name = fat_tree
logy.leaf_switches_per_subtree = 4
topology.agg_switches_per_subtree = 4
topology.concentration = 4
topology.up_ports_per_leaf_switch = 4
topology.down_ports_per_agg_switch = 4
topologies.num_agg_subtrees = 2
topologies.num_core_switches = 4
topologies.up_ports_per_agg_switch = 4
topologies.down_ports_per_core_switch = 8
````

The next example, though somewhat contrived, better demonstrates the fat-tree input flexibility.
Suppose that one wanted to use the same 8-port switches to construct a 3-level fat-tree that was both cheaper and had more endpoints (nodes), at the cost of interswitch bandwidth.
One possible configuration is shown in Figure~[32](#fig:topologies:taperedfattree).


![Figure 32: A tapered fat-tree topology using uniform 8-port switches.](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/topologies/fattree4-tapered.pdf) 

*Figure 32: A tapered fat-tree topology using uniform 8-port switches.*



Here the concentration has been increased to 6 nodes per leaf switch, leaving only two up ports per leaf switch.
Thus an aggregation subtree has a total of only 8 leaf up ports, which requires at least two aggregation switches (in order to have any ports left to connect into the core).
Each aggregation switch is then required to have 4 ports heading down.
The subtree can be configured as follows:

````
topology.leaf_switches_per_subtree = 4
topology.agg_switches_per_subtree = 2
topology.concentration = 6
topology.up_ports_per_leaf_switch = 2
topology.down_ports_per_agg_switch = 4
````

There are a total of four aggregation switches.
If the bandwidth is allowed to taper again, a single 8-port core switch can accomodate 2 ports coming up from each aggregation switch.
This core configuration can be specified as follows:

````
topologies.num_agg_subtrees = 2
topologies.num_core_switches = 1
topologies.up_ports_per_agg_switch = 2
topologies.down_ports_per_core_switch = 8
````

This is a heavily tapered tree and also has the downside of using only 6 ports per switch in the aggregation level.
This example was chosen more for its illustrative rather than practical value, though there are certainly applications where it would be perfectly adequate. 
More practical tapering becomes an option when you increase the number of ports per switch, but visualizations become more difficult to grasp.

The following constraints must be met for a valid configuration.

-   Down ports must equal up ports: leaf up ports (`leaf_switches_per_subtree` \cdot `up_ports_per_leaf_switch`) must equal aggregation down ports (`agg_switches_per_subtree` \cdot `down_ports_per_agg_switch`), and total aggregation up ports (`up_ports_per_agg_switch` \cdot `agg_switches_per_subtree` \cdot `num_agg_subtrees`) must equal total core down ports (`num_core_switches` \cdot `down_ports_per_core_switch`).
-   Need enough down ports -- each switch must have at least one link into each "unit" (subtree or switch, depending on level) below it:  `down_ports_per_core_switch` must be \geq `num_agg_subtrees`, and `down_ports_per_agg_switch` must be \geq `leaf_switches_per_subtree`.
-   Need enough up ports -- each "unit" (subtree or switch) must have at least one link into each switch above it: `up_ports_per_agg_switch` \cdot `agg_switches_per_subtree` must be \geq `num_core_switches`, and `up_ports_per_leaf_switch` must be \geq `agg_switches_per_subtree`.
-   Connections need to be regular:
  \begin{itemize}
-   `down_ports_per_core_switch` \bmod `num_agg_subtrees` must equal zero
-   `down_ports_per_agg_switch` \bmod `leaf_switches_per_subtree` must equal zero
-   `up_ports_per_leaf_switch` \bmod `agg_switches_per_subtree` must equal zero
\end{itemize}

#### 4.3.1: Switch Crossbar Bandwidth Scaling<a name="subsec:fattree:xbarbw"></a>



Allowing non-uniform switches in the topology implies that switch crossbar bandwidth should be non-uniform as well.
By default, SST assumes `switch.xbar.bandwidth` specifies the bandwidth for the switch type with the lowest port count.
The crossbar bandwidth is scaled by the total number of ports for all other switch types. 
Input keywords are provided to override this default behavior.
For the tapered-bandwidth example above, uniform switch bandwidth can be maintained by setting all bandwidth scaling to 1.0:

````
topology.leaf_bandwidth_multiplier = 1.0
topology.agg_bandwidth_multiplier = 1.0
topology.core_bandwidth_multiplier = 1.0
````

#### 4.3.2: Routing<a name="subsec:fattree:routing"></a>



The fat-tree topology should be used in conjunction with `router = fat_tree`, which will maximize the utilization of path diversity.
There is a `fat_tree_minimal` router which will use the lowest numbered valid port for any destination; this will result in poor network performance and is primarily useful for testing and perhaps experiments where network contention is desired.



### Section 4.4: Cascade<a name="sec:tutorial:cascade"></a>




![Figure 33: Schematic of cascade with three groups showing hypercube intragroup links and high bandwidth intergroup global links](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/tikz/cascade/cascade.png) 

*Figure 33: Schematic of cascade with three groups showing hypercube intragroup links and high bandwidth intergroup global links*



As bandwidth per pin increases, arguments can be made that optimal topologies should be higher radix.
A 3D torus is on the low-radix extreme while a hypercube is a high-radix extreme.
A variation on the dragonfly is the cascade topology implemented by Cray on their Aries interconnects.
A cascade is sometimes viewed as a generalization of flattened butterfly and hypercube topologies with "virtual" switches of very high radix,
not dissimilar from the fat-tree implementation with many physical commodity switches composing a single virtual switch.
The cascade topology (Figure [33](#fig:topologies:cascade)) is actually quite simple.
Small groups are connected as a generalized hypercube with full connectivity within a row or column.
Intergroup connections (global links) provide pathways for hopping between groups.
A cascade is usually understood through three parameters:

-   p: number of nodes connected to each router
-   a: number of routers in a group
-   h: number of global links that each switch has

For simplicity, only three example global links are show for clarity in the picture.
For the Cray X630, a = 96, h=10, and p=4 so that each router is connected to many other (h=10) groups.
The caveat is that in many implementations global links are grouped together for h=2 or 3 fat global links.
These demonstrate well-balanced ratios.
In general, scaling out a cascade should not increase the size of a group, only the number of groups.

#### 4.4.1: Allocation and indexing<a name="subsec:cascade:allocatoin"></a>



The cascade coordinate system is essentially the same as a 3D torus.
The group 2D hypercube layout defines X and Y coordinates.
The group number defines a Z or G coordinate.
Thus the topology in Figure [33](#fig:topologies:cascade) would be specified as

````
topology.name = cascade
topology.geometry = 3 3 3
````
for groups of size 3 X 3 with a total of 3 groups.
To complete the specification, the number of global links (h) for each router must be given

````
topology.group_connections = 10
````

#### 4.4.2: Routing<a name="subsec:cascade:routing"></a>



It is important to understand the distinction between link bandwidth, channel bandwidth, and pin bandwidth.
All topologies have the same pin bandwidth and channel bandwidth (assuming they use the same technology).
Each router in a topology is constrained to have the same number of channels (called radix, usually about k=64).
The number of channels per link changes dramatically from topology to topology.
Low radix topologies like 3D torus can allocate more channels per link, 
giving higher bandwidth between adjacent routers.
cascade is higher radix, having many more connections but having lower bandwidth between adjacent routers.
While minimal routing is often sufficient on torus topologies because of the high link bandwidth,
cascade will exhibit very poor performance with minimal routing.
To effectively utilize all the available bandwidth, packets should have a high amount of path diversity.
Packets sent between two routers should take as many different paths as possible to maximize the effective bandwidth point-to-point.


![Figure 34: Schematic of cascade showing minimal route. Traveling between groups requires routing to the correct global link, hopping the global link, then routing within a group to the correct final node.](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/tikz/cascade/cascademinroute.png) 

*Figure 34: Schematic of cascade showing minimal route. Traveling between groups requires routing to the correct global link, hopping the global link, then routing within a group to the correct final node.*



Minimal routing itself has a few complications (Figure [34](#fig:topologies:cascademinroute)).
Each router only has a few global links.  
Thus, traveling from e.g. the blue router at X=3,Y=2,G=0 to the red router at X=1,Y=2,G=2, there is no direct link between the routers.
Furthermore, there is no direct link between Groups 0 and 2.
Thus packets must route through the purple intermediate nodes.
First, the packet hops to X=3,Y=3, G=0.  
This router has a global link to Group 2, allowing the packet to hop to the next intermediate router at X=1, Y=3, G=2.
Finally, the minimal route completes by hopping within Group 2 to the final destination.


![Figure 35: Schematic of cascade showing Valiant route. Traveling between groups requires routing to a random intermediate node, then routing minimally to the final destination.](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/tikz/cascade/cascadevaliant.png) 

*Figure 35: Schematic of cascade showing Valiant route. Traveling between groups requires routing to a random intermediate node, then routing minimally to the final destination.*



To improve on minimal routing, global routing strategies are required (global routing is distinguished here from adaptive routing).  
Global essentially means "not minimal" and spreads packets along many different paths.
The simplest global routing strategy is Valiant routing, which falls in the global, oblivious category (Figure ).
Oblivious simply means packets are scattered randomly without measuring congestion.
In Valiant routing, each packet does the following:

-   Pick a random intermediate node
-   Route minimally to random node
-   Route minimally from random node to destination node
This is somewhat counterintuitive at first.
Rather than go directly to the destination node, packets go out of their way to a random node, shown in Figure  as the yellow router.
Thus, routing from the blue router in Group 0 to the red router in Group 2 first follows the minimal path (green routers) to the randomly selected yellow router in Group 1. 
From there, a second minimal path is taken through the orange routers to the final destination.
In cases with high congestion or even for large messages on a quiet network, this actually improves performance.
If a point-to-point message is composed of ten packets,
all ten packets will follow different paths to the final destination.
This essentially multiplies the maximum bandwidth by a factor of ten.
Valiant routing can be specified as

````
router = valiant
````

In contrast, UGAL routing is a global, adaptive strategy, making decisions based on congestion.
Because Valiant is oblivious, it often sends too many packets to far away random nodes.
Following a Valiant path is only relevant when enough packets fill up router queues, creating congestion.
UGAL does the following steps:

-   Start routing minimally
-   On each step, check congestion (buffer queue depth)
-   If congestion is too heavy, switch to Valiant and re-route to random intermediate node. Otherwise stay on minimal path.
UGAL packets stay on a minimal path until congestion forces them to use a Valiant strategy.
This routing can be specified as:

````
router = ugal
````







## Chapter 5: External Applications and Skeletonization<a name="chap:appsAndSkeletonization"></a>



### Section 5.1: Basic Application porting<a name="sec:skel:basic"></a>


There are three parts to successfully taking a C++ code and turning it into a scalable simulation.

-   Symbol Interception: Rather than linking to MPI, pThreads, or other parallel libraries (or even calling `hostname`), these functions must be redirected to SST-macro rather than calling the native libraries on the machine running the simulator.
You get all redirected linkage for free by using
the SST compiler wrappers `sst++` and `sstcc` installed in the `bin` folder.
-   Skeletonization: While SST-macro can run in emulation mode, executing your entire application exactly, this is not scalable.  To simulate at scale (i.e. 1K or more MPI ranks) you must strip down or "skeletonize" the application to the minimal amount of computation.  The energy and time cost of expensive compute kernels are then simulated via models rather than explicitly executed.
-   Process encapsulation: Each virtual process being simulated is not an actual physical process. It is instead modeled as a lightweight user-space thread.  This means each virtual process has its own stack and register variables, but not its own data segment (global variables).
Virtual processes share the same address space and the same global variables.  A Beta version of the auto-skeletonizing clang-based SST compiler is available with the 7.X releases. If the Beta is not stable with your application, manual refactoring may be necessary if you have global variables.

Now in Beta, another possible feature is available:

-   Memoization hooks: Rather than building an app for simulation, build an app with special profiling hooks. There is no skeletonization or redirected linkage, but the runtime or performance counters obtained by running should be used to build models for simulation.

There are generally 3 modes of using an application common with SST/macro:

-   Simulation: As lightweight as possible without sacrificing accuracy. Intended to be used for performance estimation at large scales.
-   Emulation/Virtualization: Run a parallel or distributed application within a single simulator thread/process - primarily useful for debugging.
-   Memoization: Run a full application within the simulator, collecting performance counters or timers on critical sections



|  | Simulator hooks | Symbol interception | Skeletonization | Process encapsulation |
|--|-----------------|---------------------|-----------------|-----------------------|
| Simulation | Yes | Yes | Yes | Yes |
| Virtualization | Yes | Yes | No | Yes |
| Memoization | Yes | No | No | No |


#### 5.1.1: Loading external skeletons with the standalone core<a name="subsec:externalAppStandalone"></a>


You should always write your own skeleton applications in an external folder rather then integrating directly into the `sstmac` executable.
Existing make systems can be used unmodified. Rather than producing an executable, though, SST/macro produces a shared library.
These shared libraries are then imported into the main `sstmac` executable.

If you follow the example in the `skeletons/sendrecv` folder,
the Makefile shows how to generate an importable skeleton.
If you are using `sst++`, it will automatically convert executables into loadable libraries.
If your application is named `runapp`, you would run it with `sstmac`:

````
./runapp -f parameters.ini --exe=./runapp
````
directing to load the library as a skeleton executable.

### Section 5.2: Auto-skeletonization with Clang<a name="sec:autoSkeletonization"></a>



The build of the Clang toolchain is described in Section [2.3](#sec:buildingClang). 
This enables a source-to-source translation capability in the `sst++` compiler that can auto-skeletonize computation and fix global variable references.
Some of this can be accomplished automatically (global variables), but most of it (removing computation and memory allocations) must occur through pragmas.
A good example of skeletonization can be found in the lulesh2.0.3 example in the skeletons folder. Most of the available SST pragmas are used there.
Pragmas are preferred since they allow switching easily back and forth between skeleton and full applications.
This allows much easier validation of the simulation. The section here briefly introduces the SST pragma language.
A complete tutorial on all available pragmas is given in Chapter [6](#clangTutorial).

\subsection{Redirecting Main}
Your application's `main` has to have its symbols changed.
The simulator itself takes over `main`.
SST-macro therefore has to capture the function pointer in your code and associate it with a string name for the input file.
This is automatically accomplished by defining the macro `sstmac_app_name` either in your code or through a `-D=` build flag to the name of your application (unquoted!). The value of the macro will become the string name used for launching the application via `node.app1.name=X`.
Even without Clang, this works for C++. For C, Clang source-to-source is required.

\subsection{Memory Allocations}
To deactivate memory allocations in C code that uses `malloc`, use:
````
#pragma sst malloc
  void* ptr = malloc(...)
````
prior to any memory allocations that should be deactivated during skeleton runs, but active during real runs.

Similarly, for C++ we have
````
#pragma sst new
  int* ptr = new int[...]
````

\subsection{Computation}
In general, the SST compiler captures all `#pragma omp parallel` statements.
It then analyzes the for-loop or code block and attempts to derive a computational model for it.
The computational models are quite simple (skeleton apps!), 
based simply on the number of flops executed and the number of bytes read (written) from memory.
Consider the example:

````
double A[N], B[N];
#pragma omp parallel for
for (int i=0; i < N; ++i){
  A[i] = alpha*A[i] + B[i];
}
````
The SST compiler deduces 16N bytes read, 8N bytes written, and 16N flops (or 8N if fused-multiplies are enabled).
Based on processor speed and memory speed, it then estimates how long the kernel will take without actually executing the loop.
If not wanting to use OpenMP in the code, `#pragma sst compute` can be used instead of `#pragma omp parallel`.

\subsection{Special Pragmas}
Many special cases can arise that break skeletonization.
This is often not a limit of the SST compiler, but rather a fundemental limitation in the static analysis of the code.
This most often arises due to nested loops. Consider the example:

````
#pragma omp parallel for
for (int i=0; i < N; ++i){
  int nElems = nElemLookup[i];
  for (int e=0; e < nElems; ++e){
  }
}
````
Auto-skeletonization will fail. The skeletonization converts the outer loop into a single call to an SST compute model.
However, the inner loop can vary depending on the index.
This data-dependency breaks the static analysis.
To fix this, a hint must be given to SST as to what the "average" inner loop size is.
For example, it may loops nodes in a mesh. In this case, it may almost always be 8.

````
#pragma omp parallel for
for (int i=0; i < N; ++i){
  int nElems = nElemLookup[i];
  #sst replace nElems 8
  for (int e=0; e < nElems; ++e){
  }
}
````
This hint allows SST to skeletonize the inner loop and "guess" at the data dependency.



#### 5.2.1: Skeletonization Issues<a name="subsec:skeletonIssues"></a>


Skeletonization challenges fall into three main categories:


-   Data structures - Memory is a precious commodity when running large simulations, so get rid of every memory allocation you can.
-   Loops - Usually the main brunt of CPU time, so get rid of any loops that don't contain MPI calls or calculate variables needed in MPI calls.
-   Communication buffers - While you can pass in real buffers with data to SST-macro MPI calls and they will work like normal, it is relatively expensive. If they're not needed, get rid of them.





The main issue that arises during skeletonization is data-dependent communication.  
In many cases, it will seem like you can't remove computation or memory allocation because MPI calls depend somehow on that data.  
The following are some examples of how we deal with those:


-   Loop convergence - In some algorithms, the number of times you iterate through the main loop depends on an error converging to near zero, or some other converging mechanism.  This basically means you can't take out anything at all, because the final result of the computation dictates the number of loops.  In this case, we usually set the number of main loop iterations to a fixed number.
-   Particle migration - Some codes have a particle-in-cell structure, where the spatial domain is decomposed among processes, and particles or elements are distributed among them, and forces between particles are calculated.  When a particle moves to another domain/process, how many particles migrate and how far depends on the actual computed forces. However, in the skeleton, we are not actually computing the forces - only estimated how long the force computation took.  If all we need to know is that this migration/communication happens sometimes, then we can just make it happen every so many iterations, or even sample from a probability distribution.
-   AMR - Some applications, like adaptive mesh refinement (AMR), exhibit communication that is entirely dependent on the computation.  In this case, skeletonization again depends on making approximations or probability estimates of where and when box refinement occurs without actually computing everything.

For applications with heavy dynamic data dependence, we have the following strategies:

-   Traces  - revert to DUMPI traces, where you will be limited by existing machine size.  Trace extrapolation is also an option here.
-   Synthetic - It may be possible to replace communication with randomly-generated data and decisions, which emulate how the original application worked. This occurs in the CoMD skeleton.
-   Hybrid - It is possible to construct meta-traces that describe the problem from a real run, and read them into SST-macro to reconstruct the communication that happens.  This occurs in the `boxml` aplications.

### Section 5.3: Process Encapsulation<a name="sec:processEncapsulation"></a>


As mentioned above, virtual processes are not real, physical processes inside the OS.
They are explicitly managed user-space threads with a private stack, but without a private set of global variables.
When porting an application to SST/macro, global variables used in C programs will not be mapped to separate memory addresses causing incorrect execution or even segmentation faults.
If you have avoided global variables, there is no major issue.  
If you have read-only global variables with the same value on each machine, there is still no issue.
If you have mutable global variables, you should use the `sst++` clang-based compiler wrappers to auto-refactor your code (Section [5.2](#sec:autoSkeletonization)).
This feature is current labeled Beta, but is stable for numerous tests and will be fully supported for release 7.1.




## Chapter 6: Clang Source-to-Source Auto-Skeletonization via Pragmas<a name="clangTutorial"></a>



There are three main examples of auto-skeletonization with pragmas in the SST-macro source code in the `skeletons` directory.
These applications are Lulesh, HPCG, and CoMD.
The auto-skeletonizing compiler is designed to do three main things:


-   Redirect global variable accesses to thread-specific values
-   Turn off large memory allocations that would prevent scalable simulation
-   Estimate time of compute-intensive kernels instead of executing them

\section{Pragma Overview}


![Figure 36: Source-to-source transformation workflow for SST compiler. For C source files, g++ can be swapped with gcc. The choice of underlying compiler is actually arbitrary and can be clang, gcc, icc, etc.](https://github.com/sstsimulator/sst-macro/blob/devel/docs/manual/figures/compilerWorkflow) 

*Figure 36: Source-to-source transformation workflow for SST compiler. For C source files, g++ can be swapped with gcc. The choice of underlying compiler is actually arbitrary and can be clang, gcc, icc, etc.*



\subsection{Compiler workflow}
The source-to-source compiler operates on a pre-processed source file.
The source code transformation generates a temporary source file.
This temporary source file is then compiled into the target object file.
Global variables require static registration of C++ variables.
Here another temporary C++ source file (even if the original file is C)
is generated that has all static global variable registrations.
The corresponding object file is merged with the original object file,
creating a complete SST-macro object file with the transformed code and C++ static registrations.
This workflow is shown in Figure [36](#fig:compilerWorkflow).

\subsection{Compiler Environment Variables}

\subsubsection{SSTMAC\_SRC2SRC: Default 1}
If set to zero, deactivates the source-to-source transformation. 
The compiler wrapper will then compile the code into the simulator, but will not redirect any global variable accesses or perform any skeletonization.

\subsubsection{SSTMAC\_SKELETONIZE: Default 0}
If set to zero, deactivates skeletonization. 
This does not deactivate global variable redirection.
Thus, with `SSTMAC_SRC2SRC=1` and `SSTMAC_SKELETONIZE=0`,
SST-macro will act as an MPI emulator executing a full code but with global variables refactored to maintain correctness.

\subsubsection{SSTMAC\_MEMOIZE: Default 0}
If set to nonzero, activates memoization hooks. 
This deactivates all global variable refactoring, all symbol interception, and all skeletonization.

\subsubsection{SSTMAC\_HEADERS: No default}
The compiler wrapper will only redirect global variables that it knows should definitely be modified.
All global variables found in source files will be redirected.
`extern` global variables found in header files are more difficult.
Certain system global variables like `stderr` should not be modified and so are left as global variable constants.
By default, global variables in a header file are NOT redirected unless explicitly specified in a header configuration file.
The variable `SSTMAC_HEADERS` should give the full path of a file containing the list of header files.
Header file paths in the file should be one per line and should be the full path, not a relative path.

\subsubsection{SSTMAC\_DELETE\_TEMPS: Default 1}
If non-zero, the compiler cleans up all temporary files. 
If you wish to keep temporary files to view them for debugging, set to zero.
All temporary, intermediate source files will otherwise be deleted at the end of compilation.

\section{Basic Replacement Pragmas}
When skeletonization is active (see `SSTMAC_SKELETONIZE`), these pragmas will cause replacements in the original source code.
Pragmas appy to the next statement in the source code.
For compound statements such as a for-loop with a multi-statement body, the pragma applies to the entire for-loop.
\subsection{pragma sst delete: no arguments}
This deletes the next statement from the source code.
If the statement declares a variable that is used later in the code, this will cause a compile error.
Consider an example from the Lulesh source code.

````
#pragma sst delete
    testnorms_data.values[i] = normr/normr0;
````
In the skeleton, the residual is not actually computed and the `testnorms_data` array is not actually allocated.
Thus this statement should be deleted and not actually executed in the skeleton.

\subsection{pragma sst replace [to\_replace:string] [new\_text:C++ expression]}
This applies a string replace to a variable or function call in the next statement.
Consider an example from Lulesh.

````
#pragma sst replace volo 1
   deltatime() = (Real_t(.5)*cbrt(volo(0)))/sqrt(Real_t(2.0)*einit);
````
The function call `volo(0)` is not valid in the skeleton since volumes are not actually computed.
Here we simply estimate that all cells have unit volume replacing `volo(0)` with `1`.

\subsection{pragma sst init [new\_value:string]}
This pragma can only apply to a binary equals operator assigning a value.
The pragma changes the right-hand side to use the given new value.
For example, in Lulesh:

````
#pragma sst init nullptr
  destAddr = &domain.commDataSend[pmsg * maxPlaneComm] ;
````
The send buffer `domain.commDataSend` is not allocated in the skeleton and thus is not valid to use.
The pragma causes the skeleton to simply set `destAddr` to `nullptr`.

\subsection{pragma sst return [new\_value:C++ expression]}
Pragma is equivalent to `pragma sst init`. This replaces the target of a return statement with the given expression.
This produces a compiler error if applied to anything but a return statement.

\subsection{pragma sst keep}
During the skeletonization process, some transformations occur automatically even without pragmas. 
For example, all MPI calls have input buffers converted to null pointers to indicate a simulated MPI call.
If the MPI call should be emulated with real payloads, the MPI call must be explicitly marked with `pragma keep`.
An example can be found in the HPCG skeleton:

````
#pragma sst keep
  MPI_Allreduce(&localNumberOfNonzeros, &totalNumberOfNonzeros, ...)
````
The actual allreduce operation is carried out, summing the the local number into the total number of nonzeroes.

\subsection{pragma sst keep\_if [condition:C++ bool expression]}
More control over whether transformations are skipped is provided by `keep_if`.
An example is found in CoMD.

````
#pragma sst keep_if count < 16
   MPI_Allreduce(sendBuf, recvBuf, count, MPI_DOUBLE, MPI_SUM, MPI_COMM_WORLD);
````
Any small allreduce operations are kept. 
Any allreduce operations larger than a given cutoff are simulated without emulating the actual buffer operations.

\subsection{pragma sst empty}
This pragma is applied to functions. The function prototype is kept, but an empty body is inserted instead of the actual code.
This can be useful for deleting large blocks of computation inside a function.

\subsection{pragma sst branch\_predict [condition:C++ expression]}
The branch prediction pragma can be applied in two different contexts.
We will revisit the pragma in the context of compute skeletonization below.
The branch prediction pragma should only be applied to an if-statement.
Much like the replace pragmas, it swaps out the given if condition with a new expression.

The branch prediction pragmas become necessary when skeletonizing.
Certain values may not be computed or certain variables marked null.
If these values are then used in an if predicate,
the skeletonizing compiler cannot deduce the correct behavior for the application.
When an ambiguous predicate is found, the compiler will usually print a warning and just assume the predicate is false.
Predicates are often almost always true or almost always false. 
Thus most uses of this pragma will simply supply `true` or `false` as the replacement.
However, any arbitrary C++ boolean expression can be given as the replacement.
The new predicate expression (like the original), must not involve any null variables.

\section{Memory Allocation Pragmas}
\subsection{pragma sst malloc}
Applied to any statement in which the right-hand side is a malloc. This sets the left-hand side to `NULL`.
This is critical for turning off large memory allocations on data structures not required for control-flow.


\subsection{pragma sst new}
Applied to any statement in which the right-hand side a C++ operator new. This sets the left-hand side to `nullptr`.
This is critical for turning off large memory allocations on data structures not required for control-flow.

\section{Data-Driven Type Pragmas}
\subsection{pragma sst null\_variable}
This applies to variable declarations. If pragma is not applied to a declaration, a compiler error is given.
A null variable is one in which all operations involving the variable should be deleted.
This usually applies to large data arrays that should never be allocated and therefore never dereferenced.
An example can be seen in CoMD:

````
#pragma sst null_variable
   int* nAtoms;         //!< total number of atoms in each box
````
The array is not allocated and all statements operating on the array are deleted.

This pragma is much more powerful than simply using `pragma sst new`.
`pragma sst new` simply turns off a given memory allocation setting it to a null value.
If the array is dereferenced later in code, this causes a segmentation fault.
By marking a declaration as null, the compiler can flag where segmentation faults would occur when running the skeleton.

In most cases, all operations involving the null variable are deleted.
However, there may be cases where the compiler may decide deleting an operation cannot be done automatically since
it may affect control flow, e.g., if the variable is used inside an if-statement.
When this occurs, a compiler error is thrown flagging where the ambiguity occurs.
Another pragma must then be applied to that statement to tell the compiler how to proceed.

\subsection{pragma sst null\_type [type alias] [list allowed functions]}
This applies to C++ class variable declarations. If pragma is not applied to a declaration, a compiler error is given.
This essentially works the same way as `null_variable`, but allows certain member functions to be kept instead of deleted.
Consider an example from Lulesh:

````
#pragma sst null_type sstmac::vector size resize empty
   std::vector<Real_t> m_x ;  /* coordinates */
```` 
Here we wish to indicate the vector is "null" and should not actually allocate memory or allow array accesses.
However, we still wish to track the vector size and whether it is empty.
The first argument to the pragma is a new type name that implements the "alias" functionality.
For `std::vector`, SST-macro automatically provides the alias.
For illustration, that code is reproduced here:

````
namespace sstmac {
class vector {
 public:
  void resize(unsigned long sz){
    size_ = sz;
  }

  unsigned long size() const {
    return size_;
  }

  template <class... Args>
  void push_back(Args... args){
    ++size_;
  }

  template <class... Args>
  void emplace_back(Args... args){
    ++size_;
  }

  bool empty() const {
    return size_ == 0;
  }

 private:
  unsigned long  size_;
};
}
````
This empty vector class allows the type to track its size, but not actually hold any data.
All places in the Lulesh code that an `std::vector` is used are substituted with the new type.

The remaining arguments to the pragma are the list of functions we wish to mark as valid.
In this case, even though the alias vector class provides more functions, we only allow `size`, `resize`, and `empty` to be called.



\section{Compute Pragmas}
\subsection{pragma sst compute and pragma omp parallel}
Compute-intensive should not be executed natively.
Instead, a compute model should be used to estimate the elapsed time.
Compute model replacements are automatically triggered by any OpenMP parallel pragmas.
The corresponding block or for-loop is not executed, instead calling out to a compute model to estimate time.
Currently, compute modeling is done via a very basic static analysis.
The static analysis attempts to count the number of integer and floating point operations.
It also estimates the number of memory reads and writes.
These four counters are passed to a coarse-grained processor model for time estimates.
For more details, see `sstmac_compute_detailed` in the source code.
Numerous examples can be found in the Lulesh, HPCG, and CoMD skeleton applications.

\subsection{pragma sst loop\_count [integer: C++ expression]}
If the `sst compute` or `omp parallel` pragma are applied to an outer loop with one or more inner loops,
the compute model static analysis might fail.
This occurs when the inner loop control flow depends on the actual execution.
Any variables declared inside the compute block are not valid to use in the compute estimate since they will be skeletonized and deleted.
Only variables in scope at the beginning of the outer loop are safe to use in compute modeling.

When the static analysis fails, a corresponding compiler error is thrown.
This usually requires giving a loop count hint.
Consider the example from HPCG:

````
#pragma omp parallel for
  for (local_int_t i=0; i< localNumberOfRows; i++) {
    int cur_nnz = nonzerosInRow[i];
   #pragma sst loop_count 27
    for (int j=0; j<cur_nnz; j++) mtxIndL[i][j] = mtxIndG[i][j];
  }
````
The static analysis fails on `cur_nnz`.
However, that value is almost always 27.
Thus we can safely tell the compiler to just assume the loop count is 27.

\subsection{pragma sst branch\_predict [float: C++ expression]}
Similar to the way that loop counts can break the static analysis, if statements inside a loop skeletonized with `omp parallel` or `sst compute` can also be problematic.
If the predicate depends on a variable declared inside the skeletonzied block,
the static analysis will break since it cannot predict when and how often to assume true or false.
In contrast to the branch prediction pragma previously used, branch prediction pragmas inside a compute block must give a number between 0 and 1.
This can either be a literal float or expression that computes a float value.
Consider an example from CoMD:

````
#pragma sst branch_predict 0.2
  if(r2 <= rCut2 && r2 > 0.0){
````
Inside the compute block, a compute may or may not occur depending on whether a particle distance is less than a cutoff.
Based on the way CoMD constructs unit cells and halo regions, running CoMD shows that about 1 in 5 neighbor interactions are actually below the cutoff.
Thus we given the branch prediction the hint 0.2.

\subsection{pragma sst advance\_time [units] [time to advance by]}
This pragma advances the simulator time by the specified amounts of time. It can be placed before any statement. The units can be the following: sec, msec, usec or nsec for Seconds, milliseconds, microseconds and nanoseconds respectively. 



### Section 6.1: Memoization pragmas<a name="sec:memoization"></a>



\subsection{Memoization models}
To understand the memoization pragmas, we first introduce how models get constructed in the SST/macro runtime.
Source-to-source transformations based on the pragmas causes the following hooks to get inserted:

````
int sstmac_startMemoize(const char* token, const char* model);
void sstmac_finish_memoize0(int tag, const char* token);
void sstmac_finish_memoize1(int tag, const char* token, double p1);
void sstmac_finish_memoize2(int tag, const char* token, double p1, double p2);
...
````
A start call begins a memoization region for a specific name.
The start function must return an integer tag identifying the memoization instance.
This tag gets passed back into the finish function above.
This is primarily useful for thread-safe collection, but can be generally more useful.
The finish functions take input parameters. 
Given input parameters x,y causes a function F(x,y) to be fit to the timer or performance counters.

If building a skeleton application that uses memoization data, a different hook gets inserted:
````
void sstmac_compute_memoize0(const char* token);
void sstmac_compute_memoize1(const char* token, double p1);
void sstmac_compute_memoize2(const char* token, double p1, double p2);
...
````
Assuming a model F(x,y) has been fit in a memoization pass,
that function is invoked with the given parameters to estimate a time or performance counter.

Memoization models are implemented by inheriting from a standard class

````
struct RegressionModel {
...
virtual double compute(int n_params, const double params[], ImplicitState* state) = 0;
virtual int StartCollection() = 0;
virtual void finishCollection(int n_params, const double params[], ImplicitState* state) = 0;
...
````
A call to `sstmac_finish_memoize2` causes `finishCollection(2,..)` to get invoked on the model.
The `states` object is discussed more later in [6.1.1](#subsec:implicitStates).
For now, `compute` only returns a double (total time).
Generalized performance models are planned for future versions.
Models are registered using the SST/macro factory system. 
If wanting to add a least-squares model, factory register as:

````
struct least_squares : public regression model {
 FactoryRegister("least_squares", OperatingSystem::RegressionModel, least_squares)
````

\subsection{pragma sst memoize [skeletonize(...)] [model(...)] [inputs(...)] [name(...)]}

-   skeletonize: boolean for whether code block should still be executed or remove entirely (default: true)
-   model: string name for a type of model (e.g. linear, kmeans) specifying which model to construct and fit (no default)
-   inputs: a comma-separated list of C++ expressions that are the numeric inputs
-   name: a unique name to use for identifying the memoization region (default: see below)
If the `name` parameter is not given, the file and line number is used for basic expressions while the function name is used if applied to a function.
Consider the example:

````
#pragma sst memoize skeletonize(true) model(least_squares) inputs(ncol,nlink,nrow) 
void dgemm(int ncol, int nlink, int nrow, double* left, double* right);
````
When running the memoization pass, the memoization hooks get invoked as:

````
int tag = sstmac_startMemoize("dgemm", "least_squares");
dgemm(....);
sstmac_finish_memoize3(tag, "dgemm", ncol, nlink, nrow);
````
With `skeletonize` set to true, the skeleton app would be:

````
sstmac_computeMemoize("dgemm", ncol, nlink, nrow);
````
With skeletonize set to false:

````
sstmac_computeMemoize("dgemm", ncol, nlink, nrow);
dgemm(...);
````
Both the memoization function and the original function would both get invoked.

#### 6.1.1: pragma sst implicit\_state X(Y) ...<a name="subsec:implicitStates"></a>


The implicit state pragma sets certain hardware or software states not captured by the inputs to the memoization pragma.
This might involve DVFS states, different runs of a task in which data is "cold" or "hot" in cache, or different types of cores.
The implicit state lasts for the scope of the statement:

````
#pragma sst implict_state ...
{
 //all statements here have that state
}

#pragma sst ImplicitState ...
fxn(...) //implicit state lasts the entire function
````

The arguments to the pragma are best understood by example:

````
#pragma sst ImplicitState dvfs(1) cache(hot)
fxn(...)
````
This causes a source code transformation to:

````
sstmac_set_ImplicitState2(dvfs,1,cache,hot);
fxn(...);
sstmac_unset_ImplicitState2(dvfs,cache);
````
For now, the functions take integer arguments (this may get relaxed to arbitrary strings).
Thus, e.g. enums must be available or compilation will fail:

````
enum states {
 dvfs=0,
 cache=1
};
enum cache_states {
 cold=0,
 hot=1
};
````

If a `sstmac_finish_memoize` function got invoked, the states could be read.
The class `ImplicitState` is a base class only and carries no data by default.
Specific memoization models are intended to be used only with known implicit state classes.
As such, the memoization model `collect`, etc, functions must dynamic cast to an expected type.
A library of standard implicit state implementations is planned for future releases.



\chapter{Issues and Limitations}
\section{Polling in applications}

Use of probe non-blocking functions in a loop, such as:


````
while(!flag){
 MPI_Iprobe( 0, 0, MPI_COMM_WORLD, &flag, &status );
}
````
creates problems for the simulation. Virtual time never advances in the MPI\_Iprobe call. 
This causes an infinite loop that never returns to the discrete event manager. 
Even if configured so that time progresses, the code will work but will take a very long time to run.

### Section 6.2: Fortran<a name="subsec:issues:fortran"></a>



SST-macro previously provided some experimental support for Fortran90 applications. 
This has been discontinued for the foreseeable future.
For profiling existing apps written with Fortran, DUMPI traces can still be generated. 







## Chapter 7: Detailed Parameter Listings<a name="chapter:parameters"></a>


The following chapter is organized by parameter namespaces. Tables in each namespace are organized as



| Name (type) | Default if not given | Allowed  Values | Description |
|-------------|----------------------|-----------------|-------------|
|             |                      |                 |             |

which lists the possible parameter names, allowed values, and brief descriptions.
More detailed descriptions of particular parameter values are found in the documentation in previous chapters.

The allowed parameter types are:


| int | Any integer |
|-----|-------------|
| long | Any integer value, but guaranteed not to overflow for long integers |
| bool | Either "true" or "false" as lowercase string |
| time | Any valid float value followed by time units (s,ms,us,ns,ps) |
| freq | Any valid float value followed by frequency units (Hz, MHz, GHz) |
| bandwidth | Any valid float value followed by bandwidth units (b/s, B/s, Mb/s, MB/s, etc) |
| byte length | Any positive integer followed by length units (B, KB, MB, GB,TB) |
| string | An arbitrary string |
| vector of X | A vector of type X with entries separated by spaces |
| filepath | A valid filepath to an existing file, either absolute or relative |
| quantiy | A catch-all for a quantity with units. Any of frequency, bandwidth, byte length, or time can be given |

### Section 7.1: Global namespace<a name="sec:globalParams"></a>




| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| sst\_nthread (int) | 1 | Positive int | Only relevant for multi-threading. Specifying more threads than cores can lead to deadlock. |
| timestamp\_resolution (time) | 1ps |  | Specifies the length of time occupied by 1 timestamp tick - the smallest resolvable time difference. Numerical stability depends on this parameter matching the time scales of the simulation. |
| serialization\_buffer\_size (byte length) | 16 KB |  | Size to allocate for buffering point-to-point sends in parallel. This should set be large enough to handle serialization of all messages in a given time window, but not so large that significant space is wasted. |
| backup\_buffer\_size (byte length) | 1 MB |  | Size to allocate for special overflow buffers when the standard buffer is overrun. This is the base size and continues to grow if buffers overflow again in a time window. This should be large enough so that buffers do not continuously overflow, but not so large that memory gets filled up. |
| cpu\_affinity (vector of int) | No default | Invalid cpu IDs give undefined behavior | When in multi-threading, specifies the list of core IDs that threads will be pinned to. |

### Section 7.2: Namespace "topology"<a name="sec:topologyParams"></a>




| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| geometry (vector of int) | No default | See Topology section | Geometry configuration of the topology. For details of the keyword, users should refer to Section [4](#chapter:topologies) |
| auto (bool) | false | Whether to auto-generate the topology based on the application size. |
| name (string) | No default | torus, cascade, dragonfly, fat\_tree, crossbar, tapered\_fat\_tree | The name of the topology to build. For details, see Section [4](#chapter:topologies) |
| seed (long) | System time |  | If no value given, random numbers for topology will be generated from system time |
| concentration (int) | 1 | Positive int | The number of nodes per network switch. For indirect networks, this is the number of nodes per leaf switch. |
| num\_leaf\_switches (int) | No default | Positive int | Only relevant for fat trees. This is the number of switches at the lowest level of the tree that are connected to compute nodes. Depending on how the fat tree is specified, this number may not be required. |
| k (int) | No default | int >= 2 | The branching fraction of a fat tree. k=2 is a binary tree. k=4 is a quad-tree. |
| l (int) | No default | Positive int | The number of levels in a fat tree. |
| num\_inj\_switches\_per\_subtree | No default | Positive int | For a tapered tree, the number of injection switches, N(inj), within an aggregation tree that connect directly toc ompute nodes. |
| num\_agg\_switches\_per\_subtree | No default | Positive int | For a tapered tree, the number of aggregations witches per aggregation tree linking injection switches to the core. |
| num\_agg\_subtrees | No default | Positive int | For a tapered fat tree with 3 levels (injection, aggregation, core), this gives the number, N(agg), of aggregation subtrees. To find the total number, N(tot) of injection (leaf) switches, we have N(tot) = N(agg) X N(inj). |
| num\_core\_switches | No default | Positive int | The total number of core switches in a tapered tree linking the individual aggregation trees. |
| group\_connections (int) | No default | Positive int | For cascase ir dragonfly, the number of intergroup connections on each switch in a Dragonfly group |
| redundant (vector of int) | vector of 1's | Positive ints | For Cartesian topologies (hypercube, cascadem, dragonfly, torus) this specifies a bandwidth (redundancy) multiplier for network links in each dimension. |

### Section 7.3: Namespace "node"<a name="sec:nodeParams"></a>




| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| name (string) | simple | simple | The type of node model (level of detail) for node-level operations |
| services (vector of strings) | Empty | Valid service names | For  details, see section on distributed services in developer's manual. Advanced feature. |

#### 7.3.1: Namespace "node.nic"<a name="subsec:node:nic:Params"></a>




| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| name (string) | No default | pisces, logP | The type of NIC model (level of detail) for modeling injection of messages (flows) to/from the network. |
| packetizer (string) | cut\_through | merlin, simple, cut\_through | The type of packetizer for injecting flows into the network. Merlin is part of sst-elements. Simple and cut-through use PISCES |
| negligible\_size (byte length) | 256B |  | Messages (flows) smaller than size will not go through detailed congestion modeling. They will go through a simple analytic model to compute the delay. |

##### Namespace "node.nic.delay\_histogram"<a name="subsubsec:node:nic:delayHistogram:Params"></a>



| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| fileroot (string) | No default |  | The path of a file to be created (or overwritten) where results from the statistics will be placed. |
| bin\_size (quantity) | No default | Positive value | For the quantity being collected (bytes,time,etc), this determines how large a value range should be binned together. |
| num\_bins (int) | 20 | Positive int | Determines the number of initial bins to allocate. If number is too small, simulation will allocate more. This is just a guess for optimization. |
| logarithmic (bool) | false |  | Whether to use a logarithmic axis for binning. |


##### Namespace "node.nic.congestion\_spyplot"<a name="subsubsec:node:nic:congestionSpyplot:Params"></a>



| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| fileroot (string) | No default |  | The path of a file to be created (or overwritten) where results from the statistics will be placed. |
| type (string) | ascii | ascii, png | Whether to generate a simple traffic matrix in an ASCII file or to generate a PNG image of the traffic pattern |
| normalization (long) | Ignored | Positive int | Determines a normalization value that will effect the color scales of an output PNG file. By default, if not specified, the spyplot will determine the largest value and normalize all values for the output PNG to be 0.0-1.0. Values larger than 1.0 will have the same color as 1.0. Useful for having two PNG files with different values have the same color scales. |


##### Namespace "node.nic.traffic\_matrix"<a name="subsubsec:node:nic:trafficMatrix:Params"></a>



| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| fileroot (string) | No default |  | The path of a file to be created (or overwritten) where results from the statistics will be placed. |
| type (string) | ascii | ascii, png | Whether to generate a simple traffic matrix in an ASCII file or to generate a PNG image of the traffic pattern |
| normalization (long) | Ignored | Positive int | Determines a normalization value that will effect the color scales of an output PNG file. By default, if not specified, the spyplot will determine the largest value and normalize all values for the output PNG to be 0.0-1.0. Values larger than 1.0 will have the same color as 1.0. Useful for having two PNG files with different values have the same color scales. |


##### Namespace "node.nic.local\_bytes\_sent"<a name="subsubsec:node:nic:localSent:Params"></a>



| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| fileroot (string) | No default |  | The path of a file to be created (or overwritten) where results from the statistics will be placed. |


##### Namespace "node.nic.global\_bytes\_sent"<a name="subsubsec:node:nic:globalSent:Params"></a>



| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| fileroot (string) | No default |  | The path of a file to be created (or overwritten) where results from the statistics will be placed. |


##### Namespace "node.nic.message\_size\_histogram"<a name="subsubsec:node:nic:sizeHistogram:Params"></a>



| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| fileroot (string) | No default |  | The path of a file to be created (or overwritten) where results from the statistics will be placed. |
| bin\_size (quantity) | No default | Positive value | For the quantity being collected (bytes,time,etc), this determines how large a value range should be binned together. |
| num\_bins (int) | 20 | Positive int | Determines the number of initial bins to allocate. If number is too small, simulation will allocate more. This is just a guess for optimization. |
| logarithmic (bool) | false |  | Whether to use a logarithmic axis for binning. |


##### Namespace "node.nic.ejection"<a name="subsubsec:node:nic:ejection:Params"></a>


These parameters do not need to be specified, but can be given.
Generally, the simulation assumes an infinite buffer size (unlimited memory) and no latency.
All other parameters can be filled in from `node.nic.injection`.


| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| stats (string) | null | delay\_histogram,   congestion\_spyplot,   multi, 	null | The type of statistics to collect from packets leaving or arriving at the NIC.    Null indicates no statistics collected. For details on the other types of statistics, see Section [3.13](#sec:tutorials:packetStats) |
| callbacks (vector of string) | No default | Any valid stats name | If `stats` is set to `multi`,then multiple different stats can be given here |
| arbitrator (string) | cut\_through | null, simple, cut\_through | Bandwidth arbitrator for PISCES congestion modeling. Null uses simple delays with no congestion. Simple uses store-and-forward that is cheap to compute, but can have severe latency errors for large packets. Cut-through approximates pipelining of flits across stages. |
| latency (time) | No default |  | If given, overwrites the send and credit latency parameters. Depending on component, the entire latency may be put on either the credits or the send. |
| bandwidth | No default |  | The bandwidth of the arbitrator |
| send\_latency (time) | No default |  | The latency to send a packet to the next stage in the network. This can be omitted if the generic latency parameter is given (see above). |
| credit\_latency (time) | No default |  | The latency to send a credit to the previous network stage. This can be omitted if the generic latency parameter is given (see above). |
| credits (byte length) | No default |  | The number of initial credits for the component. Corresponds to an input buffer on another component. In many cases, SST/macro can compute this from other parameters and fill in the value. In some cases, it will be required. |
| mtu (byte length) | 1024B |  | The packet size. All messages (flows) will be broken into units of this size. |

##### Namespace ``node.nic.injection"<a name="subsubsec:node:nic:injection:Params"></a>




| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| stats (string) | null | delay\_histogram, 	null | The type of statistics to collect from packets leaving or arriving at the NIC.    Null indicates no statistics collected. For details on the other types of statistics, see Section [3.13](#sec:tutorials:packetStats) |
| arbitrator (string) | cut\_through | null, simple, cut\_through | Bandwidth arbitrator for PISCES congestion modeling. Null uses simple delays with no congestion. Simple uses store-and-forward that is cheap to compute, but can have severe latency errors for large packets. Cut-through approximates pipelining of flits across stages. |
| latency (time) | No default |  | If given, overwrites the send and credit latency parameters. Depending on component, the entire latency may be put on either the credits or the send. |
| bandwidth | No default |  | The bandwidth of the arbitrator |
| send\_latency (time) | No default |  | The latency to send a packet to the next stage in the network. This can be omitted if the generic latency parameter is given (see above). |
| credit\_latency (time) | No default |  | The latency to send a credit to the previous network stage. This can be omitted if the generic latency parameter is given (see above). |
| credits (byte length) | No default |  | The number of initial credits for the component. Corresponds to an input buffer on another component. In many cases, SST/macro can compute this from other parameters and fill in the value. In some cases, it will be required. |
| mtu (byte length) | 1024B |  | The packet size. All messages (flows) will be broken into units of this size. |


#### 7.3.2: Namespace ``node.memory"<a name="subsec:node:memory:Params"></a>




| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| model (string) | No default | logP, pisces | The type of memory model (level of detail) for modeling memory transactions. |
| arbitrator (string) | cut\_through | null, simple, cut\_through | The type of arbitrator. See arbitrator descriptions above. |
| latency (time) | No default |  | The latency of single memory operation |
| total\_bandwidth | No default |  | The total memory bandwidth possible across all memory controllers. |
| max\_single\_bandwidth | Computed |  | The maximum memory bandwidth of a single stream of requests. Essentially the bandwidth of a single memory controller. If not given, this defaults the value of total\_bandwidth. |

#### 7.3.3: Namespace "node.os"<a name="subsec:node:os:Params"></a>




| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| compute\_scheduler (string) | simple | simple, cpuset | The level of detail for scheduling compute tasks to cores. Simple looks for any empty core. cpuset allows bitmasks to be set for defining core affinities. |
| stack\_size (byte length) | 64 KB |  | The size of user-space thread stack to allocate for each virtual application |
| stack\_chunk\_size (byte length) | 1 MB |  | The size of memory to allocate at a time when allocating new thread stacks. Rather than allocating one thread stack at a time, multiple stacks are allocated and added to a pool as needed. |

##### Namespace ``node.os.call\_graph"<a name="subsubsec:node:os:callGraph:Params"></a>



| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| fileroot (string) | No default |  | The path of a file to be created (or overwritten) where results from the statistics will be placed. |

##### Namespace ``node.os.ftq"<a name="subsubsec:node:os:ftq:Params"></a>



| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| fileroot (string) | No default |  | The path of a file to be created (or overwritten) where results from the statistics will be placed. |
| epoch (time) | No default |  | The length of simulation time to treat as a single interval. This is much like a histogram bin, except the bin is partitioned into multiple categories. If too small, not enough data will be collected per interval to give reasonable looking results. If too large, significant changes in system activity over time will not be resolved. |

#### 7.3.4: Namespace ``node.proc"<a name="subsec:node:proc:Params"></a>



| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| ncores (int) | No default | Positive int | The number of cores contained in a processor (socket). Total number of cores for a node is ncores X nsockets. |
| frequency | No default |  | The baseline frequency of the node |
| parallelism (double) | 1.0 | Positive number | Fudge factor to account for superscalar processor. Number of flops per cycle performed by processor. |

### Section 7.4: Namespace "mpi"<a name="sec:mpi:Params"></a>




| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| test\_delay (time) | 0 |  | The minimum time spent by MPI on each MPI\_Test call |
| iprobe\_delay (time) | 0 |  | The minimum time spent by MPI on each MPI\_Iprobe call |
| otf2\_dir\_basename (time) | empty string |  | Enables OTF2 and combines this parameter with a timestamp to name the archive |

#### 7.4.1: Namespace ``mpi.queue"<a name="subsec:mpi:queue:Params"></a>




| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| max\_vshort\_msg\_size (byte length) | 512B |  | The maximum size to use the very short message protocol. Small messages are sent eagerly using special pre-allocated mailbox buffers. Sends complete immediately. |
| max\_eager\_msg\_size (byte length) | 8KB |  | The maximum size to use the RDMA eager protocol. This also uses buffers to send message, but instead of using pre-allocated mailboxes, it coordinates an RDMA get. Sends complete immediately. |
| post\_rdma\_delay (time) | 0 |  | The minimum time spent by MPI posting each RDMA operation |
| post\_header\_delay (time) | 0 |  | The mimimum time spent by MPI sending headers into pre-allocated mailboxes |
| poll\_delay (time) | 0 |  | The minimum time spent by MPI each time it polls for incoming messages |

### Section 7.5: Namespace "switch"<a name="subsec:switch:Params"></a>




| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| model (string) | No default | logP, pisces | The type of switch model (level of detail) for modeling network traffic. |
| buffer\_size (byte length) | No default |  | The size of input and output buffers on each switch. This determines the number of credits available to other components |

#### 7.5.1: Namespace "switch.router"<a name="subsec:switch:router:Params"></a>




| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| name (string) | No default | minimal, valiant, ugal, dragonfly\_minimal, fat\_tree | The name of the routing algorithm to use for routing packets. |
| ugal\_threshold (int) | 0 |  | The minimum number of network hops required before UGAL is considered. All path lengths less than value automatically use minimal. |

#### 7.5.2: Namespace "switch.output\_buffer"<a name="subsec:switch:outputBuffer:Params"></a>




| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| stats (string) | null | delay\_histogram,   byte\_hops, 	null | The type of statistics to collect from packets leaving or arriving at the switch output buffers.    Null indicates no statistics collected. For details on the other types of statistics, see Section [3.13](#sec:tutorials:packetStats) |

##### Namespace ``switch.output\_buffer.delay\_histogram"<a name="subsubsec:switch:outputBuffer:delayHistogram:Params"></a>



| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| fileroot (string) | No default |  | The path of a file to be created (or overwritten) where results from the statistics will be placed. |
| bin\_size (quantity) | No default | Positive value | For the quantity being collected (bytes,time,etc), this determines how large a value range should be binned together. |
| num\_bins (int) | 20 | Positive int | Determines the number of initial bins to allocate. If number is too small, simulation will allocate more. This is just a guess for optimization. |
| logarithmic (bool) | false |  | Whether to use a logarithmic axis for binning. |


##### Namespace "switch.output\_buffer.byte\_hops"<a name="subsubsec:switch:outputBuffer:delayHistogram:Params"></a>



| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| fileroot (string) | No default |  | The path of a file to be created (or overwritten) where results from the statistics will be placed. |


#### 7.5.3: Namespace "switch.xbar"<a name="sec:switch:outputBuffer:delayHistogram:Params"></a>




| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| stats (string) | null | bytes\_sent, 	null | The type of statistics to collect from packets leaving or arriving at the switch crossbar.    Null indicates no statistics collected. For details on the other types of statistics, see Section [3.13](#sec:tutorials:packetStats) |
| arbitrator (string) | cut\_through | null, simple, cut\_through | Bandwidth arbitrator for PISCES congestion modeling. Null uses simple delays with no congestion. Simple uses store-and-forward that is cheap to compute, but can have severe latency errors for large packets. Cut-through approximates pipelining of flits across stages. |
| latency (time) | No default |  | If given, overwrites the send and credit latency parameters. Depending on component, the entire latency may be put on either the credits or the send. |
| bandwidth | No default |  | The bandwidth of the arbitrator |
| send\_latency (time) | No default |  | The latency to send a packet to the next stage in the network. This can be omitted if the generic latency parameter is given (see above). |
| credit\_latency (time) | No default |  | The latency to send a credit to the previous network stage. This can be omitted if the generic latency parameter is given (see above). |
| credits (byte length) | No default |  | The number of initial credits for the component. Corresponds to an input buffer on another component. In many cases, SST/macro can compute this from other parameters and fill in the value. In some cases, it will be required. |
| mtu (byte length) | 1024B |  | The packet size. All messages (flows) will be broken into units of this size. |

##### Namespace ``switch.xbar.bytes\_sent"<a name="subsubsec:switch:xbar:bytesSent:Params"></a>



| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| fileroot (string) | No default |  | The path of a file to be created (or overwritten) where results from the statistics will be placed. |


#### 7.5.4: Namespace "switch.link"<a name="subsec:switch:link:Params"></a>



| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| arbitrator (string) | cut\_through | null, simple, cut\_through | Bandwidth arbitrator for PISCES congestion modeling. Null uses simple delays with no congestion. Simple uses store-and-forward that is cheap to compute, but can have severe latency errors for large packets. Cut-through approximates pipelining of flits across stages. |
| latency (time) | No default |  | If given, overwrites the send and credit latency parameters. Depending on component, the entire latency may be put on either the credits or the send. |
| bandwidth | No default |  | The bandwidth of the arbitrator |
| send\_latency (time) | No default |  | The latency to send a packet to the next stage in the network. This can be omitted if the generic latency parameter is given (see above). |
| credit\_latency (time) | No default |  | The latency to send a credit to the previous network stage. This can be omitted if the generic latency parameter is given (see above). |
| credits (byte length) | No default |  | The number of initial credits for the component. Corresponds to an input buffer on another component. In many cases, SST/macro can compute this from other parameters and fill in the value. In some cases, it will be required. |
| mtu (byte length) | 1024B |  | The packet size. All messages (flows) will be broken into units of this size. |


#### 7.5.5: Namespace "switch.ejection"<a name="subsec:switch:ejection:Params"></a>


This namespace is not actually required. 
If unspecified, all of the values here will be filled in from `nic.injection`.


| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| arbitrator (string) | cut\_through | null, simple, cut\_through | Bandwidth arbitrator for PISCES congestion modeling. Null uses simple delays with no congestion. Simple uses store-and-forward that is cheap to compute, but can have severe latency errors for large packets. Cut-through approximates pipelining of flits across stages. |
| latency (time) | No default |  | If given, overwrites the send and credit latency parameters. Depending on component, the entire latency may be put on either the credits or the send. |
| bandwidth | No default |  | The bandwidth of the arbitrator |
| send\_latency (time) | No default |  | The latency to send a packet to the next stage in the network. This can be omitted if the generic latency parameter is given (see above). |
| credit\_latency (time) | No default |  | The latency to send a credit to the previous network stage. This can be omitted if the generic latency parameter is given (see above). |
| credits (byte length) | No default |  | The number of initial credits for the component. Corresponds to an input buffer on another component. In many cases, SST/macro can compute this from other parameters and fill in the value. In some cases, it will be required. |
| mtu (byte length) | 1024B |  | The packet size. All messages (flows) will be broken into units of this size. |


### Section 7.6: Namespace "appN"<a name="sec:appN:Params"></a>


This is a series of namespaces `app1`, `app2`, and so on for each of the launched applications. These should be contained within the `node` namespace.


| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| name (string) | No default | parsedumpi, cxx\_full\_main, cxx\_empty\_main | The name of the application to launch. Very few applications are built-in. Registration of external apps is shown starting in Section [3.5](#sec:tutorial:basicmpi). |
| size (int) | No default | Positive int | The number of procs (MPI ranks) to launch. If launch\_cmd given, this parameter is not required. |
| start (int) | 0 |  | The time at which a launch request for the application will be made |
| concentration (int) | 1 | Positive int | The number of procs (MPI ranks) per compute node |
| core\_affinities (vector of int) | Empty |  |  |
| launch\_cmd (string) | No default | Valid aprun or srun | This uses a launch command as would be found with ALPS or SLURM launchers on real systems, e.g. aprun -n 4 -N 1 |
| indexing (string) | block | block, random, cart, node\_id, coordinate | The indexing scheme for assign proc ID (MPI rank number) to compute nodes |
| node\_id\_mapper\_file (filepath) | No default |  | If using Node ID indexing, the file containing the node ID index list |
| random\_indexer\_seed (long) | System time |  | The seed to use for a random allocation. If not specified, system time is used. |
| allocation (string) | first\_available | first\_available, random, cart, node\_id, coordinate | The scheme to use for allocating compute nodes to a given job. |
| random\_allocation\_seed (long) | System time |  | For random allocation policy. If unspecified, system time is used as the seed. |
| node\_id\_allocation\_file (filepath) | No default |  | If using Node ID allocation, the file containing the list of node IDs to allocate for the job |
| dumpi\_metaname (filepath) | No default |  | If running DUMPI trace, the location of the metafile for configuring trace replay |
| coordinate\_file (filepath) | No default |  | If running using coordinate allocation or indexing, the path to the file containing the node coordinates of each proc (MPI rank) |
| cart\_sizes (vector of int) | No default |  | Launch a contiguous block of nodes in a Cartesian topology. This gives the size of each dimension in the block. |
| cart\_offsets (vector of int) | No default |  | Launch a contiguous block nodes in a Cartesian topology. This gives the offset in each dimension where the block begins. |
| parsedumpi\_timescale (double) | 1.0 | Positive float | If running DUMPI traces, scale compute times by the given value. Values less than 1.0 speed up computation. Values greater than 1.0 slow down computation. |
| parsedumpi\_terminate\_percent (int) | 100 | 1-100 | Percent of trace. Can be used to terminate large traces early |
| host\_compute\_timer (bool) | False |  | Use the compute time on the host to estimate compute delays |


| Name (type) | Default | Allowed | Description |
|-------------|---------|---------|-------------|
| otf2\_metafile (string) | No default | string | The root file of an OTF2 trace. |
| otf2\_timescale (double) | 1.0 | Positive float | If running OTF2 traces, scale compute times by the given value. Values less than 1.0 speed up computation. Values greater than 1.0 slow down computation. |
| otf2\_print\_mpi\_calls (bool) | false |  | Print MPI calls found in the OTF2 trace |
| otf2\_print\_trace\_events (bool) | false |  | Debugging flag that printsindividual trace events (which includes details such as when an MPI call begins, ends, and when a collective begins and ends |
| otf2\_print\_time\_deltas (bool) | false |  | Debugging flag that prints compute delays injected by the simulator |
| otf2\_warn\_unknown\_callback (bool) | false |  | Debugging flag the prints unknown callbacks |








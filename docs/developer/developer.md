# SST/macro 6.0: Developer's Reference

![](figures/sstlogo.png)

**







# Table of Contents
   - [Chapter 1: Introduction](#chapter:intro)
      - [Section 1.1: Use of C++/Boost](#sec:boost)
      - [Section 1.2: Polymorphism and Modularity](#sec:polymorphism)
      - [Section 1.3: Most Important Style and Coding Rules](#sec:stylerules)
      - [Section 1.4: Code Style Reformatting](#sec:styleReformat)
      - [Section 1.5: Memory Allocation](#sec:memalloc)
   - [Chapter 2: SProCKit](#chapter:sprockit)
      - [Section 2.1: Debug](#sec:debug)
      - [Section 2.2: Serialization](#sec:serialize)
      - [Section 2.3: Keyword Registration](#sec:keywords)
   - [Chapter 3: SST-macro Classes](#chapter:classes)
      - [Section 3.1: Class Style](#sec:style)
         - [3.1.1: Basic Class](#subsec:basicClass)
         - [3.1.2: Inheritance and Parent Classes](#subsec:inheritance)
         - [3.1.3: Exceptions](#classes:style:basic:exceptions)
      - [Section 3.2: Factory Types](#sec:factory)
         - [3.2.1: Usage](#subsec:usage)
         - [3.2.2: Base Class](#subsec:baseClass)
         - [3.2.3: Child Class](#subsec:childClass)
         - [3.2.4: External Linkage](#subsec:linkage)
   - [Chapter 4: Discrete Event Simulation](#chapter:des)
   - [Chapter 5: Hardware Models](#chapter:hardware)
      - [Section 5.1: Connectables](#sec:connectables)
      - [Section 5.2: Interconnect](#sec:topInterconnect)
      - [Section 5.3: Node](#sec:node)
      - [Section 5.4: Network Interface (NIC)](#sec:nic)
      - [Section 5.5: Memory Model](#sec:memModel)
      - [Section 5.6: Network Switch](#sec:networkSwitch)
      - [Section 5.7: Topology](#sec:topology)
         - [5.7.1: Structured Topology](#subsec:structuredTopology)
      - [Section 5.8: Router](#sec:router)
   - [Chapter 6: A Custom Object: Beginning To End](#chapter:custom)
   - [Chapter 7: How SST-macro Launches](#chapter:launching)
      - [Section 7.1: Configuration of Simulation](#sec:simConfig)
      - [Section 7.2: Building and configuration of simulator components](#sec:buildConfig)
         - [7.2.1: Event Manager](#sec:eventMan)
         - [7.2.2: Interconnect](#subsec:interconnect)
         - [7.2.3: Applications](#subsec:apps)
      - [Section 7.3: Running](#sec:running)
   - [Chapter 8: Statistics Collection](#chapter:stats)
      - [Section 8.1: Dumping Data](#sec:dumping)
      - [Section 8.2: Reduction and Aggregation](#sec:reduceStats)
      - [Section 8.3: Storage Contraints](#sec:storageStats)



## Chapter 1: Introduction<a name="chapter:intro"></a>



### Section 1.1: Use of C++/Boost<a name="sec:boost"></a>

SST/macro (Structural Simulation Toolkit for Macroscale) is a discrete event simulator designed for macroscale (system-level) experiments in HPC. SST/macro is an object-oriented C++ code that makes heavy use of dynamic types and polymorphism. While a great deal of template machinery exists under the hood, nearly all users and even most developers will never actually need to interact with any C++ templates. Most template wizardry is hidden in easy-to-use macros. While C++ allows a great deal of flexibility in syntax and code structure, we are striving towards a unified coding style.

### Section 1.2: Polymorphism and Modularity<a name="sec:polymorphism"></a>

The simulation progresses with different modules (classes) exchanging messages. In general, when module 1 sendings a message to module 2, module 1 only sees an abstract interface for module 2. The polymorphic type of module 2 can vary freely to employ different physics or congestions models without affecting the implementation of module 1. Polymorphism, while greatly simplifying modularity and interchangeability, does have some consequences. The "workhorse" of SST/macro is the base `event` and `message` classes. To increase polymorphism and flexibility, every SST/macro module that receives messages does so via the generic function

````
void
handle(event* ev){
...
}
````
The prototype therefore accepts any event type. The class `message` is a special type of event that refers specifically to a message (e.g. MPI message) carrying a data payload.
Misusing types in SST/macro is not a compile-time error.
The onus of correct event types falls on runtime assertions.
All event types may not be valid for a given module.
A module for the memory subsystem should throw an error if the developer accidentally passes it a event intended for the OS or the NIC.
Efforts are being made to convert runtime errors into compile-time errors.
In many cases, though, this cannot be avoided.
The other consequence is that a lot of dynamic casts appear in the code.
An abstract `event` type is received, but must be converted to the specific message type desired.
NOTE: While some dynamic casts are sometimes very expensive in C++ (and are implementation-dependent),
most SST/macro dynamic casts are simple equality tests involving virtual table pointers.

While, SST/macro strives to be as modular as possible, allowing arbitrary memory, NIC, interconnect components, in many cases certain physical models are simply not compatible. For example, using a fluid flow model for memory reads cannot be easily combined with a packet-based model for the network. Again, pairing incompatible modules is not a compile-time error. Only when the types are fully defined at runtime can an incompatibility error be detected. Again, efforts are being made to convert as many type-usage problems into compiler errors. We prefer simulation flexibility to compiler strictness, though.

### Section 1.3: Most Important Style and Coding Rules<a name="sec:stylerules"></a>

Here is a selection of C++ rules we have tended to follow. Detailed more below, example scripts are included to reformat the code style however you may prefer for local editing. However, if committing changes to the repository, only use the default formatting in the example scripts.

-   snake\_case is used for variable and class names.
-   We use ``one true brace`` style (OTBS) for source files.
-   In header files, all functions are inline style with attached brackets.
-   To keep code compact horizontally, indent is set to two spaces. Namespaces are not indented.
-   Generally, all if-else and for-loops have brackets even if a single line.
-   Accessor functions are not prefixed, i.e. a function would be called `name()` not `get_name()`, except
where conflicts require a prefix. Functions for modifying variables are prefixed with `set_`,
-   We use .h and .cc instead of .hpp and .cpp
-   As much implementation as possible should go in .cc files.
	Header files can end up in long dependency lists in the make system.  
	Small changes to header files can result in long recompiles.
	If the function is more than a basic set/get, put it into a .cc file.
-   Header files with many classes are discouraged.  When reasonable, one class per header file/source file is preferred.
	 Many short files are better than a few really long ones.
-   Document, document, document.  If it isn't obvious what a function does, add doxygen-compatible documentation.
	Examples are better than abstract wording.
-   Use STL and Boost containers for data structures.  Do not worry about performance until much later.
	Premature optimization is the root of all evil. If determined that an optimized data structure is needed,
	please do that after the entire class is complete and debugged.
-   Forward declarations.  There are a lot of interrelated classes, often creating circular dependencies. In addition, you can add up with an explosion of include files, often involving classes that are never used in a given `.cc` file.  For cleanliness of compilation, you will find many `*_fwd.h` files throughout the code. If you need a forward declaration, we encourage including this header file rather than ad hoc forward declarations throughout the code.

### Section 1.4: Code Style Reformatting<a name="sec:styleReformat"></a>

Since we respect the sensitivity of code-style wars, we include scripts that demonstrate basic usage of the C++ code formatting tool astyle. This can be downloaded from {http://astyle.sourceforge.net}. A python script called `fix_style` is included in the top-level bin directory. It recursively reformats all files in a given directory and its subdirectories.

### Section 1.5: Memory Allocation<a name="sec:memalloc"></a>

To improve performance, custom memory allocation strategies can be employed in C++. At present, a global custom `operator new` can be optionally activated which is optimized for large pages and memory pools. At present, no class-specific implementation of `operator new` is used. However, we may soon implement custom allocation techniques to improve things like cache/TLB efficiency. This is the only major change expected to SST/macro that would affect externally developed modules - and even here the expected modifications would be quite small. Because custom allocation schemes may be used, all externally developed code should use `operator new`, rather than not  `malloc` or `mmap`, unless allocating very large arrays.



## Chapter 2: SProCKit<a name="chapter:sprockit"></a>

SST-macro is largely built on the Sandia Productivity C++ Toolkit (SProCKit), which is included in the SST-macro distribution. Projects developed within the simulator using SProCKit can easily move to running the application on real machines while still using the SProCKit infrastructure. One of the major contributions is reference counted pointer types. The parameter files and input deck are also part of SProCKit.

### Section 2.1: Debug<a name="sec:debug"></a>

The goal of the SProCKit debug framework is to be both lightweight and flexible. The basic problem encountered in SST-macro development early on was the desire to have very fine-grained control over when and where something prints. Previously declared debug flags are passed through the `debug_printf` macro.

````
debug_printf(sprockit::dbg::mpi,
  "I am MPI rank 
  rank, nproc);
````

The macro checks if the given debug flag is active. If so, it executes a `printf` with the given string and arguments. Debug flags are turned on/off via static calls to

````
sprockit::debug::turn_on(sprockit::dbg::mpi);
````

SST-macro automatically turns on the appropriate debug flags based on the `-d` command line flag or the `debug = ` parameter in the input file.

Multiple debug flags can be specified via OR statements to activate a print statement through multiple different flags.

````
using namespace sprockit;
debug_printf(dbg::mpi | dbg::job_launch,
  "I am MPI rank 
  rank, nproc);
````

Now the print statement is active if either MPI or job launching is going to be debugged.

In `sprockit/debug.h` a set of macros are defined to facilitate the declaration. To create new debug flags, there are two macros. The first, `DeclareDebugSlot`, generally goes in the header file to make the flag visible to all files. The second, `RegisterDebugSlot`, goes in a source file and creates the symbols and linkage for the flag.

````
launch.h:
DeclareDebugSlot(job_launch);

launch.cc
RegisterDebugSlot(job_launch);
````

### Section 2.2: Serialization<a name="sec:serialize"></a>

Internally, SST-macro makes heavy use of object serialization/deserialization in order to run in parallel with MPI. To create a serialization archive, the code is illustrated below. Suppose we have a set of variables

````
struct point {
 int x;
 int y;
}
point pt;
pt.x = 0;
pt.y = 2;
int niter = 5;
std::string str = "hello";
````

We can serialize them to a buffer

````
spkt_serializer ser;
ser.set_mode(spkt_serializer::PACK);
ser.init(new char[512]);
ser & pt;
ser & niter;
ser & str;
````
In the current implementation, the buffer must be explicitly given.

To reverse the process for a buffer received over MPI, the code would be

````
char* buf = new char[512];
MPI_Recv(buf, ...)
spkt_serializer;
ser.set_mode(spkt_serializer::UNPACK);
ser.init(buf);
ser & pt;
ser & niter;
ser & str;
````

Thus the code for serializing is exactly the same as deserializing. The only change is the mode of the serializer is toggled.

The above code assumes a known buffer size (or buffer of sufficient size). To serialize unknown sizes, the serializer can also compute the total size first.

````
spkt_serializer ser;
ser.reset();
ser.set_mode(spkt_serializer::SIZER);
ser & pt;
ser & niter;
ser & str;
int size = ser.sizer.size(); //would be 17 for example above
char* buf = new char[size];
...
````
The known size can now be used safely in serialization.

The above code only applies to plain-old dataypes and strings. The serializer also automatically handles STL and Boost containers through the `<<` syntax. To serialize custom objects, a C++ class must implement the serializable interface.

````
namespace my_ns {
class my_object : 
  public serializable,
  public serializable_type<my_object>
{
 ImplementSerializable(my_object)
 ...
 void
 serialize_order(spkt_serializer& ser);
 ...
};
}
````
The serialization interface requires two inheritances.
The first inheritance from `serializable` is the one "visible" to the serializer.
This inheritance forces the object to define a `serialize_order` function.
The second inheritance is used in registering a type descriptor.
The macro `ImplementSerializable` inside the class creates a set of necessary functions.
This is essentially a more efficient RTTI, mapping unique integers to a polymorphic type.
There are cleaner ways of doing this in terms of the interface,
but the current setup is chosen for safety.
The forced inheritance allows more safety checks to ensure types are being set up and used correctly.

In the source file, the type descriptor must be registered. This is done through the macro

````
DeclareSerializable(my_ns::my_object);
````
This macro should exist in the global namespace.
All that remains now is defining the `serialize_order` in the source file

````
void
my_object::serialize_order(spkt_serializer& ser)
{
  ser & my_int_;
  set << my_double_;
  ...
}
````

For inheritance, only the top-level parent class needs to inherit from `serializable`.

````
class parent_object : 
  public serializable
{
...
  void
  serialize_order(spkt_serializer& set);
...
};

class my_object :
  public parent_object,
  public serializable_type<my_object>
{
 ImplementSerializable(my_object)
 ...
 void
 serialize_order(spkt_serializer& ser);
 ...
};
````
In the above code, only `my_object` can be serialized.
The `parent_object` is not a full serializable type because no descriptor is registered for it.
Only the child can be serialized and deserialized.
However, the parent class can still contribute variables to the serialization.
In the source file, we would have

````
void
my_object::serializer_order(spkt_serializer& ser)
{
  parent_object::serialize_order(ser);
  ...
}
````
The child object should always remember to invoke the parent serialization method.

### Section 2.3: Keyword Registration<a name="sec:keywords"></a>

As stated previously, SProCKit actually implements all the machinery for parameter files. This is not part of the SST-macro core. To avoid annoying bugs, the SProCKit input system requires all allowed values for input parameters to be declared. This can happen in any source file through static initialization macros. Only one invocation is allowed per source file, but keywords can be registered in as many source files as desired. The macro is used in the global namespace:

````
RegisterKeywords("nx", "ny", "nz");
````
This registers some basic keywords that might be used in a 3D grid application.

In many cases a parameter is an enumerated value or fits a pattern. SProCKit allows regular expressions to be declared as valid patterns for a keyword.

````
StaticKeywordRegisterRegexp my_regexp("particle\\d+");
````
Here you create a static instance of a keyword registration object.
The constructor registers the regular expression.
Now, any keywords matching the regular expression will be considered valid.





## Chapter 3: SST-macro Classes<a name="chapter:classes"></a>



### Section 3.1: Class Style<a name="sec:style"></a>



#### 3.1.1: Basic Class<a name="subsec:basicClass"></a>

Most classes are manually managed, being explicitly deleted.  Whenever possible, smart pointers should be avoided since they create thread-safety headaches. Classes in SST-macro can inherit from the top-level class `ptr_type`, which exists in `namespace sprockit`. The examples here can be found in the code repository in `tutorials/programming/basic`.

To begin, you just need to include the appropriate header file, declare the namespace desired, and start the class declaration (see `illustration.h`).

````
#include <sprockit/ptr_type.h>

namespace sstmac { namespace tutorial {

class illustration :
  public sprockit::ptr_type
{
````
To simplify reading of the code (i.e. not typing `sprockit::refcount_ptr` everywhere), every class is required to typedef itself.

````
public:
  typedef sprockit::refcount_ptr<illustration> ptr;
  typedef sprockit::refcount_ptr<const illustration> const_ptr;
````
The reference-counted pointer type can now be used as `illustration::ptr`.

After declaring public typedefs, the public function interface can be declared. Every `ptr_type` class that is instantiated must implement a `to_string` function, mostly used for debugging purposes.

````
public:
  std::string
  to_string() const {
    return "message class";
   }
````

Public set/get functions can be added for member variables, if desired. We have generally followed snake\_case, using lower-case letters and underscores.

````
std::string
  message() const {
    return message_;
   }

void
  set_message(const std::string& msg){
    message_ = msg;
  }
````
Unfortunately, amongst the various developers, a uniform style was never agreed.
Some accessor functions could be called `get_message`, but most avoid the prefix and would be called just `message`.
After any `private`/`protected` functions, member variables can be declared.
SST-macro developers have not historically been very strict with use of `protected` vs. `private` for members, typically using `protected` as a default for convenience.

NOTE: We already have our first style violation here.  The entire class is implemented here in a header file. In general, unless a trivial function like set/get, put the function implementation into a .cc file.

#### 3.1.2: Inheritance and Parent Classes<a name="subsec:inheritance"></a>

We can also work through an example with inheritance. If you are going to use smart pointers, inheritance is somewhat tricky. Here we demonstrate in detail how to use smart pointers with virtual inheritance, which is a scenario that often occurs with the `sst_message` class. Many classes will never actually be instantiated in SST-macro - they are virtual classes defining an abstract interface. In the file `gem.h`, we define the `gem` abstract interface. Again, we include the header, declare namespaces, and declare the class.

````
#include <sprockit/ptr_type.h>

namespace sstmac { namespace tutorial {

class gem :
  virtual public sprockit::ptr_type
{
````
We note here the use of virtual inheritance, which will become important later.
In general, you should almost always use virtual inheritance from `ptr_type`.
Even though this class is not a complete type (pure virtual functions), we still need the public pointer typedefs.

````
public:
  typedef sprockit::refcount_ptr<gem> ptr;
  typedef sprockit::refcount_ptr<const gem> const_ptr;
````
We then define the abstract `gem` interface
````
public:
  virtual int
  value() const = 0;

virtual ~gem(){}
````
There is no static `construct` function!
The static construct function
is a signal that the class is a complete-type, with all virtual methods implemented.
Because of C++ destructors for abstract classes, you must define a virtual destructor for every parent class - even if it does nothing.
Without this, the memory management system will not work correctly.
Many tutorials on the subtleties of virtual destructors can be found online.

We can repeat the same process for a new abstract interface called `mineral`.
Declare the class:
````
#include <sprockit/ptr_type.h>

namespace sstmac { namespace tutorial {

class mineral :
    virtual public sprockit::ptr_type
{
````
Again, we use virtual inheritance. We declare public typedefs:
````
public:
  typedef sprockit::refcount_ptr<mineral> ptr;
  typedef sprockit::refcount_ptr<const mineral> const_ptr;
````
We then define the abstract `mineral` interface:
````
public:
  virtual std::string
  structure() const = 0;

virtual ~mineral(){}
````

Now we want to create a specific instance of a `gem` and `mineral`.
````
#include "gem.h"
#include "mineral.h"

namespace sstmac { namespace tutorial {

class diamond :
    public gem,
    public mineral
{
````
The reason for using virtual inheritance is now evident.
The `diamond` class is going to implement both the `gem` and `mineral` interfaces.
However, both classes separately inherit from `ptr_type`.
We thus have the multiple inheritance diamond problem. 
For this reason, both parent classes must virtually inherit from `ptr_type`.
Most of this discussion is just a basic C++ tutorial without really being SST-macro specific.
However, this diamond problem is so ubiquitous with `ptr_type`, it merits a discussion here in the programmer's reference.

We can now fill out the class.  First, we have the public typedefs

````
public:
  typedef sprockit::refcount_ptr<diamond> ptr;
  typedef sprockit::refcount_ptr<const diamond> const_ptr;
````

The `ptr_type` class requires we add a `to_string` function

````
std::string
to_string() const {
  return "diamond";
}
````

We now complete the gem interface

````
int
value() const {
  return num_carats_ * 100;
}
````

and the mineral interface
````
std::string
structure() const {
  return "tetrahedral carbon";
}
````

And the constructor

````
diamond(num_carats)
   : num_carats_(num_carats)
{
}
````

Finally, we declare member variables
````
protected:
  int num_carats_;
````

#### 3.1.3: Exceptions<a name="classes:style:basic:exceptions"></a>



The `ptr_type.h` header file automatically includes a common set of exceptions, all in `namespace sprockit`. The most notable are:

-   `value_error`: A parameter value is wrong
-   `unimplemented_error`: A place-holder to indicate this function is not valid or not yet done
-   `illformed_error`: A catch-all for anything that fails a sanity check
-   `null_error`: A null object was received
SProCKit provides a macro for throwing exceptions.
It should always be used since they provide extra metadata to the exception like file and line number.
The macro, `spkt_throw_printf`, takes two mandatory arguments: the exception type and an exception message.
An arbitrary number of arguments can be given, that get passed a `printf` invocation, e.g.

````
spkt_throw_printf(value_error, "invalid number of carats
````

### Section 3.2: Factory Types<a name="sec:factory"></a>

We here introduce factory types, i.e. polymorphic types linked to keywords in the input file. String parameters are linked to a lookup table, finding a factory that produces the desired type. In this way, the user can swap in and out C++ classes using just the input file. There are many distinct factory types relating to the different hardware components. There are factories for topology, NIC, node, memory, switch, routing algorithm - the list goes on. Here show how to declare a new factory type and implement various polymorphic instances. The example files can be found in `tutorials/programming/factories`.

#### 3.2.1: Usage<a name="subsec:usage"></a>

Before looking at how to implement factory types, let's look at how they are used. Here we consider the example of an abstract interface called `actor`. The code example is found in `main.cc`. The file begins

````
#include <sstmac/software/process/app.h>
#include <sstmac/common/sstmac_env.h>
#include "actor.h"

namespace sstmac { namespace tutorial { sstmac_register_app(rob_reiner);

int
rob_reiner_main(int argc, char **argv)
{
````
The details of declaring and using external apps is found in the manual.
From here it should be apparent that we defined a new application with name `rob_reiner`
which is invoked via the `rob_reiner_main` function.
Inside the main function, we create an object of type `actor`.

````
actor* the_guy = actor_factory::get_param("actor_name", sstmac::env::params);
the_guy->act();
return 0;
````
We use the `actor_factory` to create the object.
The value of `actor_name` is read from the input file `parameters.ini` in the directory.
Depending on the value in the input file, a different type will be created.
The input file contains several parameters related to constructing a machine model - ignore these for now.
The important parameters are:

````
launch_app1 = rob_reiner
biggest_fan = jeremy_wilke
actor_name = patinkin
sword_hand = right
````

Using the Makefile in the directory, if we compile and run the resulting executable we get the output

````
Hello. My name is Inigo Montoya. You killed my father. Prepare to die!
Estimated total runtime of           0.00000000 seconds
SST/macro ran for       0.0025 seconds
````

If we change the parameters:

````
launch_app1 = rob_reiner
biggest_fan = jeremy_wilke
actor_name = guest
num_fingers = 6
````

we now get the output

````
You've been chasing me your entire life only to fail now.
I think that's the worst thing I've ever heard. How marvelous.
Estimated total runtime of           0.00000000 seconds
SST/macro ran for       0.0025 seconds
````

Changing the values produces a different class type and different behavior. Thus we can manage polymorphic types by changing the input file.

#### 3.2.2: Base Class<a name="subsec:baseClass"></a>

To declare a new factory type, you must include the factory header file and inherit from the base class `factory_type`.

````
#include <sprockit/factories/factory.h>

namespace sstmac { namespace tutorial {

class actor :
    public sprockit::factory_type
{
````

We now define the public interface for the actor class

````
public:
  virtual void
  act() = 0;

virtual ~actor(){}
````
Again, we must have a public, virtual destructor.
Each instance of the actor class must implement the `act` method.

For factory types, each class must implement

````
virtual void
init_factory_params(sprockit::sim_parameters* params);
````
In general, constructors for factory types do no work.
Reading parameters and initializing values happens entirely in this function.

The parent class has a single member variable

````
protected:
  std::string biggest_fan_;
````

After finishing the class, we need to invoke a macro

````
DeclareFactory(actor);
````
making SST-macro aware of the new factory type.

Moving to the `actor.cc` file, we see the implementation

````
namespace sstmac {
    namespace tutorial {

void
actor::init_factory_params(sprockit::sim_parameters* params)
{
  biggest_fan_ = params->get_param("biggest_fan");
}
````
We initialize the member variable from the parameter object. 
For cloning, we have instead

````
void
actor::clone_into(actor* cln) const
{
  cln->biggest_fan_ = biggest_fan_;
}
````
which initializes from the existing object being cloned.

We additionally need a macro

````
ImplementFactory(sstmac::tutorial::actor);
````
that defines certain symbols needed for implementing the new factory type.
For subtle reasons, this must be done in the global namespace.

#### 3.2.3: Child Class<a name="subsec:childClass"></a>

Let's now look at a fully implemented, complete actor type.  We declare it

````
#include "actor.h"

namespace sstmac { namespace tutorial {

class mandy_patinkin :
    public actor
{
````

We have a single member variable

````
protected:
  std::string sword_hand_;
````

This is a complete type that can be instantiated.

To create the class we will need the functions
````
virtual void
init_factory_params(sprockit::sim_parameters* params);

virtual actor*
clone() const;
````

And finally, to satisfy the `actor` public interface, we need

````
virtual void
act();
````

For doing the clone work, we need a protected helper function

````
protected:
  void
  clone_into(mandy_patinkin* cln) const;
````

Moving to the implementation, we must first register the new type using the macro

````
namespace sstmac {
    namespace tutorial {

SpktRegister("patinkin", actor, mandy_patinkin,
    "He's on one of those shows now... NCIS? CSI?");
````
The first argument is the string descriptor that will be linked to the type.
The second argument is the parent, base class. 
The third argument is the specific child type.
Finally, a documentation string should be given with a brief description.
Whatever string value is registered here will be used in the input file to create the type.
We can now implement the functions

````
void
mandy_patinkin::init_factory_params(sprockit::sim_parameters* params)
{
  sword_hand_ = params->get_param("sword_hand");

if (sword_hand_ == "left"){
    spkt_throw(value_error, "I am not left handed!");
  }
  else if (sword_hand_ != "right"){
      spkt_throw_printf(value_error,
          "Invalid hand specified: 
          sword_hand_.c_str());
  }
  actor::init_factory_params(params);
}
````
We are safe to throw exceptions here since we are not in the constructor.
The child class must invoke the parent class method. 
Finally, we specify the acting behavior

````
void
mandy_patinkin::act()
{
    std::cout << "Hello. My name is Inigo Montoya. You killed my father. Prepare to die!"
              << std::endl;
}
````

Another example `guest.h` and `guest.cc` in the code folder shows the implementation for the second class.

#### 3.2.4: External Linkage<a name="subsec:linkage"></a>

If you glance at the Makefile, you will see how and why the executable is created. A compiler wrapper `sst++` points the makefile to the SST-macro libraries, including a library `libsstmac_main` that actually implements the `main` routine and the SST-macro driver. By creating another executable, arbitrary code can be linked together with the core SST-macro framework. The same SST-macro driver is invoked in the external executable as would be invoked by the default main executable.



\newcommand{`event_handler`}{`event_handler`\xspace} \newcommand{`event_scheduler`}{`event_scheduler`\xspace} \newcommand{`event_manager`}{`event_manager`\xspace}

## Chapter 4: Discrete Event Simulation<a name="chapter:des"></a>

There are abundant tutorials on discrete event simulation around the web. To understand the basic control flow of SST-macro simulations, you should consult Section 3.6, Discrete Event Simulation, in the user's manual. For here, it suffices to simply understand that objects schedule events to run at a specific time. When an event runs, it can create new events in the future. A simulation driver gradually progresses time, running events when their time stamp is reached. As discussed in the user's manual, we must be careful in the vocabulary. Simulation time or simulated time is the predicted time discrete events are happening in the simulated hardware. Wall time or {wall clock time} is the time SST-macro itself has been running. There are a variety of classes the cooperate in driving the simulation, which we now describe.

\section{Event Managers} The driver for simulations is an event manager that provides the function

````
virtual void
schedule(timestamp start_time, event* event) = 0;
````
This function must receive events and order them according to timestamp.
Two main types of data structures can be used, which we briefly describe below.

The event manager also needs to provide

````
virtual void
run() = 0;
````

The termination condition can be:

-   A termination timestamp is passed.  For example, a simulation might be specified to terminate after 100 simulation seconds. 
Any pending events after 100 seconds are ignored and the simulation exits.
-   The simulation runs out of events.  With nothing left to do, the simulation exits.

Events are stored in a queue (sorted by time)

````
namespace sstmac {

class event_queue_entry { public: virtual void execute() = 0;

...
};
````

The execute function is invoked by the `event_manager` to run the underlying event. There are generally two basic event types in SST-macro, which we now introduce.

\subsection{Event Handlers} In most cases, the event is represented as a message sent to an object called an `event_handler` at a specific simulation time. In handling the message, the event handlers change their internal state and may cause more events by scheduling new messages at other event handlers (or scheduling messages to itself) at a future time. The workhorses for SST-macro are therefore classes that inherit from `event_handler`. The only method that must be implemented is

````
void
handle(event* ev);
````
The function is the common interface for numerous different operations from network injection to memory access to MPI operations.
In general, objects have two "directions" for the action - send or receive.
A NIC could "handle" a packet by injecting it into the network or "handle" a message by reporting up the network stack the message has arrived.
In most cases, the handled message must therefore carry with it some notion of directionality or final destination.
An event handler will therefore either process the message and delete the message, or, if that handler is not the final destination, forward it along.
Some event handlers will only ever receive, such as a handler representing a blocking $MPI_Recv$ call.
Some event handlers will always receive and then send, such as network switches who are always intermediate steps between the start and endpoints of a network message.

In most cases, events are created by calling the function

````
void
schedule(const timestamp &t,
  event_handler* handler,
  event* ev);
````

This then creates a class of type `event_queue_entry`, for which the execute function is

````
void
handler_event_queue_entry::execute()
{
  if (!canceled_) {
    handler_->handle(ev_to_deliver_);
  }
}
````

\subsection{Arbitrary Events} In some cases, it can be inconvenient (and inefficient) to require every event to be funneled through an `event_handler` type. A generic macro for creating events from any class member function is provided in the file `event_callback.h`. For example, the MPI server creates an event

````
mpi_queue_recv_request* req = next_request();
event* ev = new_event(this, &mpi_queue::start_recv, req);
````
The new event macro takes as first argument an object and second argument a member function pointer to be invoked when the event is run.
The remaining arguments are an arbitrary-length list of parameters of any type - they don't need to be messages. 
These parameters should match the member function prototype. 
For example, the prototype

````
void
mpi_queue::start_recv(mpi_queue_recv_request* req)
{
  ...
}
````
takes a single `recv_request` object as input.
This eases the programming burden in two ways.
First, it avoids having to always create `event_handler` types.
Without template events, you would have to create a new class the inherits from `event_handler` that performs a single, desired action.
Second, it helps direct messages to the right place.  A single `event_handler` might process many different types of events or objects.
If every event went to a single `handle` method, the handle method would need either a long if-else block or switch to sort messages.
The event would also need to be dynamic cast to the correct type.
By creating event functors, the message can be immediately directed to the correct type and correct action.

For generic events, one must ensure the event is scheduled to the same node and does not cross any network boundaries. Generic events are not compatible with parallel simulation. The event created must be run on the same logical process.

\subsection{Event Heap/Map} The major distinction between different event containers is the data structured used. The simplest data structure is an event heap or ordered event map. The event manager needs to always be processing the minimum event time, which maps naturally onto a min-heap structure. Insertion and removal are therefore log(N) operations where N is the number of currently scheduled events. For most cases, the number and length of events is such that the min-heap is fine.

\section{Event Schedulers} The simulation is partitioned into objects that are capable of scheduling events. `event_scheduler` objects are also `event_handler` objects, although their handle methods might never be used. Common examples of `event_scheduler` objects are nodes, NICs, memory systems, or the operating system. In serial runs, an event scheduler is essentially just a wrapper for the `event_manager` and the class is not strictly necessary. In parallel simulation, though, the simulation must be partitioned into different scheduling units. Scheduling units are then distributed amongst the parallel processes. The `event_scheduler` is therefore the basic unit of parallelism. Additionally, when simulating failures (in either serial or parallel), certain devices must be deactivated such as a node or network switch going down. This basically amounts to canceling all events associated with a given `event_scheduler`. The `event_scheduler` is therefore primarily a means of structuring the computation.





\newcommand{\nodecls}{`node`\xspace} \newcommand{\topcls}{`topology`\xspace} \newcommand{\switchid}{`switch_id`\xspace} \newcommand{\nodeid}{`node_id`\xspace}

## Chapter 5: Hardware Models<a name="chapter:hardware"></a>



### Section 5.1: Connectables<a name="sec:connectables"></a>

While in common usage, SST-macro follows a well-defined machine model (see below), it generally allows any set of components to be connected. As discussed in Chapter [4](#chapter:des), the simulation proceeds by having event components exchange messages, each scheduled to arrive at a specific time. SST-macro provides a generic interface for any set of hardware components to be linked together. Any hardware component that connects to other components and exchanges messages must inherit from the `connectable` class. The `connectable` class presents a standard virtual interface

````
class connectable
{
 public:
  virtual void
  connect(
    int src_outport,
    int dst_inport,
    connection_type_t ty,
    connectable* mod);

virtual void
  connect_weighted(
    int src_outport,
    int dst_inport, 
    connection_type_t ty,
    connectable* mod,
    double weight, int red);
};
````

Any connection is identified by two parameters. First, a port number must be assigned. For example, a switch may have several outgoing connections, each of which must be assigned a unique port number. Second, a device may make several different types of connections. A network switch might connect to other network switches or it might connect an injection link to a node's network interface. A network interface might connect to an injector or the memory subsystem. Given the two parameters, the device is free to implement or store the connection in any way. Two port numbers must be given.  The output port on the router sending messages must be given, but also the input port on which the destination will receive the packet. Both port numbers must be given since the destination may have to return credits to the sender.

A certain style and set of rules is recommended for all connectables. If these rules are ignored, setting up connections can quicky become confusing and produce difficult to maintain code.

The first and most important rule is that `connectables` never make their own connections. Some "meta"-object should create connections between objects. In general, this work is left to a topology object. An object should never be responsible for knowing about the "world" outside itself. A topology or interconnect tells the object to make a connection rather than the object deciding to make the connection itself. This will be illustrated below in [5.7](#sec:topology).

The second rule to follow is that a connect function should never call another connect function. In general, a single call to a connect function should create a single link. If connect functions start calling other connect functions, you can end up a with a recursive disaster. If you need a bidirectional link (A $\rightarrow$ B, B $\rightarrow$ A), two separate function calls should be made

````
A->connect(B);
B->connect(A);
````

rather than having, e.g. A create a bidirectional link.

The first two rules should be considered rigorous. A third recommended rule is that all port numbers should be non-negative, and, in most cases, should start numbering from zero. Negative port numbers are generally used to indicate special meaning, not an actual port number. For example, routers generally return a negative number to indicate a packet has arrived at its destination and should be ejected.

Combining the factory system for polymorphic types and the connectable system for building arbitrary machine links and topologies, SST-macro provides flexibility for building essentially any machine model you want. However, SST-macro provides a recommended machine structure to guide constructing machine models.

### Section 5.2: Interconnect<a name="sec:topInterconnect"></a>

For all standard runs, the entire hardware model is driven by the interconnect object. The interconnect creates nodes, creates network switches, chooses a topology, and connects all of the network endpoints together. In this regard, the interconnect also choose what types of components are being connected together. For example, if you were going to introduce some custom FPGA device that connects to the nodes to perform filesystem operations, the interconnect is responsible for creating it.

To illustrate, here is the code for the interconnect that creates the node objects. The interconnect is itself a factory object, configured from a parameter file.

````
void
interconnect::init_factory_params(sprockit::sim_parameters* params)
{
  topology_ = topology_factory::get_param("topology_name", params);

node_tmpl_ = node_factory::get_param("node_name", params, node_id());

nic_tmpl_ = nic_factory::get_param("nic_name", params, node_tmpl_, this);

long count = topology_->num_nodes();
  for (long i = 0; i < count; i++) {
    add_node(node_id(i));
  }
}
````

The interconnect creates a topology and then template objects for the node and NIC. The topology object tells the interconnect how many nodes to create.

````
void
interconnect::add_node(node_id nid)
{
  node* newnode = node_tmpl_->clone(nid);
  nic* newnic = nic_tmpl_->clone(newnode, this);
  newnode->set_nic(newnic);

nodes_[nid] = newnode;
  available_.insert(nid);
}
````

Here we use the clone interface, creating copies of the template NIC and node. The `node_factory` and `nic_factory` could have been used to create new nodes from the parameter object instead of cloning. In general, SST-macro follows a style of creating a single template from the factory and then cloning it. This is done for two reasons. First, cloning is essentially just a memcpy, which is significantly cheaper than reading parameters and doing string conversion operations. This can save a lot of time and effort when there are a million hardware objects to create. Second, when a parameter object arrives in the `init_factory_params` function, it is correctly "namespaced" for the local scope. It may happen that after leaving `init_factory_params`, the interconnect needs to create more nodes and the parameters are no longer available. The only option at this point is to have a template lying around that can be cloned.

### Section 5.3: Node<a name="sec:node"></a>

Although the \nodecls can be implemented as a very complex model, it fundamentally only requires a single set of functions to meet the public interface. The \nodecls must provide `execute_kernel` functions that are invoked by the `operating_system` or other other software objects. The prototypes for these are:

````
virtual void
  execute_kernel(ami::AMI_COMP_FUNC func, sst_message* data);

virtual void
  execute_kernel(ami::AMI_COMM_FUNC func, sst_message* data);
````

By default, the abstract \nodecls class throws an `sprockit::unimplemented_error`. These functions are not pure virtual. A node is only required to implement those functions that it needs to do. The various function parameters are enums for the different operations a node may perform: computation, communication, or other hardware events. The distinction between computation and hardware is subtle. Hardware operations are things like interrupts, device resets, device failures. They are not necessarily "kernels" in the standard parlance.

To illustrate a single example, we show the code that handles the function call

````
node->execute_kernel(ami::AMI_COMM_SEND, msg);
````

leads to

````
switch (func) {
    case sstmac::sw::ami::AMI_COMM_SEND: 
    {
      network_message* netmsg = ptr_safe_cast(network_message, data);
      netmsg->set_fromaddr(my_id_);
      if (netmsg->toaddr() == nodeid_) {
      	/* Intranode send */
      }
      else {
        nic_->send(netmsg);
      }
    }
````
All dynamic casts are routed through a special macro `safe_cast` for most types or `ptr_safe_cast` for smart pointer types.

### Section 5.4: Network Interface (NIC)<a name="sec:nic"></a>

The network interface can implement many services, but the basic public interface requires the NIC to do three things:

-   Inject messages into the network
-   Receive messages ejected from the network
-   Deliver ACKs (acknowledgments) of message delivery

For sending messages, the NIC must implement

````
virtual void
  do_send(network_message*payload);
````
A non-virtual, top-level `send` function performs operations standard to all NICs.
Once these operations are complete, the NIC invokes `do_send` to perform model-specific send operations.
The NIC should only ever send `network_message` types.

For the bare-bones class `null_nic`, the function is

````
injector_->handle(msg);
  if (msg->get_needs_ack()) {
    sst_message* acker = msg->clone_ack();
    schedule(now(), parent_node_, acker);
  }
````
After injecting, the NIC creates an ACK and delivers the notification to the \nodecls.
In general, all arriving messages or ACKs should be delivered to the node.
The node is responsible for generating any software events in the OS.

For receiving, messages can be moved across the network and delivered in two different ways.

````
virtual void
  recv_chunk(const message_chunk::ptr &chunk,
             network_message*parent_msg);

virtual void
  recv_whole(network_message*msg);
````

Depending on the congestion model, a large message (say a 1 MB MPI message) might be broken up into many packets. These message chunks are moved across the network independently and then reassembled at the receiving end. Alternatively, for flow models or simple analytical models, the message is not packetized and instead delivered as a single whole. The methods are not pure virtual.  Depending on the congestion model,  the NIC might only implement chunk receives or whole receives. Upon receipt, just as for ACKs, the NIC should deliver the message to the node to interpret. Again, for the bare-bones class `null_nic`, we have

````
void
null_nic::recv_chunk(const message_chunk::ptr &chunk,
                     network_message*parent_msg)
{
  bool complete = completion_queue_.recv(chunk);
  if (complete){
    parent_->handle(parent_msg);
  }
}
````

A special completion queue object tracks chunks and processes out-of-order arrivals, notifying the NIC when the entire message is done.

### Section 5.5: Memory Model<a name="sec:memModel"></a>

As with the NIC and node, the memory model class can have a complex implementation under the hood, but it must funnel things through the a common function.

````
virtual void
  access(sst_message* msg);
````

The memory model must handle many different types of messages since so many different devices need to access the memory subsystem. A NIC, the CPU, a GPU, disk may all issue requests to the memory. In this regard, any message that implements

````
virtual long
  byte_length() const;
````
should be valid.

### Section 5.6: Network Switch<a name="sec:networkSwitch"></a>



Unlike the other classes above, a network switch is not required to implement any specific functions. It is only required to be an `event_handler`, providing the usual `handle(sst_message* msg)`. The internal details can essentially be arbitrary. However, the basic scheme for most switches follows the code below for the simplest `packet_switch` model.

````
packet_message::ptr pack = safe_cast(packet_message, msg);
  router_->route(pack);
  int port = pack->rinfo()->port();
  timestamp bw_delay(pack->byte_length() / portbw_[port]);
  timestamp delay = lat_r2r_ + bw_delay;
  event_handler* dest = outports_[port];
  schedule(now()+ delay, dest, msg);
````
The router object selects the next destination (port).
Then bandwidth and latency terms determine the packet delay.
The packet is then scheduled to the next switch.
Generally, a negative port number indicates ejection.

### Section 5.7: Topology<a name="sec:topology"></a>

Of critical importance for the network modeling is the topology of the interconnect. Common examples are the torus, fat tree, or butterfly. To understand what these topologies are, there are many resources on the web that do a better job than we could. Regardless of the actual structure as a torus or tree, the topology should present a common interface to the interconnect and NIC for routing messages. Here we detail the public interface. \subsection{Basic Topology} Not all topologies are "regular" like a torus.  Ad hoc computer networks (like the internet) are ordered with IP addresses, but don't follow a regular geometric structure. The abstract topology base class is intended to cover both cases. Irregular or arbitrary topology connections are not fully supported yet.

The most important functions in the \topcls class are

````
class topology
{

virtual std::vector<node_id> get_nodes_connected_to_switch(switch_id swid) const = 0;

virtual long num_switches() const = 0;

virtual long num_nodes() const = 0;

virtual void connect_objects(connectable_map& objects, connectable_factory* cloner) = 0;

virtual switch_id node_to_injector_addr(node_id nodeaddr, int& switch_port) const = 0;

virtual switch_id node_to_ejector_addr(node_id nodeaddr, int& switch_port) const = 0;

virtual void minimal_route_to_switch( switch_id current_sw_addr, switch_id dest_sw_addr, routing_info::path& path) const = 0;

virtual int
  num_hops_to_node(node_id src, node_id dst) const = 0;
````

These functions are documented at some length in the `topology.h` header file. The most important thing to distinguish here are \nodeid and \switchid types. These are both special opaque integer types that distinguish between a switch or internal object in the topology and a node or network endpoint. The first few functions just give the number of switches, number of nodes, and finally which nodes are connected to a given switch. The most critical function for a topology is the `connect_objects` function that takes a list of objects and actually forms the links between them. Each compute node will be connected to an injector switch and an ejector switch (often the same switch). The topology must provide a mapping between a node and its ejection and injection points. Additionally, the topology must indicate a port number or offset for the injection in case the switch has many nodes injecting to it.

Besides just forming the connections, a topology is responsible for routing. Given a source switch and the final destination, a topology must fill out path information.

````
struct path {
  int outport;
  int dim;
  int dir;
  int vc;
}
````

The most important information is the outport, telling a switch which port to route along to arrive at the destination. More detail can be given with dimension and direction for some topologies (i.e. +X or -X in a torus). For congestion models with channel dependencies, the virtual channel must also be given to avoid deadlock. In general, network switches and other devices should be completely topology-agnostic. The switch is responsible for modeling congestion within itself - crossbar arbitration, credits, outport multiplexing. The switch is not able to determine for itself which outport to route along. The topology tells the switch which port it needs and the switch determines what sort of congestion delay to expect on that port. This division of labor is complicated a bit by adaptive routing, but remains essentially the same.  More details are given later.

#### 5.7.1: Structured Topology<a name="subsec:structuredTopology"></a>



The structured topology assumes a regular, ordered connecting of nodes like a torus. It is synonymous with any topology that can be mapped onto a coordinate system. This is true even of tree structures for which the coordinates define which branch/leaf. The structured topology introduces a few extra virtual functions:

````
class structured_topology :
  public topology
{
  ....
  virtual int
  ndimensions() const = 0;

virtual void compute_switch_coords(switch_id swid, coordinates& coords) const = 0;

virtual void minimal_route_to_coords( const coordinates& src_coords, const coordinates& dest_coords, routing_info::path& path) const = 0;

virtual int minimal_distance( const coordinates& src_coords, const coordinates& dest_coords) const = 0;

virtual void
  get_productive_path(
    int dim,
    const coordinates& src,
    const coordinates& dst,
    routing_info::path& path) const = 0;
  ...
};
````
The structured topology must have a well-defined number of dimensions, such as a 3D-torus or the number of levels in a tree.
The topology must also be able to map any \switchid to a unique set of coordinates.
Given two input sets of input coordinates, it should be able to perform the routing operation or compute the minimal distance between two points.
Finally, for adaptive routing purposes, the topology should map routing requests to the correct path.
For example, if I want to move in the X dimension in a 3D-torus, what path (port) is needed to make a productive step?
It can be that any step along that dimension is unproductive - you've already arrived.
The behavior is then undefined.

### Section 5.8: Router<a name="sec:router"></a>

The router has a simple public interface

````
class router :
  public sprockit::factory_type
{
...
  void
  route(const routable_message::ptr& msg);

virtual void minimal_route_to_node( node_id node_addr, routing_info::path& path) = 0;

virtual void
  minimal_route_to_switch(
    switch_id sw_addr,
    routing_info::path& path) = 0;
...
};
````

Here the route function queries the message for algorithm info.

````
void
router::route(const routable_message::ptr& msg)
{
  if (!msg->routing_initialized()) {
    init_routing(msg);
  }
  routing_algorithm* algo = algos_[msg->rinfo()->route_algo()];
  if (unlikely(!algo)) {
    spkt_throw_printf(sprockit::value_error,
                     "routing algorithm 
                     routing_info::tostr(msg->rinfo()->route_algo()));
  }
  algo->route(msg, this);
}
````

The actual routing work is done by a routing algorithm object rather than the router. Different messages may choose different algorithms and we do not want to hard-wire routing to a single algorithm for the entire machine. The router object exists as a routing metadata store, potentially accumulating congestion or other data over time to inform future routing decisions. Thus, routing algorithms are static objects with no state - just instructions. Router carry the location-specific state.

For minimal routing on regular routers, the algorithm code is extremely simple

````
void
minimal_routing::route(
  const routable_message::ptr& msg,
  router* rter)
{
  rter->minimal_route_to_node(msg->toaddr(), msg->rinfo()->get_path());
}
````

````
void
structured_router::minimal_route_to_node(
  node_id dest_node_addr,
  routing_info::path& path)
{
  switch_id ej_addr = regtop_->node_to_ejector_addr(dest_node_addr, path.dir);
  if (ej_addr == me_) {
    path.dim = topology::EJECT;
    path.vc = 0;
    path.outport = topology::eject_port(path.dir);
  }
  else {
    minimal_route_to_switch(ej_addr, path);
  }
}
...
````

For adaptive routing, a bit more work is done

````
void
minimal_adaptive_routing::route(
  const routable_message::ptr &msg,
  router* rter)
{
  routing_info::path_set paths;
  bool eject  = rter->get_productive_paths_to_node(msg->toaddr(), paths);
  if (eject) {
    msg->rinfo()->assign_path(paths[0]);
    return;
  }
````
We now query for all possible paths.
Some network switches (not all) provide functionality to query congestion info.

````
int test_length = rter->get_switch()->queue_length(paths[i].outport);
````
from which the lowest congestion path can be chosen.





## Chapter 6: A Custom Object: Beginning To End<a name="chapter:custom"></a>



Suppose we have brilliant design for a new topology we want to test. We want to run a simple test without having to modify the SST-macro build system. We can create a simple external project that links the new topology object to SST-macro libraries. The Makefile can be found in `tutorials/programming/topology`. You are free to make any Makefile you want. After SST-macro installs, it creates compiler wrappers `sst++` and `sstcc` in the chosen `bin` folder. These are essentially analogs of the MPI compiler wrappers. This configures all include and linkage for the simulation.

We want to make an experimental topology in a ring. Rather than a simple ring with connections to nearest neighbors, though, we will have "express" connections that jump to switches far away.

We begin with the standard typedefs.
````
#include <sstmac/hardware/topology/structured_topology.h>

namespace sstmac { namespace hw {

class xpress_ring : public structured_topology { public: typedef enum { up_port = 0, down_port = 1, jump_up_port = 2, jump_down_port = 3 } port_t;

typedef enum {
    jump = 0, step = 1
  } stride_t;
```` 
Packets can either go to a nearest neighbor or they can "jump" to a switch further away.
Each switch in the topology will need four ports for step/jump going up/down.
The header file can be found in `tutorials/programm/topology/xpressring.h`.
We now walk through each of the functions in turn in the source in the topology public interface.
We got some functions for free by inheriting from `structured_topology`.

We start with

````
void
xpress_ring::init_factory_params(sprockit::sim_parameters* params)
{
  structured_topology::init_factory_params(params);
  ring_size_ = params->get_int_param("xpress_ring_size");
  jump_size_ = params->get_int_param("xpress_jump_size");
}
````
determining how many switches are in the ring and how big a "jump" link is.

The topology then needs to tell objects how to connect

````
void
xpress_ring::connect_objects(connectable_map& objects,
                             connectable_factory* cloner)
{
  for (int i=0; i < ring_size_; ++i) {
    connectable* center_obj = objects[switch_id(i)];

switch_id up_idx((i + 1) connectable* up_partner = find_object(objects, cloner, up_idx); center_obj->connect(up_port, down_port, connectable::network_link, up_partner);

switch_id down_idx((i + ring_size_ - 1) connectable* down_partner = find_object(objects, cloner, down_idx); center_obj->connect_mod_at_port(down_port, up_port, connectable::network_link, down_partner);

switch_id jump_up_idx((i + jump_size_) connectable* jump_up_partner = find_object(objects, cloner, jump_up_idx); center_obj->connect(jump_up_port, jump_down_port, connectable::network_link, jump_up_partner);

switch_id jump_down_idx((i + ring_size_ - jump_size_) 
    connectable* jump_down_partner = find_object(objects, cloner,
                                         jump_down_idx);
    center_obj->connect(jump_down_port, jump_up_port, connectable::network_link,
                                    jump_down_partner);
  }
}
````
We loop through every switch in the ring and form $+/-$ connections to neighbors and $+/-$ connections to jump partners.
Each of the four connections get a different unique port number.  We must identify both the outport port for the sender and the input port for the receiver.

To compute the distance between two switches

````
int
xpress_ring::num_hops(int total_distance) const
{
  int num_jumps = total_distance / jump_size_;
  int num_steps = total_distance 
  int half_jump = jump_size_ / 2;
  if (num_steps > half_jump) {
    //take an extra jump
    ++num_jumps;
    num_steps = jump_size_ - num_steps;
  }
  return num_jumps + num_steps;
}

int xpress_ring::minimal_distance( const coordinates& src_coords, const coordinates& dest_coords) const { int src_pos = src_coords[0]; int dest_pos = dest_coords[0]; int up_distance = abs(dest_pos - src_pos); int down_distance = abs(src_pos + ring_size_ - dest_pos);

int total_distance = std::max(up_distance, down_distance);
  return num_hops(total_distance);
}
````
Essentially you compute the number of jumps to get close to the final destination and then the number of remaining single steps.

For computing coordinates, the topology has dimension one.
````
switch_id
xpress_ring::get_switch_id(const coordinates& coords) const
{
  return switch_id(coords[0]);
}

void xpress_ring::get_productive_path( int dim, const coordinates& src, const coordinates& dst, routing_info::path& path) const { minimal_route_to_coords(src, dst, path); }

void
xpress_ring::compute_switch_coords(switch_id swid, coordinates& coords) const
{
  coords[0] = int(swid);
}
````
Thus the coordinate vector is a single element with the \switchid.

The most complicated function is the routing function.

````
void
xpress_ring::minimal_route_to_coords(
  const coordinates& src_coords,
  const coordinates& dest_coords,
  routing_info::path& path) const
{
  int src_pos = src_coords[0];
  int dest_pos = dest_coords[0];

//can route up or down
  int up_distance = abs(dest_pos - src_pos);
  int down_distance = abs(src_pos + ring_size_ - dest_pos);
  int xpress_cutoff = jump_size_ / 2;
````
First we compute the distance in the up and down directions.
We also compute the cutoff distance where it is better to jump or step to the next switch.
If going up is a shorter distance, we have

````
if (up_distance <= down_distance) {
    if (up_distance > xpress_cutoff) {
      path.outport = jump_up_port;
      path.dim = UP;
      path.dir = jump;
      path.vc = 0;
    }
    else {
      path.outport = up_port;
      path.dim = UP;
      path.dir = step;
      path.vc = 0;
    }
  }
````
We then decide if it is better to step or jump.
We do not concern ourselves with virtual channels here and just set it to zero.
Similarly, if the down direction in the ring is better

````
else {
    if (down_distance > xpress_cutoff) {
      path.outport = jump_down_port;
      path.dim = DOWN;
      path.dir = jump;
      path.vc = 0;
    }
    else {
      path.outport = down_port;
      path.dim = DOWN;
      path.dir = step;
      path.vc = 0;
    }
  }
}
````

For adaptive routing, we need to compute productive paths. In this case, we only have a single dimension so there is little adaptive routing flexibility. The only productive paths are the minimal paths.

````
void
xpress_ring::get_productive_path(
  int dim,
  const coordinates& src,
  const coordinates& dst,
  routing_info::path& path) const
{
  minimal_route_to_coords(src, dst, path);
}
````

We are now ready to use our topology in an application. In this case, we just demo with the built-in MPI ping all program from SST-macro. Here every node in the network sends a point-to-point message to every other node. There is a parameter file in the `tutorials/programming/toplogy` folder. To specify the new topology

````
# Topology
topology_name = xpress
xpress_ring_size = 10
xpress_jump_size = 5
````
with application launch parameters

````
# Launch parameters
launch_indexing = block
launch_allocation = first_available
launch_app1_cmd = aprun -n10 -N1
launch_app1 = mpi_test_all
````
The file also includes a basic machine model.

After compiling in the folder, we produce an executable `runsstmac`. Running the executable, we get the following

````
Estimated total runtime of           0.00029890 seconds
SST/macro ran for       0.4224 seconds
````
where the SST-macro wall clock time will vary depending on platform.
We estimate the communication pattern executes and finishes in 0.30 ms.
Suppose we change the jump size to a larger number.
Communication between distant nodes will be faster, but communication between medium-distance nodes will be slower.
We now set `jump_size = 10` and get the output

````
Estimated total runtime of           0.00023990 seconds
SST/macro ran for       0.4203 seconds
````
We estimate the communication pattern executes and finishes in 0.24 ms, a bit faster.
Thus, this communication pattern favors longer jump links.





## Chapter 7: How SST-macro Launches<a name="chapter:launching"></a>



It is useful for an intuitive understanding of the code to walk through the steps starting from `main` and proceeding to the discrete event simulation actually launching. The code follows these basic steps:

\begin{enumerate} \item Configuration of the simulation via environment variables, command line parameters, and the input file \item Building and configuration of simulator components \item Running of the actual simulation \end{enumerate}

We can walk through each of these steps in more detail.

### Section 7.1: Configuration of Simulation<a name="sec:simConfig"></a>

The configuration proceeds through the following basic steps: \begin{enumerate} \item Basic initialization of the `parallel_runtime` object from environment variables and command line parameters \item Processing and parallel broadcast of the input file parameters \item Creation of the simulation `manager` object \item Detailed configuration of the `manager` and `parallel_runtime` object

The first step in most programs is to initialize the parallel communication environment via calls to MPI\_Init or similar. Only rank 0 should read in the input file to minimize filesystem traffic in a parallel job. Rank 0 then broadcasts the parameters to all other ranks. We are thus left with the problem of wanting to tune initialization of the parallel environment via the input file, but the input file is not yet available. Thus, we have an initial bootstrap step where the all parameters affecting initialization of the parallel runtime must be given either via command line parameters or environment variables. These automatically get distributed to all processes via the job launcher. Most critically the environment variable `SSTMC_PARALLEL` takes on values of `serial` or `mpi`.

As stated above, only rank 0 ever touches the filesystem. A utility is provided within the Sprockit library for automatically distributing files via the `parallel_build_params` function within `sim_parameters`. Once broadcast, all ranks now have all they need to configure, setup, and run. Some additional processing is done here to map parameters. If parameters are missing, SST-macro may fill in sensible defaults at this stage. For deprecated parameters, SST-macro also does some remapping to ensure backwards compatibility.

After creation of the `manager` object, since all of the parameters even from the input file are now available, a more detailed configuration of the `manager` and `parallel_runtime` can be done.

### Section 7.2: Building and configuration of simulator components<a name="sec:buildConfig"></a>

Inside the function `mgr->init_factory_params`, the simulation manager now proceeds to build all the necessary components. There are three basic classes of components to build.

-   The event manager that drives the discrete event simulation
-   The interconnect object that directs the creation of all the hardware components
-   The generation of application objects that will drive the software events

#### 7.2.1: Event Manager<a name="sec:eventMan"></a>

The `event_manager` object is a polymorphic type that depends on 1) what sort of parallelism is being used and 2) what sort of data structure is being used. Some allowed values include `event_map` or `event_calendar` via the `event_manager` variable in the input file. For parallel simulation, only the `event_map` data structure is currently supported. For MPI parallel simulations, the `event_manager` parameter should be set to `clock_cycle_parallel`. For multithreaded simulations (single process or coupled with MPI), this should be set to `multithread`. In most cases, SST-macro chooses a sensible default based on the configuration and installation.

As of right now, the event manager is also responsible for partitioning the simulation. This may be refactored in future versions. This creates something of a circular dependency between the `event_manager` and the `interconnect` objects. When scheduling events and sending events remotely, it is highly convenient to have the partition information accessible by the event manager. For now, the event manager reads the topology information from the input file. It then determines the total number of hardware components and does the partitioning. This partitioning object is passed on to the interconnect.

#### 7.2.2: Interconnect<a name="subsec:interconnect"></a>

The interconnect is the workhorse for building all hardware components. After receiving the partition information from the `event_manager`, the interconnect creates all the nodes, switches, and NICs the current MPI rank is responsible for. In parallel runs, each MPI rank only gets assigned a unique, disjoint subset of the components. The interconnect then also creates all the connections between components that are linked based on the topology input (see Section [5.1](#sec:connectables)). For components that are not owned by the current MPI rank, the interconnect inserts a dummy handler that informs the `event_manager` that the message needs to be re-routed to another MPI rank.

#### 7.2.3: Applications<a name="subsec:apps"></a>

All events generated in the simulation ultimately originate from application objects. All hardware events start from real application code. To generate application objects, the manager first calls `build_apps`, which loops through and finds all the applications to be launched. It then builds a template object, which will be used to launch all the individual instances of the application.

Every application gets assigned a `software_id`, which is a struct containing a `task_id` and `app_id`. The task ID identifies the process number (essentially MPI rank). The application ID identifies which currently running application instance is being used. This is really only relevant in cases like in situ analysis where two distinct applications are running. In most cases, only a single application is being used, in which case the application ID is always zero. The simulation manager depends on an `app_manager` object that keeps track of the mapping between software IDs and the actual physical nodes that are running the apps.

To actually launch the app, we have the following code:

````
void
manager::launch_app(int appnum, timestamp start, sw::app_manager* appman)
{
  appman->allocate_and_index_jobs();
  launch_info* linfo = appman->launch_info();
  sstmac::sw::app_id aid(appnum);
  for (int i=0; i < appman->nproc(); ++i) {
    node_id dst_nid = appman->node_assignment(i);
    sstmac_runtime::register_node(aid, task_id(i), dst_nid);

hw::node* dst_node = interconnect_->node_at(dst_nid); if (!dst_node) { // mpiparallel, this node belongs to someone else continue; }

sw::launch_message::ptr lmsg = new launch_message(linfo, sw::launch_message::ARRIVE, task_id(i));
    int dstthread = dst_node->thread_id();
    event_manager_->ev_man_for_thread(dstthread)->schedule(start, new handler_event(lmsg, dst_node));
  }
````
Here the application manager first allocates the correct number of nodes and indexes (assigns task numbers to nodes).
This is detailed in the user's manual.
The application manager has a launch info object that contains all the information needed to launch a new instance of the application on each node.
The application manager then loops through all processes it is supposed to launch,
queries for the correct node assignment,
and fetches the physical node that will launch the application.
If a null node is returned, that indicates the physical node is owned by another MPI rank and should be skipped.
Finally a launch message containing the launch info is scheduled to arrive at the node.
When the node receives the launch message, it will actually create the application object and pass it off to the `operating_system` object to allocate stack resources and start it running.

### Section 7.3: Running<a name="sec:running"></a>

Now that all hardware components have been created and all application objects have been assigned to physical nodes, the `event_manager` created above is started. It begins looping through all events in the queue ordered by timestamp and runs them. As stated above, all events originate from application code. Thus, the first events to run are always the application launch events generated from the launch messages sent to the nodes.

\end{enumerate}



## Chapter 8: Statistics Collection<a name="chapter:stats"></a>



Statistics collection for tracking things like congestion or number of bytes sent is difficult to standardize. Stats collection must be specifically configured to different components (e.g. NIC, CPU, memory) and types of statistic (histogram, spyplot, timeline). The stats framework is therefore intended to be highly customizable based on the individual analysis being performed without too many constraints. There are a few universal features all stats objects must comply with. First, any object that collects stats must inherit from `stat_collector` contained in the header `sstmac/common/stats/stat_collector.h`. This defines a virtual interface that every stats object must comply with. Second, stats objects should not operate on any global or static data unless absolutely necessary for space constraints. This means if you have 100K nodes, e.g., each node should maintains its own histogram of message sizes. While some storage could be saved by aggregating results into a single object, in many cases the storage overhead is minimal. This is particularly important for thread safety that stats collection be done on independent, non-interfering objects. At the very end, the `stat_collector` interface defines hooks for aggregating results if you want, e.g., a global histogram for all nodes.

\section{Setting Up Objects} We use the example here of a the network interface histogram declared in `nic.h`.

````
class nic 
{
  ...
  stat_histogram* hist_msg_size_;
  ...
  nic() : hist_msg_size_(0)
  ...
````

Here the stats object is initialized to zero. The `stat_collector` object is a factory type. Thus individual stat collectors can be associated with string identifiers. For histogram, we declare in `stat_histogram.cc`

````
SpktRegister("histogram", stat_collector, stat_histogram);
````

Inside the `init_factory_params` function for `nic`, we check if the histogram stats should be activated

````
if (params->has_namespace("message_sizes")){
  params->enter_namespace("message_sizes");
  stat_collector* hist = stat_collector_factory::get_optional_param("type", "histogram", params);
  hist_msg_size_ = test_cast(stat_histogram, hist);
  params->exit_namespace("message_sizes");
}
````

It is possible that you create your own customized histogram object. However, here we default to the histogram provided by SST-macro. The histogram constructor initializes a few parameters internally.

````
bin_size_ = params->get_quantity("bin_size");
is_log_ = params->get_optional_bool_param("logarithmic", false);
fileroot_ = params->get_param("fileroot");
````

defining how large histogram bins are, whether the scale is logarithmic, and finally defining a file root for dumping results later.

This only defines a template histogram object, though. Recall from earlier sections, almost everything in SST-macro builds a template object and then clones copies of it to fill out the machine.

````
void
nic::clone_into(nic* nic) const
{
  if (hist_msg_size_){
    nic->hist_msg_size_ = hist_msg_size_->clone_me(parent_->id());
  }
````
If we have built a histogram object, create a copy of it but assign it a unique ID.
This unique ID is important for the runtime to distinguish a stat collector object for node 0 from that for node 1, node 2, etc.

When the simulation begins, every component gets assigned an `event_manager`. At this time, an object must register all statistics with the runtime

````
void
nic::set_event_manager(event_manager* m)
{
  if (hist_msg_size_) m->register_stat(hist_msg_size_);
````

At this point, the stats object is completely configured. The stats object copy has been assigned a unique ID and registered with the SST runtime. Each individual NIC can now start (thread-safely) collecting stats. Internally in the event manager, all objects with the same file root are grouped together. Thus the `fileroot` parameter is critical for defining unique groups of stats object. This is important during simulation post-processing when the event manager wants to aggregate results from each individual node.

### Section 8.1: Dumping Data<a name="sec:dumping"></a>

The first set of virtual functions that every stats object must provide are

````
virtual void
simulation_finished(timestamp end) = 0;

virtual void dump_local_data() = 0;

virtual void
dump_global_data() = 0;
````

`simulation_finished` tells the stats object what the final time of the simulation is and allows any final post-processing to be done. This is particularly useful in time-dependent analyses.  In other cases like message size histograms, it is a no-op. After the stats object has been notified of the simulation finishing, at some point the event manager will instruct it that it is safe to dump its data. The next method, `dump_local_data`, dumps the data specific to a given node. A unique filename based on the ID provided above in the `clone_me` function is created to hold the output. The last method, `dump_global_data`, dumps aggregate data for all nodes. Here a unique filename based on the file root parameter is generated. For the default histogram, a data file and gnuplot script are created.

### Section 8.2: Reduction and Aggregation<a name="sec:reduceStats"></a>

Before the `dump_global_data` function can be called, an aggregation of results must be performed. Each stats object is therefore required to provide the functions

````
virtual void
reduce(stat_collector* coll) = 0;

virtual void
global_reduce(parallel_runtime* rt) = 0;
````
The first function does a local reduce.
The object calling the `reduce` function aggregates data into itself from input parameter `coll`
The event manager automatically loops all objects registered to the same file root and reduces them into a global aggregator.
Once the aggregation is complete across all local copies,
a parallel global aggregation must be performed across MPI ranks.
This can be the most complicated part.
For histograms, this is quite easy.
A histogram is just a vector of integers.
The SST-macro parallel runtime object provides a set of reduce functions for automatically summing a vector.
For more complicated cases, packing/unpacking of data might need to be performed or more complicated parallel operations.
Once the global reduce is done, the event manager is now safe to call `dump_global_data`.
When developing new stats we recommend running medium-sized jobs as a single thread, multi-threaded, and in MPI parallel to confirm the answer is the same.

For the histogram, the reduce functions are quite simple

````
void
stat_histogram::reduce(stat_collector *coll)
{
  stat_histogram* other = safe_cast(stat_histogram, coll);

/** make sure we have enough bins to hold results */ int max_num = std::max(counts_.size(), other->counts_.size()); if (max_num > counts_.size()){ counts_.resize(max_num); }

/** loop all bins to aggregate results */
  int num_bins = other->counts_.size();
  for (int i=0; i < num_bins; ++i){
    counts_[i] += other->counts_[i];
  }
}
````

and for the global reduce
````
void
stat_histogram::global_reduce(parallel_runtime* rt)
{
  int root = 0;
  /** Align everyone to have the same number of bins */
  int my_num_bins = counts_.size();
  int num_bins = rt->global_max(my_num_bins);
  counts_.resize(num_bins);

/** Now global sum the data vector */
  rt->global_sum(&counts_[0], num_bins, root);
}
````

### Section 8.3: Storage Contraints<a name="sec:storageStats"></a>

In some cases, storage constraints prevent each node from having its own copy of the data. This is particularly important for the fixed-time quanta charts which generate several MB of data even in the reduced, aggregated form. In this case it is acceptable to operate on global or static data. However, as much as possible, you should maintain the illusion of each component having an individual copy. For example, a NIC should not declare

````
class nic {
 ...
 static ftq_calendar* ftq_;
````
but instead
````
class nic {
 ...
 ftq_calendar* ftq_;
````

Inside the `ftq_calendar` object you can then declare
````
class ftq_calendar {
 ...
 static thread_lock lock_;
 static std::vector<ftq_epoch> results_;
````
which creates a static, aggregated set of results.
The `ftq_calendar` must ensure thread-safety itself via a thread-lock.







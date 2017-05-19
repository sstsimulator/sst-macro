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

#ifndef SPROCKIT_SPROCKIT_PREPROCESSOR_H_
#define SPROCKIT_SPROCKIT_PREPROCESSOR_H_

#include <sprockit/spkt_config.h>

#if SPKT_HAVE_CPP11
#  define spkt_static_assert(...) static_assert(__VA_ARGS__)
#endif

//================================================================================//
//================================================================================//
//================================================================================//

#if SPKT_HAVE_CPP11
// Note: these validation metafunctions will not work if the class in question is final
#include <type_traits>  // for std::integral_constant
/**
 * A macro to generate a metafunction that checks if a class has a member type with a name
 *   given by the macro parameter.
 * Adapted from http://en.wikibooks.org/wiki/More_C%2B%2B_Idioms/Member_Detector
 *
 * Example usage: (adapted from the same page as above)
 *
 *   GENERATE_HAS_MEMBER_TYPE(Foo) // Creates 'has_member_type_Foo'.
 *
 *   struct A
 *   {
 *       struct Foo;
 *   };
 *
 *   struct B
 *   {
 *       using Foo = int;
 *   };
 *
 *   struct D
 *   {
 *       typedef char* Foo;
 *   };
 *
 *   struct C : A, B { }; // Will also work on incomplete or ambiguous types.
 *
 *
 *   int main ( )
 *   {
 *       std::cout << std::boolalpha
 *                 << "'Foo' in 'C' : "
 *                 << sprockit::validation::has_member_type_Foo<C>::value
 *                 << "\n";
 *                 << "'Foo' in 'D' : "
 *                 << sprockit::validation::has_member_type_Foo<D>::value
 *                 << "\n";
 *   }
 */
#define SPKT_GENERATE_HAS_MEMBER_TYPE(Type)                                       \
                                                                                  \
namespace sprockit { namespace validation {                                       \
template < class T >                                                              \
class HasMemberType_##Type                                                        \
{                                                                                 \
private:                                                                          \
    using Yes = char[2];                                                          \
    using  No = char[1];                                                          \
                                                                                  \
    struct Fallback { struct Type { }; };                                         \
    struct Derived : T, Fallback { };                                             \
                                                                                  \
    template < class U >                                                          \
    static No& test ( typename U::Type* );                                        \
    template < typename U >                                                       \
    static Yes& test ( U* );                                                      \
                                                                                  \
public:                                                                           \
    static constexpr bool RESULT = sizeof(test<Derived>(nullptr)) == sizeof(Yes); \
};                                                                                \
                                                                                  \
template < class T >                                                              \
struct has_member_type_##Type                                                     \
: public std::integral_constant<bool, HasMemberType_##Type<T>::RESULT>            \
{ };                                                                              \
} } /* end of namespaces */

// And a flag to let the compiler know this feature is available
#  define SPKT_HAS_MEMBER_TYPE_VALIDATION 1

////////////////////////////////////////////////////////////////////////////////

/**
 * A macro to generate a metafunction that checks if a class has a member with a name
 *   given by the macro parameter.
 * Adapted from http://en.wikibooks.org/wiki/More_C%2B%2B_Idioms/Member_Detector
 *
 * Example usage: (adapted from the same page as above)
 *
 *   GENERATE_HAS_MEMBER(att)  // Creates 'has_member_att'.
 *   GENERATE_HAS_MEMBER(func) // Creates 'has_member_func'.
 *
 *
 *   struct A
 *   {
 *       int att;
 *       void func ( double );
 *   };
 *
 *   struct B
 *   {
 *       char att[3];
 *       double func ( const char* );
 *   };
 *
 *   struct C : A, B { }; // It will also work with ambiguous members.
 *
 *
 *   int main ( )
 *   {
 *       std::cout << std::boolalpha
 *                 << "\n" "'att' in 'C' : "
 *                 << has_member_att<C>::value // <type_traits>-like interface.
 *                 << "\n" "'func' in 'C' : "
 *                 << has_member_func<C>() // Implicitly convertible to 'bool'.
 *                 << "\n";
 *   }
 */
#define SPKT_GENERATE_HAS_MEMBER(member)                                          \
                                                                                  \
namespace sprockit { namespace validation {                                       \
template < class T >                                                              \
class HasMember_##member                                                          \
{                                                                                 \
private:                                                                          \
    using Yes = char[2];                                                          \
    using  No = char[1];                                                          \
                                                                                  \
    struct Fallback { int member; };                                              \
    struct Derived : T, Fallback { };                                             \
                                                                                  \
    template < class U >                                                          \
    static No& test ( decltype(U::member)* );                                     \
    template < typename U >                                                       \
    static Yes& test ( U* );                                                      \
                                                                                  \
public:                                                                           \
    static constexpr bool RESULT = sizeof(test<Derived>(nullptr)) == sizeof(Yes); \
};                                                                                \
                                                                                  \
template < class T >                                                              \
struct has_member_##member                                                        \
: public std::integral_constant<bool, HasMember_##member<T>::RESULT>              \
{ };                                                                              \
} } /* end namespaces */
// And a flag to let the compiler know this feature is available
#  define SPKT_HAS_MEMBER_VALIDATION 1

////////////////////////////////////////////////////////////////////////////////

// TODO overloaded member function detection macro

////////////////////////////////////////////////////////////////////////////////

#else // No validation when CPP11 is not available

// TODO non-c++11 versions of these macros

#endif // SPKT_HAVE_CPP11

//================================================================================//
//================================================================================//
//================================================================================//



#endif /* SPROCKIT_SPROCKIT_PREPROCESSOR_H_ */
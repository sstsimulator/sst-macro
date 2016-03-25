
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

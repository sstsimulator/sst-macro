#include <sstmac/util.h>
#include <sprockit/test/test.h>
#include <stdexcept>


class error1 : public std::runtime_error
{
    public:
    error1(const std::string& err)
        : std::runtime_error(err)
    {
    }
};

class error2 : public std::runtime_error
{
    public:
    error2(const std::string& err)
        : std::runtime_error(err)
    {
    }
};

template <class T>
void throw_exc(int arg){
    throw T("error");
}

void dont_throw_exc(int arg)
{
}

void throw_int(int arg)
{
    throw arg;
}

class MemberTestClass
{
    public:
        template <class T>
        void
        throw_exc(int arg)
        {
            throw T("error");
        }

        void
        dont_throw_exc(int arg)
        {
        }

        void
        throw_int(int arg)
        {
            throw arg;
        }

};

int main(int argc, char** argv)
{
    UnitTest unit;
    assertEqual(unit, "int equality", 3, 3);
    assertEqual(unit, "int equality", 3, 4);
    assertEqual(unit, "float equality", 3.14, 3.14);

    {
    std::vector<int> vec; fillContainer(vec, 1, 2, 3, 4);
    assertEqual(unit, "fill vector", vec, 1, 2, 3, 4);
    }

    {
    std::vector<int> vec; fillContainer(vec, 1, 2, 3, 4);
    assertNotEqual(unit, "fill vector", vec, 1, 2, 4, 4);
    }

    assertFalse(unit, "test false", false);
    assertTrue(unit, "test true", true);

    assertThrows(unit, "does throw", error1,
                 static_fxn(&throw_exc<error1>, 0));
    assertThrows(unit, "doesn't throw", error1,
                 static_fxn(&dont_throw_exc, 0));
    assertThrows(unit, "throws wrong", error2,
                 static_fxn(&throw_exc<error1>, 0));
    assertThrows(unit, "throws int", error2,
                 static_fxn(&throw_int, 20));

    {
    MemberTestClass* cls = new MemberTestClass;
    assertThrows(unit, "ptr member does throw", error1,
                 member_fxn(cls, &MemberTestClass::throw_exc<error1>, 0));
    assertThrows(unit, "ptr member doesn't throw", error1,
                 member_fxn(cls, &MemberTestClass::dont_throw_exc, 0));
    assertThrows(unit, "ptr member throws wrong", error2,
                 member_fxn(cls, &MemberTestClass::throw_exc<error1>, 0));
    assertThrows(unit, "ptr member throws int", error2,
                 member_fxn(cls, &MemberTestClass::throw_int, 20));
    }

    {
    MemberTestClass cls;
    assertThrows(unit, "member does throw", error1,
                 member_fxn(&cls, &MemberTestClass::throw_exc<error1>, 0));
    assertThrows(unit, "member doesn't throw", error1,
                 member_fxn(&cls, &MemberTestClass::dont_throw_exc, 0));
    assertThrows(unit, "member throws wrong", error2,
                 member_fxn(&cls, &MemberTestClass::throw_exc<error1>, 0));
    assertThrows(unit, "member throws int", error2,
                 member_fxn(&cls, &MemberTestClass::throw_int, 20));
    }

    unit.validate();
    //return UnitTest::exit_status(unit.validate());
    return 0;
}


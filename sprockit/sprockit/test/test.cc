#include <sprockit/test/test.h>

double TestEquals<double>::cutoff = 1e-12;
float TestEquals<float>::cutoff = 1e-12;

int
UnitTest::validate(std::ostream &os)
{
  int nfailed = 0;
  for (int i=0; i < tests_.size(); ++i) {
    TestCase* test = tests_[i];
    bool success = test->is_correct();
    if (success) {
      os << "SUCCESS: ";
      test->print_descr(os);
    }
    else {
      ++nfailed;
      os << "FAILURE: ";
      test->print_descr(os);
      os << "  ";
      test->print_error(os);
      os << "\n\t     Test: ";
      test->print_test_value(os);
      os << "\n\t Asserted: ";
      test->print_asserted_value(os);
    }
    os << "\n";
  }
  return nfailed;
}

void _assertFalse(UnitTest& test_set,
                  const char* descr,
                  const char* file,
                  int line,
                  bool test)
{
  AssertEqual<bool,bool>::add_test(test_set, descr, file, line, test, false,
                                   true);
}

void _assertTrue(UnitTest& test_set,
                 const char* descr,
                 const char* file,
                 int line,
                 bool test)
{
  AssertEqual<bool,bool>::add_test(test_set, descr, file, line, test, true, true);
}

const char*
truncate_file(const char* file)
{
  int len = ::strlen(file);
  const char* charptr = file + len;
  while (charptr != file) {
    --charptr;
    if (*charptr == '/') {
      return (charptr+1);
    }
  }
  return file;
}


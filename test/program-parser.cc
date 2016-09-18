#include <boost/test/unit_test.hpp>
#include <scribbu/program-parser.hh>
#include <scribbu/program-lexer.hh>

namespace {

  bool test_program(const std::string &P) {

    YY_BUFFER_STATE buf = program_scan_string(P.c_str());
    program_switch_to_buffer(buf); // switch flex to the buffer we just created
    int status = programparse(0);
    return 0 == status;

  }

}

extern int programdebug;

BOOST_AUTO_TEST_CASE( test_programs )
{
  using namespace std;

  const vector<string> CONDITIONS = {
      "artist == \"Pogues, The\"",
      "artist == \"artist\"",
      "artist == \"Pogues, The\" && album == \"Lorca's Novena\"",
      "encoded-by == \"sp1ff\" || content-type == \"Rock\"",
      "! artist present",
      "(! album present) && artist present",
      "artist contains \"Pogues\"",
      "if (! artist present) { print; }",
      "if (! (artist present)) { print; }",
      "if (! artist present) { set artist = \"Pogues, The\"; }",
    };

  programdebug = 1;
  for (auto c: CONDITIONS) {
    BOOST_CHECK_MESSAGE(test_program(c), c);
  }

}

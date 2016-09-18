#include "command.hh"

#include <stdexcept>
#include <boost/lexical_cast.hpp>
#include <boost/regex.hpp>

std::ostream&
operator<<(std::ostream &os, command x)
{
  switch (x) {
  case command::dump:
    os << "dump";
    break;
  case command::find:
    os << "find";
    break;
  case command::report:
    os << "report";
    break;
  case command::rename:
    os << "rename";
    break;
  case command::run:
    os << "run";
    break;
  case command::split:
    os << "split";
    break;
  default:
    throw std::logic_error("Unknown command");
  }
  return os;
}

std::istream&
operator>>(std::istream &is, command &x)
{
  using namespace boost;

  static const regex RE_DUMP  ( "(dump|print)"  );
  static const regex RE_FIND  ( "find"          );
  static const regex RE_REPORT( "(report|list)" );
  static const regex RE_RENAME( "rename"        );
  static const regex RE_RUN   ( "(run|execute)" );
  static const regex RE_SPLIT ( "split"         );

  std::string s;
  is >> s;

  if        (regex_match(s, RE_DUMP  )) {
    x = command::dump;
  } else if (regex_match(s, RE_FIND  )) {
    x = command::find;
  } else if (regex_match(s, RE_REPORT)) {
    x = command::report;
  } else if (regex_match(s, RE_RENAME)) {
    x = command::rename;
  } else if (regex_match(s, RE_RUN  )) {
    x = command::run;
  } else if (regex_match(s, RE_SPLIT)) {
    x = command::split;
  } else {
    throw std::runtime_error(s + ": unknown command");
  }

  return is;
}

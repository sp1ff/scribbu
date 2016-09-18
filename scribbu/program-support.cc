#include "program-support.hh"

#include <typeinfo>


#define ACCEPT(clazz)                                                   \
  void scribbu::program::clazz::accept(syntax_node_visitor &v) {        \
    try {                                                               \
      clazz##_visitor &V = dynamic_cast<clazz##_visitor&>(v);           \
      V.visit(*this);                                                   \
    }                                                                   \
    catch (const std::bad_cast&) {                                      \
    }                                                                   \
  }

///////////////////////////////////////////////////////////////////////////////

ACCEPT(integer_literal)
ACCEPT(get_text_tag)
ACCEPT(static_text)
ACCEPT(equality_term)
ACCEPT(presence_term)
ACCEPT(textual_term)
ACCEPT(set_text_tag)
ACCEPT(term_condition)
ACCEPT(and_condition)
ACCEPT(or_condition)
ACCEPT(not_condition)
ACCEPT(conditional)
ACCEPT(statement)

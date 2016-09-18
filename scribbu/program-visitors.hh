#ifndef PROGRAM_VISITORS_HH_INCLUDED
#define PROGRAM_VISITORS_HH_INCLUDED 1

namespace scribbu {

  namespace program {

    struct print_visitor {
      virtual void visit(const print&);
    };

    struct condition_visitor {
      virtual void visit(const condition&);
    };

  } // End namespace program.

} // End namespace scribbu.

#endif // not PROGRAM_VISITORS_HH_INCLUDED

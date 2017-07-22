#ifndef SCHEME_HH
#define SCHEME_HH
#include <libguile.h>

// TODO(sp1ff): This is my first attempt at integrating Guile into scribbu. I'm
// writing provisionally; I may delete this & re-write. I may factor out Guile
// support into a separate library.

extern "C" {

  /**
   * \brief Initializae the Guile interpreter
   *
   *
   * \param p [in] ignored
   *
   * \return always returns nil
   *
   *
   * This function is intended to be called from within scm_with_guile; it will:
   * 
   *   <li>define the scribbu Scheme module
   *   <li>define all scribbu foreign ojbect types
   *   <li>define all scribbu internal functions
   *
   *
   */

  void*
  initialize_guile(void*);

}

#endif // not SCHEME_HH

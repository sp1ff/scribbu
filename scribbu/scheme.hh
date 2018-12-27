/**
 * \file scheme.hh
 *
 * Copyright (C) 2015-2019 Michael Herstine <sp1ff@pobox.com>
 *
 * This file is part of scribbu.
 *
 * scribbu is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * scribbu is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with scribbu.  If not, see <http://www.gnu.org/licenses/>. *
 *
 *
 */

#ifndef SCHEME_HH
#define SCHEME_HH
#include <libguile.h>

extern "C" {

  /**
   * \brief Initializae the Guile interpreter
   *
   *
   * \return always returns nil
   *
   *
   * This function is intended to be called from within scm_with_guile; it will:
   *
   *   <li>define the scribbu Scheme module
   *   <li>define all scribbu foreign ojbect types
   *   <li>define all scribbu exported functions
   *
   *
   */

  void*
  initialize_guile(void*);

}

#endif // not SCHEME_HH

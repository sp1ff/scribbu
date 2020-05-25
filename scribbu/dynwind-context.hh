/**
 * \file dynwind-context.hh
 *
 * Copyright (C) 2019-2020 Michael Herstine <sp1ff@pobox.com>
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

#ifndef DYNWIND_CONTEXT_HH
#define DYNWIND_CONTEXT_HH

#include <libguile.h>

namespace scribbu {

  /// Stack-based allocator for scm_dynwind_begin/end
  class dynwind_context
  {
  public:
    dynwind_context(bool rewindable = false);
    ~dynwind_context()
    { scm_dynwind_end(); }
    void free(void *p)
    { scm_dynwind_free(p); }
    char* free_locale_string(SCM scm);
    char* free_utf8_string(SCM scm);

    dynwind_context(const dynwind_context&)            = delete;
    dynwind_context(dynwind_context&&)                 = delete;
    dynwind_context& operator=(const dynwind_context&) = delete;
    dynwind_context& operator=(dynwind_context&&)      = delete;

  };

} // End namespace scribbu.

#endif // DYNWIND_CONTEXT_HH

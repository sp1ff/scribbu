/**
 * \file dynwind-context.cc
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

#include "dynwind-context.hh"

scribbu::dynwind_context::dynwind_context(bool rewindable /*= false*/)
{
  scm_dynwind_begin( (scm_t_dynwind_flags)
                     (rewindable ? SCM_F_DYNWIND_REWINDABLE : 0));
}

char* scribbu::dynwind_context::free_locale_string(SCM scm)
{
  char *p = scm_to_locale_string(scm);
  scm_dynwind_free(p);
  return p;
}

char* scribbu::dynwind_context::free_utf8_string(SCM scm)
{
  char *p = scm_to_utf8_string(scm);
  scm_dynwind_free(p);
  return p;
}

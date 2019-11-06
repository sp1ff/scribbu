/**
 * \file tagset.cc
 *
 * Copyright (C) 2019 Michael Herstine <sp1ff@pobox.com>
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

#include <tagset.hh>

///////////////////////////////////////////////////////////////////////////////
//                        class invalid_tagset_request                       //
///////////////////////////////////////////////////////////////////////////////

/*virtual*/
const char * scribbu::invalid_tagset_request::what() const noexcept
{
  if (!pwhat_) {
    switch (cause_) {
    case emplace_mismatch_sz:
      pwhat_.reset(new std::string("replace_tagset_emplace called with a "
                                   "new tagset whose size does not match "
                                   "the existing tagset"));
      break;
    case adj_pad_not_enough:
      pwhat_.reset(new std::string("adjust_padding_to called to reduce "
                                   "tagset size, but there's not enough "
                                   "padding to satisfy the requested "
                                   "size"));
      break;
      // LATER(sp1ff): how to architect an exception class where this isn't
      // necessary
    default:
      // Should never happen
      throw std::logic_error("you didn't keep invalid_tagset_request::"
                             "cause up-to-date");
    }
  }
  return pwhat_->c_str();
}




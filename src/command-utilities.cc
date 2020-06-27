/**
 * \file command-utilities.cc
 *
 * \brief Assorted utilities for scribbu & its sub-commands
 *
 *
 * Copyright (C) 2015-2020 Michael Herstine <sp1ff@pobox.com>
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

#include "command-utilities.hh"

#include <algorithm>
#include <numeric>
#include <sstream>
#include <map>

#include <boost/algorithm/string/replace.hpp>

#include <scribbu/id3v1.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/id3v22.hh>
#include <scribbu/id3v23.hh>
#include <scribbu/id3v24.hh>
#include <scribbu/id3v2-utils.hh>
#include <scribbu/tagset.hh>

namespace fs = boost::filesystem;
namespace po = boost::program_options;

////////////////////////////////////////////////////////////////////////////////
//                      types & methods related to help                       //
////////////////////////////////////////////////////////////////////////////////

std::tuple<help_level, boost::optional<verbose_flavor>>
help_level_for_parsed_opts(const po::parsed_options &opts)
{
  using namespace std;
  using boost::optional;

  help_level out = help_level::none;
  optional<verbose_flavor> flav = boost::none;

  for (auto o: opts.options) {
    for (auto t: o.original_tokens) {
      if (t == "-h") {
        out = help_level::regular;
        break;
      } else if (t == "--help" || t == "--man") {
        out = help_level::verbose;
        flav = verbose_flavor::man;
        break;
      } else if (t == "--info") {
        out = help_level::verbose;
        flav = verbose_flavor::info;
        break;
      }
    }
  }

  return make_tuple(out, flav);

}

void
print_usage(std::ostream                  &os,
            const po::options_description &opts,
            const std::string             &usage)
{
  using namespace std;

  // Workaround for https://svn.boost.org/trac10/ticket/4644; marked as fixed
  // some time ago, but still happening...
  stringstream stm4644;
  stm4644 << opts;
  string str4644 = stm4644.str();
  boost::algorithm::replace_all(str4644, "--h ", "-h  ");
  os << usage << str4644  << std::endl;
}

void
show_man_page(const std::string &page)
{
  using namespace std;

  execlp("man", "man", page.c_str(), (char *)NULL);
  // If we're here, `execlp' failed.
  stringstream stm;
  stm << "Failed to exec man: [" << errno << "]: " << strerror(errno);
  throw runtime_error(stm.str());
}

void
show_info_node(const std::string &node)
{
  using namespace std;

  execlp("info", "info", node.c_str(), (char *)NULL);
  // If we're here, `execlp' failed.
  stringstream stm;
  stm << "Failed to exec info: [" << errno << "]: " << strerror(errno);
  throw runtime_error(stm.str());
}

void
maybe_handle_help(const po::parsed_options      &opts,
                  const po::options_description &dsc,
                  const std::string             &usage,
                  const std::string             &page,
                  const std::string             &node)
{
  using namespace std;

  using boost::optional;

  help_level level;
  optional<verbose_flavor> flav;
  std::tie(level, flav) = help_level_for_parsed_opts(opts);

  if (help_level::none == level) {
    return;
  } else if (help_level::regular == level) {
    print_usage(cout, dsc, usage);
    exit(EXIT_SUCCESS);
  } else {
    if (verbose_flavor::man == flav) {
      show_man_page(page);
    } else {
      show_info_node(node);
    }
  }

}

////////////////////////////////////////////////////////////////////////////////
//                           class tagset_processor                           //
////////////////////////////////////////////////////////////////////////////////

namespace {

  /// Read a tagset from file
  template <typename FOI> // Forward Output Iterator
  std::unique_ptr<scribbu::id3v1_tag>
  read_tagset(const fs::path &pth, FOI pout)
  {
    using namespace std;

    const std::ios::iostate EXC_MASK = std::ios::eofbit  |
                                       std::ios::failbit |
                                       std::ios::badbit;

    fs::ifstream ifs(pth, ios_base::binary);
    ifs.exceptions(EXC_MASK);
    scribbu::read_all_id3v2(ifs, pout);
    return scribbu::process_id3v1(ifs);
  }

}

// For internal use through delegation
tagset_processor::tagset_processor(v2_tag_scope_policy v2tsp,
                                   v1_tag_scope_policy v1tsp,
                                   v2_creation_policy v2c,
                                   v1_creation_policy v1c,
                                   bool dry_run,
                                   bool verbose,
                                   bool adjust_unsync,
                                   bool create_backups):
  v1_creation_policy_(v1c),
  v2_creation_policy_(v2c),
  v1_tag_scope_policy_(v1tsp),
  v2_tag_scope_policy_(v2tsp),
  dry_run_(dry_run),
  verbose_(verbose),
  adjust_unsync_(adjust_unsync),
  create_backups_(create_backups)
{ }


/**
 * \brief Process the tagset contained in \a pth; in an implementation-specifico
 * manner
 *
 *
 * \param pth [in] filesystem path (absolute or relative to the present working
 * directory) of the file to be processed
 *
 *
 * By "tagset", I mean the collection of ID3v2 tags & ID3v1 tag present in a
 * given file. This method will:
 *
 * 1. read the tagset, if any, from \a pth
 *
 * 2. if there are one or more ID3v2 tags present, process them according to
 *    v2_tag_scope_policy_ & the implementation; else, if not, maybe create one
 *    according to v2_creation_policy_ and the implementation.
 *
 * 3. if there is an ID3v1 tag present, process it according to
 *    v1_tag_scope_policy_ & the implementation; else, if not, maybe create one
 *    according to v1_creation_poilcy_ and the implementation.
 *
 * 4. if steps 2 & 3 resulted in any changes to the tagset either print a
 *    message to that effect (if dry_run is true), or write the new tagset to \a
 *    pth if it is false. A backup file will be created if create_backups is
 *    true, else the existing file will be overwritten. The ID3v2 tagset will be
 *    emplaced, rather than re-written, if possible. The unsynchronisation
 *    flag will be set according to adj_unsync.
 *
 *
 */

void
tagset_processor::process_file(const fs::path &pth)
{
  using namespace std;
  using namespace scribbu;

  vector<unique_ptr<id3v2_tag>> v2;
  unique_ptr<id3v1_tag> pv1 = read_tagset(pth, back_inserter(v2));

  bool dirty = false; // Will => true if the tagset is changed

  unique_ptr<id3v2_tag> pnv2; // holds address of newly create ID3v2 tag, if any
  if (v2.empty()) {
    // We have no ID3v2 tags in this file; create one?
    //            v2_create_policy
    //         |     | never | when v1 present   | always           |
    // ID3v1   |-----+-------+-------------------+------------------|
    // tag     | yes | no    | yes (with v1 tag) | yes (with v1 tag |
    // present | no  | no    | no                | yes              |
    //         |-----+-------+-------------------+------------------|
    if (pv1 && v2_creation_policy_ != v2_creation_policy::never) {
      pnv2 = create_v2(*pv1);
    } else if (!pv1 && v2_creation_policy_ == v2_creation_policy::always) {
      pnv2 = create_v2();
    }
  } else if (v2_tag_scope_policy_ != v2_tag_scope_policy::none) {
    // `tags' will contain the indicies of the ID3v2 tags to be processed
    deque<size_t> tags;
    if (v2_tag_scope_policy_ == v2_tag_scope_policy::all) {
      // we are to process all of 'em-- just fill `tags' with 0,1,2...
      tags.resize(v2.size());
      iota(tags.begin(), tags.end(), 0);
    } else { // v2_tag_scope_policy_ = some
      tags = v2_tags_;
    }

    // Ok-- now we walk the pointers to `id3v2_tag' in `v2'
    for (size_t i = 0; i < v2.size(); ++i) {
      if (tags.front() == i) {
        bool upd = false;

        id3v2_tag *ptag = v2[i].get();
        unsigned char version = ptag->version();
        if (2 == version) {
          id3v2_2_tag &tag = dynamic_cast<id3v2_2_tag&>(*ptag);
          upd = process_v2(tag);
        } else if (3 == version) {
          id3v2_3_tag &tag = dynamic_cast<id3v2_3_tag&>(*ptag);
          upd = process_v2(tag);
        } else if (4 == version) {
          id3v2_4_tag &tag = dynamic_cast<id3v2_4_tag&>(*ptag);
          upd = process_v2(tag);
        } else {
          // TODO(sp1ff): throw something more specific?
          throw std::logic_error("unknown ID3v2 version!");
        }

        tags.pop_front();
        dirty = dirty || upd;
      } // End if on `tags'
    } // End iteration over `v2'
  } // End if on the presence or absence of ID3v2 tags in `pth'.

  // Right-- that's the ID3v2 tags sorted. Now for the ID3v1 tag (if any):
  unique_ptr<id3v1_tag> pnv1; // will hold the address of newly created ID3v1 tag
  if (!pv1) {
    // We have no ID3v1 tag in this file; create one?
    //            v1_creation_policy
    // >= 1     |     | never | when v2 present    | always             |
    // ID3v2    |-----+-------+--------------------+--------------------+
    // tag      | yes | no    | yes (with v2 tags) | yes (with v2 tags) |
    // present? | no  | no    | no                 | yes                |
    //          |-----+-------+--------------------+--------------------+
    if (!v2.empty() && v1_creation_policy_ != v1_creation_policy::never) {
      pnv1 = create_v1(v2);
    } else if (v2.empty() && v1_creation_policy_ == v1_creation_policy::always) {
      pnv1 = create_v1();
    }
  } else if (v1_tag_scope_policy_ != v1_tag_scope_policy::no) {
    dirty |= process_v1(*pv1);
  }

  // OK-- processing logic done. Update our tagset with any newly created tags:
  if (pnv2) {
    // TODO(sp1ff): realy!?
    // v2.resize(1);
    v2.push_back(std::move(pnv2));
  }
  if (pnv1) {
    pv1.swap(pnv1);
  }

  // Wheh! Write the tagset out
  write_tagset(dirty, v2.begin(), v2.end(), pv1, pth);
}

////////////////////////////////////////////////////////////////////////////////
//                           sub-command utilities                            //
////////////////////////////////////////////////////////////////////////////////

detail::handler_map& detail::get_handler_map()
{
  static handler_map the_map;
  return the_map;
}

register_command::register_command(const std::string &s, handler_type f)
{
  detail::handler_map &M = detail::get_handler_map();
  if (M.count(s)) {
    throw std::out_of_range(s);
  }
  M[s] = f;
}

bool
has_sub_command(const char *s)
{
  return 0 != detail::get_handler_map().count(s);
}

handler_type get_sub_command(const char *s)
{
  return detail::get_handler_map().at(s);
}


/// Create a new ID3v2 tag given an ID3v1 tag
std::unique_ptr<scribbu::id3v2_tag>
copy_id3_v1(const scribbu::id3v1_tag &v1)
{
  using namespace std;
  using namespace scribbu;

  // TODO(sp1ff): numeric literal
  unique_ptr<id3v2_tag> p(new id3v2_3_tag(1024));

  string text = v1.artist<string>();
  if (text.length()) p->text(id3v2_text_frames::tpe1, text);
  text = v1.title<string>();
  if (text.length()) p->text(id3v2_text_frames::tit2, text);
  text = v1.album<string>();
  if (text.length()) p->text(id3v2_text_frames::talb, text);

  // TODO(sp1ff): bring over the comment field, if any

  unsigned char genre = v1.genre();
  if (192 > genre) {
    auto opt = text_for_genre(genre);
    p->content_type(*opt);
  }

  text = v1.year<string>();
  if (4 == text.length()) p->year(text);

  return p;
}

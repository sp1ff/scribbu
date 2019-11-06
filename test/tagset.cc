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

#include <scribbu/tagset.hh>
#include <scribbu/id3v23.hh>
#include <scribbu/id3v2-utils.hh>

#include "unit.hh"

#include <boost/test/unit_test.hpp>

namespace fs = boost::filesystem;

namespace {

  size_t count_files(const fs::path &pth)
  {
    size_t num_files = 0;
    for (fs::directory_iterator p0(pth), p1; p0 != p1; ++p0) {
      switch (p0->status().type()) {
      case fs::regular_file:
      case fs::symlink_file:
      case fs::block_file:
      case fs::character_file:
      case fs::fifo_file:
      case fs::socket_file:
        ++num_files;
        break;
      default: // Get teh compiler to shut-up
        break;
      }
    }
    return num_files;
  }

}

BOOST_AUTO_TEST_CASE( test_get_backup_name )
{
  using scribbu::detail::get_backup_name;

  fs::path tmp = fs::temp_directory_path() / fs::unique_path();
  fs::create_directories(tmp);

  fs::path a = tmp / "a.mp3";

  fs::path bu0 = get_backup_name(a);

  BOOST_CHECK( bu0.filename().string() == "a.mp3.1" );

  fs::ofstream ofs(a);
  ofs << "a";
  ofs.close();

  fs::path bu1 = get_backup_name(a);

  BOOST_CHECK( bu1.filename().string() == "a.mp3.1" );

  boost::system::error_code ec;
  fs::copy_file(a, bu1, ec);

  fs::path bu2 = get_backup_name(a);

  BOOST_CHECK( bu2.filename().string() == "a.mp3.2" );

  fs::copy_file(a, bu2, ec);

  fs::path bu3 = get_backup_name(a);

  BOOST_CHECK( bu3.filename().string() == "a.mp3.3" );

  fs::copy_file(a, bu3, ec);

  fs::remove(bu2);

  fs::path bu4 = get_backup_name(a);

  BOOST_CHECK( bu4.filename().string() == "a.mp3.4" );

  fs::remove_all(tmp);

}

BOOST_AUTO_TEST_CASE( test_replace_tagset_copy )
{
  using namespace std;
  using namespace scribbu;

  const fs::path CERULEAN("cerulean.mp3");

  // Cop cerulean.mp3 to a test directory...
  fs::path tmp = fs::temp_directory_path() / fs::unique_path();
  boost::system::error_code ec;
  fs::create_directories(tmp, ec);

  fs::path src = get_data_directory() / CERULEAN, test = tmp / CERULEAN;

  fs::copy_file(src, test, ec);

  // Ensure our tmp file is writable
  fs::permissions(test, fs::perms::add_perms|fs::perms::owner_write);

  unsigned char test_md5[16];
  compute_md5(test, test_md5);

  // create a new tagset-- let's just do an ID3v2.3 tag with artist, title &
  // album:
  vector<unique_ptr<id3v2_tag>> tagset;
  tagset.emplace_back(unique_ptr<id3v2_tag>(new id3v2_3_tag));

  id3v2_3_tag &S = dynamic_cast<id3v2_3_tag&>(*(tagset[0]));

  S.push_back(id3v2_3_text_frame("TPE1", "Ocean Blue, The"));
  S.push_back(id3v2_3_text_frame("TIT2", "Questions of Travel (LP Version)"));
  S.push_back(id3v2_3_text_frame("TALB", "Cerulean (U.S. Release)"));

  // What should happen here is that `test' will be copied to a backup file (in
  // `tmp') named "${tmp}/cerulean.mp3.1". Then, "${tmp}/cerulean.mp3" will have
  // its tagset repalced with `tagset'-- it won't be an emplace operation, since
  // we specifically requested a copy.
  BOOST_CHECK_NO_THROW(
                       replace_tagset_copy(test, tagset.begin(), tagset.end(), apply_unsync::as_needed) );

  fs::path bu(test.string() + ".1");
  BOOST_CHECK( fs::exists(bu) );

  unsigned char bu_md5[16];
  compute_md5(bu, bu_md5);
  BOOST_CHECK( 0 == memcmp(test_md5, bu_md5, 16) );


  vector<unique_ptr<id3v2_tag>> new_tagset;
  fs::ifstream ifs(test, fs::ifstream::binary);
  read_all_id3v2(ifs, back_inserter(new_tagset));

  BOOST_REQUIRE( 1 == new_tagset.size() );

  BOOST_CHECK( 3 == new_tagset[0]->version() );

  const id3v2_3_tag &T = dynamic_cast<const id3v2_3_tag&>(*(new_tagset[0]));

  BOOST_CHECK( "Ocean Blue, The" == T.artist() );
  BOOST_CHECK( "Questions of Travel (LP Version)" == T.title() );
  BOOST_CHECK( "Cerulean (U.S. Release)" == T.album() );


  BOOST_CHECK( 3 == T.num_frames() );

  auto q = T.begin();
  BOOST_CHECK( "TPE1" == q->id() ); q++;
  BOOST_CHECK( "TIT2" == q->id() ); q++;
  BOOST_CHECK( "TALB" == q->id() ); q++;
  BOOST_CHECK( q == T.end() );

  // If we made it this far, clean-up the test directory behind us
  fs::remove_all(tmp);
}

BOOST_AUTO_TEST_CASE( test_replace_tagset_emplace )
{
  using namespace std;
  using namespace scribbu;

  const fs::path CERULEAN("cerulean.mp3");

  // Cop cerulean.mp3 to a test directory...
  fs::path tmp = fs::temp_directory_path() / fs::unique_path();
  fs::create_directories(tmp);

  fs::path src = get_data_directory() / CERULEAN, test = tmp / CERULEAN;

  boost::system::error_code ec;
  fs::copy_file(src, test, ec);
  // Ensure our tmp file is writable
  fs::permissions(test, fs::perms::add_perms|fs::perms::owner_write);

  size_t cb_curr = tagset_size(test);

  vector<unique_ptr<id3v2_tag>> tagset;
  tagset.emplace_back(unique_ptr<id3v2_tag>(new id3v2_3_tag));

  id3v2_3_tag &S = dynamic_cast<id3v2_3_tag&>(*(tagset[0]));

  S.push_back(id3v2_3_text_frame("TPE1", "Ocean Blue, The"));
  S.push_back(id3v2_3_text_frame("TIT2", "Questions of Travel (LP Version)"));
  S.push_back(id3v2_3_text_frame("TALB", "Cerulean (U.S. Release)"));

  size_t cb_new, cb_pad;
  tie(cb_new, cb_pad) = tagset_sizes(tagset.begin(), tagset.end(), apply_unsync::as_needed);

  BOOST_REQUIRE( cb_new < cb_curr );
  BOOST_CHECK( 0 == cb_pad );

  S.padding(cb_curr - cb_new);

  // What should happen is that `S' will be written overthe old tag in `test'
  // without any copy being made (the new bytes are just overwritten).
  BOOST_CHECK_NO_THROW(
    replace_tagset_emplace(test, tagset.begin(), tagset.end(), apply_unsync::as_needed) );

  size_t cb_after = tagset_size(test);
  BOOST_CHECK(cb_after == cb_curr);

  fs::ifstream ifs(test, fs::ifstream::binary);
  vector<unique_ptr<id3v2_tag>> new_tagset;
  read_all_id3v2(ifs, back_inserter(new_tagset));

  BOOST_CHECK(1 == new_tagset.size());
  size_t after_pad = new_tagset[0]->padding();
  BOOST_CHECK( after_pad == cb_curr - cb_new );

  const id3v2_3_tag &T = dynamic_cast<const id3v2_3_tag&>(*(new_tagset[0]));

  BOOST_CHECK( "Ocean Blue, The" == T.artist() );
  BOOST_CHECK( "Questions of Travel (LP Version)" == T.title() );
  BOOST_CHECK( "Cerulean (U.S. Release)" == T.album() );

  // If we made it this far, clean-up the test directory behind us
  fs::remove_all(tmp);
}

BOOST_AUTO_TEST_CASE( test_adjust_padding_to )
{
  using namespace std;
  using namespace scribbu;

  // Let's create an array of two tags:
  vector<unique_ptr<id3v2_3_tag>> tags;
  tags.emplace_back(unique_ptr<id3v2_3_tag>(new id3v2_3_tag));
  tags.emplace_back(unique_ptr<id3v2_3_tag>(new id3v2_3_tag));


  tags[0]->push_back(id3v2_3_text_frame("TPE1", string("Foo"), encoding::ASCII));
  tags[1]->push_back(id3v2_3_text_frame("TIT2", string("Bar"), encoding::ASCII));

  // their padding should both be zero at this moment...
  BOOST_CHECK( 0 == tags[0]->padding() );
  BOOST_CHECK( 0 == tags[1]->padding() );

  BOOST_REQUIRE( 14 == tags[0]->size() );
  BOOST_REQUIRE( 14 == tags[1]->size() );

  // Each is 24 bytes on disk, or 48 altogether, Let's increase that to 99.
  adjust_padding_to(tags.begin(), tags.end(), 99, 48,
                    padding_strategy::adjust_padding_evenly);

  // 99 - 48 = 51 => 26 for tags[0] & 25 for tags[1]
  BOOST_CHECK( 26 == tags[0]->padding() );
  BOOST_CHECK( 25 == tags[1]->padding() );

  // Now let's shrink them
  adjust_padding_to(tags.begin(), tags.end(), 50, 99,
                    padding_strategy::adjust_padding_evenly);
  BOOST_CHECK( 1 == tags[0]->padding() );
  BOOST_CHECK( 1 == tags[1]->padding() );
}

BOOST_AUTO_TEST_CASE( test_maybe_emplace_tagset )
{
  using namespace std;
  using namespace scribbu;

  const fs::path CERULEAN("cerulean.mp3");

  // Cop cerulean.mp3 to a test directory...
  fs::path tmp = fs::temp_directory_path() / fs::unique_path();
  fs::create_directories(tmp);

  fs::path src = get_data_directory() / CERULEAN, test = tmp / CERULEAN;

  boost::system::error_code ec;
  fs::copy_file(src, test, ec);
  // Ensure our tmp file is writable
  fs::permissions(test, fs::perms::add_perms|fs::perms::owner_write);


  // Let's create a very simple tagset (something that can easily be emplaced)
  vector<unique_ptr<id3v2_3_tag>> tags;
  tags.emplace_back(unique_ptr<id3v2_3_tag>(new id3v2_3_tag));
  tags.emplace_back(unique_ptr<id3v2_3_tag>(new id3v2_3_tag));

  tags[0]->push_back(id3v2_3_text_frame("TPE1", string("foo"), encoding::ASCII));
  tags[1]->push_back(id3v2_3_text_frame("TIT2", string("bar"), encoding::ASCII));

  // Each is 24 bytes on disk, or 48 altogether-- we can easily emplace.
  BOOST_CHECK_NO_THROW(
    maybe_emplace_tagset(test, tags.begin(), tags.end(), apply_unsync::as_needed,
                         emplace_strategy::reduce_padding_evenly,
                         padding_strategy::adjust_padding_evenly));

  fs::ifstream ifs(test, fs::ifstream::binary);
  vector<unique_ptr<id3v2_tag>> new_tagset;
  read_all_id3v2(ifs, back_inserter(new_tagset));

  BOOST_CHECK(2 == new_tagset.size());

  BOOST_CHECK( "foo" == tags[0]->artist() );
  BOOST_CHECK( "bar" == tags[1]->title() );

  BOOST_CHECK( 1 == count_files(tmp) );

  // We added massive amounts of padding to make up the space:

  // add to the padding yet further:
  tags[0]->padding( 147786 ); // +1
  tags[1]->padding( 147785 ); // +1

  BOOST_CHECK_NO_THROW(
    maybe_emplace_tagset(test, tags.begin(), tags.end(), apply_unsync::as_needed,
                         emplace_strategy::reduce_padding_evenly,
                         padding_strategy::adjust_padding_evenly));


  BOOST_CHECK( 1 == count_files(tmp) );

  tags[0]->padding( 147786 ); // +1
  tags[1]->padding( 147785 ); // +1

  BOOST_CHECK_NO_THROW(
    maybe_emplace_tagset(test, tags.begin(), tags.end(), apply_unsync::as_needed,
                         emplace_strategy::only_with_full_padding,
                         padding_strategy::adjust_padding_evenly));


  BOOST_CHECK( 2 == count_files(tmp) );

  // Corner case-- try with a file that doesn't exist
  fs::path test2 = test.string() + ".not-there";
  BOOST_CHECK_NO_THROW(
    maybe_emplace_tagset(test2, tags.begin(), tags.end(), apply_unsync::as_needed,
                         emplace_strategy::only_with_full_padding,
                         padding_strategy::adjust_padding_evenly));
  BOOST_CHECK( fs::exists(test2) );

  // If we made it this far, clean-up the test directory behind us
  fs::remove_all(tmp);
}

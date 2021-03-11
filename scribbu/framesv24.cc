/**
 * \file framesv24.cc
 *
 * Copyright (C) 2015-2021 Michael Herstine <sp1ff@pobox.com>
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

#include <scribbu/framesv24.hh>

#include <scribbu/charsets.hh>
#include <scribbu/id3v2.hh>


///////////////////////////////////////////////////////////////////////////////
//                            class id3v2_4_frame                            //
///////////////////////////////////////////////////////////////////////////////

namespace scribbu {

  template <>
  /*static*/
  std::string id3v2_4_frame::as_str(
    const unsigned char *pbuf,
    std::size_t cbbuf,
    unsigned char unicode,
    scribbu::encoding dstenc,
    scribbu::on_no_encoding rsp /*=
      scribbu::on_no_encoding::fail*/,
    const boost::optional<scribbu::encoding> &force /*=
      boost::none*/)
  {
    using std::string;
    using scribbu::encoding;
    using scribbu::convert_encoding;

    encoding src = encoding::ISO_8859_1;
    if (force) {
      src = force.get();
    }
    else if (unicode) {

      if (1 == unicode || 2 == unicode ) {
        // Spec says this is UTF-16 with BOM or without BOM, resp., but let's
        // be generous
        if (1 < cbbuf && 0xfe == pbuf[0] && 0xff == pbuf[1]) {
          src =  encoding::UTF_16BE;
        } else if (1 < cbbuf && 0xff == pbuf[0] && 0xfe == pbuf[1]) {
          src =  encoding::UTF_16LE;
        } else {
          src =  encoding::UTF_16;
        }
      }
      else if (3 == unicode) {
        src = encoding::UTF_8;
      }
      else {
        throw std::range_error("illegal encoding");
      }

    }

    return convert_encoding<string>(pbuf, cbbuf, src, dstenc, rsp);
  }

}

/// Serialize this tag's header to an output stream, including any special
/// fields such as decompressed size, group id, &c
/*virtual*/
std::size_t
scribbu::id3v2_4_frame::write_header(std::ostream &os,
                                     std::size_t cb_payload,
                                     std::size_t dlind) const
{
  std::size_t cb = 10, sz = cb_payload;

  char flags[2] = { 0, 0 };
  if (tap()) {
    flags[0] |= 32;
  }
  if (fap()) {
    flags[0] |= 16;
  }
  if (ro()) {
    flags[0] |= 8;
  }
  //  %0h00kmnp
  if (grouped()) {
    flags[1] |= 32;
    sz += 1;
    cb += 1;
  }
  if (compressed_) {
    flags[1] |= 8;
  }
  if (encrypted()) {
    flags[1] |= 4;
    sz += 1;
    cb += 1;
  }
  if (unsynchronised_) {
    flags[1] |= 2;
  }
  if (data_len_indicator_) {
    flags[1] |= 1;
    sz += 4;
    cb += 4;
  }

  char idbuf[4]; id().copy(idbuf);
  os.write(idbuf, 4);

  // ID3v2.3 frame sizes *are* sync-safe:
  unsigned char szbuf[4];
  detail::sync_safe_from_unsigned(size(), szbuf);
  os.write((char*)szbuf, 4);

  os.write(flags, 2);

  if (encrypted()) {
    unsigned char x = encmeth();
    os.write((char*)&x, 1);
  }

  if (grouped()) {
    unsigned char x = gid();
    os.write((char*)&x, 1);
  }

  if (data_len_indicator_) {
    os.write((char*)&dlind, 4);
  }

  return cb;
}


///////////////////////////////////////////////////////////////////////////////
//                        class unknown_id3v2_3_frame                        //
///////////////////////////////////////////////////////////////////////////////

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::unknown_id3v2_4_frame::size() const
{
  return data_.size();
}

/*virtual*/
std::size_t
scribbu::unknown_id3v2_4_frame::serialize(std::ostream &os) const
{
  os.write((char*)&(data_[0]), data_.size());
  return data_.size();
}


///////////////////////////////////////////////////////////////////////////////
//                              class UFID_2_4                               //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::UFID_2_4::create(const frame_id4 &id,
                          const unsigned char *p,
                          std::size_t cb,
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only read_only,
                          const boost::optional<unsigned char> &enc,
                          const boost::optional<unsigned char> &gid,
                          bool compressed,
                          bool unsynchronised,
                          const boost::optional<std::size_t> &dli)
{
  return std::unique_ptr<id3v2_4_frame>(new UFID_2_4(p, p + cb,
                                                     tap,
                                                     fap,
                                                     read_only, enc,
                                                     gid, compressed,
                                                     unsynchronised, dli));
}

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::UFID_2_4::size() const
{
  return unique_file_id::size();
}

/*virtual*/
std::size_t
scribbu::UFID_2_4::serialize(std::ostream &os) const
{
  return unique_file_id::write(os);
}



///////////////////////////////////////////////////////////////////////////////
//                              class ENCR_2_4                               //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::ENCR_2_4::create(const frame_id4 &id,
                          const unsigned char *p,
                          std::size_t cb,
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only read_only,
                          const boost::optional<unsigned char> &enc,
                          const boost::optional<unsigned char> &gid,
                          bool compressed,
                          bool unsynchronised,
                          const boost::optional<std::size_t> &dli)
{
  return std::unique_ptr<id3v2_4_frame>(new ENCR_2_4(p, p + cb,
                                                     tap,
                                                     fap,
                                                     read_only, enc,
                                                     gid, compressed,
                                                     unsynchronised, dli));
}

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::ENCR_2_4::size() const
{
  return encryption_method::size();
}

/*virtual*/
std::size_t
scribbu::ENCR_2_4::serialize(std::ostream &os) const
{
  return encryption_method::write(os);
}


///////////////////////////////////////////////////////////////////////////////
//                         class id3v2_4_text_frame                          //
///////////////////////////////////////////////////////////////////////////////

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::id3v2_4_text_frame::size() const
{
  return 1 + text_.size();
}

/*virtual*/
std::size_t
scribbu::id3v2_4_text_frame::serialize(std::ostream &os) const
{
  os.write((char*)&unicode_, 1);
  os.write((char*)&(text_[0]), text_.size());
  return 1 + text_.size();
}

/*static*/
scribbu::encoding
scribbu::id3v2_4_text_frame::encshim(frame_encoding x)
{
  if (frame_encoding::ISO_8859_1 == x) {
    return scribbu::encoding::ISO_8859_8;
  }
  else if (frame_encoding::UTF_16_BOM == x) {
    return scribbu::encoding::UTF_16;
  }
  else if (frame_encoding::UTF_16_BE == x) {
    return scribbu::encoding::UTF_16BE;
  }
  else {
    return scribbu::encoding::UTF_8;
  }
}

/*static*/
unsigned char
scribbu::id3v2_4_text_frame::encshim2(frame_encoding x)
{
  if (frame_encoding::ISO_8859_1 == x) {
    return 0;
  }
  else if (frame_encoding::UTF_16_BOM == x) {
    return 1;
  }
  else if (frame_encoding::UTF_16_BE == x) {
    return 2;
  }
  else {
    return 3;
  }
}

void
scribbu::id3v2_4_text_frame::set(const std::string &text,
                                 encoding src /*= encoding::UTF_8*/,
                                 frame_encoding dst /*= encoding::UTF_8*/,
                                 bool add_bom /*=false*/,
                                 on_no_encoding rsp /*= on_no_encoding::fail*/)
{
  std::vector<unsigned char> data =
    scribbu::convert_encoding(text, src, encshim(dst), add_bom, rsp);

  unicode_ = encshim2(dst);
  text_.swap(data);
}

/*static*/
std::unique_ptr<scribbu::id3v2_4_text_frame>
scribbu::id3v2_4_text_frame::create(const frame_id4 &id,
                                    const unsigned char *p,
                                    std::size_t cb,
                                    tag_alter_preservation tap,
                                    file_alter_preservation fap,
                                    read_only read_only,
                                    const boost::optional<unsigned char> &enc,
                                    const boost::optional<unsigned char> &gid,
                                    bool compressed,
                                    bool unsynchronised,
                                    const boost::optional<std::size_t> &dli)
{
  return std::unique_ptr<id3v2_4_text_frame>(new id3v2_4_text_frame(id, p, p + cb, tap,
                                                               fap, read_only,
                                                               enc, gid,
                                                               compressed, unsynchronised,
                                                               dli));
}

///////////////////////////////////////////////////////////////////////////////
//                              class TXXX_2_4                               //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::TXXX_2_4::create(const frame_id4 &id,
                          const unsigned char *p,
                          std::size_t cb,
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only read_only,
                          const boost::optional<unsigned char> &enc,
                          const boost::optional<unsigned char> &gid,
                          bool compressed,
                          bool unsynchronised,
                          const boost::optional<std::size_t> &dli)
{
  return std::unique_ptr<id3v2_4_frame>(new TXXX_2_4(p, p + cb, tap, fap,
                                                     read_only, enc, gid,
                                                     compressed,
                                                     unsynchronised, dli));
}

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::TXXX_2_4::size() const
{
  return user_defined_text::size();
}

/*virtual*/
std::size_t
scribbu::TXXX_2_4::serialize(std::ostream &os) const
{
  return user_defined_text::write(os);
}


///////////////////////////////////////////////////////////////////////////////
//                              class COMM_2_4                               //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::COMM_2_4::create(const frame_id4 &id,
                          const unsigned char *p,
                          std::size_t cb,
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only read_only,
                          const boost::optional<unsigned char> &enc,
                          const boost::optional<unsigned char> &gid,
                          bool compressed,
                          bool unsynchronised,
                          const boost::optional<std::size_t> &dli)
{
  return std::unique_ptr<id3v2_4_frame>(new COMM_2_4(p, p + cb, tap, fap,
                                                     read_only, enc,
                                                     gid, compressed,
                                                     unsynchronised, dli));
}

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::COMM_2_4::size() const
{
  return comments::size();
}

/*virtual*/
std::size_t
scribbu::COMM_2_4::serialize(std::ostream &os) const
{
  return comments::write(os);
}


///////////////////////////////////////////////////////////////////////////////
//                              class PCNT_2_4                               //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::PCNT_2_4::create(const frame_id4 &id,
                          const unsigned char *p,
                          std::size_t cb,
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only read_only,
                          const boost::optional<unsigned char> &enc,
                          const boost::optional<unsigned char> &gid,
                          bool compressed,
                          bool unsynchronised,
                          const boost::optional<std::size_t> &dli)
{
  return std::unique_ptr<id3v2_4_frame>(new PCNT_2_4(p, p + cb, tap, fap,
                                                     read_only, enc,
                                                     gid, compressed,
                                                     unsynchronised, dli));
}

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::PCNT_2_4::size() const
{
  return play_count::size();
}

/*virtual*/
std::size_t
scribbu::PCNT_2_4::serialize(std::ostream &os) const
{
  return play_count::write(os);
}


///////////////////////////////////////////////////////////////////////////////
//                              class POPM_2_4                               //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::POPM_2_4::create(const frame_id4 &id,
                          const unsigned char *p,
                          std::size_t cb,
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only read_only,
                          const boost::optional<unsigned char> &enc,
                          const boost::optional<unsigned char> &gid,
                          bool compressed,
                          bool unsynchronised,
                          const boost::optional<std::size_t> &dli)
{
  return std::unique_ptr<id3v2_4_frame>(new POPM_2_4(p, p + cb, tap, fap,
                                                     read_only, enc,
                                                     gid, compressed,
                                                     unsynchronised, dli));
}

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::POPM_2_4::size() const
{
  return popularimeter::size();
}

/*virtual*/
std::size_t
scribbu::POPM_2_4::serialize(std::ostream &os) const
{
  return popularimeter::write(os);
}

///////////////////////////////////////////////////////////////////////////////
//                              class XTAG_2_4                               //
///////////////////////////////////////////////////////////////////////////////

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::XTAG_2_4::create(const frame_id4 &id,
                          const unsigned char *p,
                          std::size_t cb,
                          tag_alter_preservation tap,
                          file_alter_preservation fap,
                          read_only read_only,
                          const boost::optional<unsigned char> &enc,
                          const boost::optional<unsigned char> &gid,
                          bool compressed,
                          bool unsynchronised,
                          const boost::optional<std::size_t> &dli)
{
  return std::unique_ptr<id3v2_4_frame>(new XTAG_2_4(p, p + cb, tap, fap,
                                                     read_only, enc,
                                                     gid, compressed,
                                                     unsynchronised, dli));
}

/// Return the size, in bytes, of the frame, prior to desynchronisation,
/// compression, and/or encryption exclusive of the header
/*virtual*/
std::size_t
scribbu::XTAG_2_4::size() const
{
  return tag_cloud::size();
}

/*virtual*/
std::size_t
scribbu::XTAG_2_4::serialize(std::ostream &os) const
{
  return tag_cloud::write(os);
}

/**
 * \file mp3.cc
 *
 * Copyright (C) 2021 Michael Herstine <sp1ff@pobox.com>
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
 * along with scribbu.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 */

#include <scribbu/mp3.hh>

#include <arpa/inet.h>

namespace {

  // The first index (i.e. index zero) contains the bitrates for MPEG version
  // 1, layer II. The second contains bitrates for MPEG versions 2 & 2.5 (LSF)
  // Layers II & III.
  // This implementation does not support "free" bit rates, so to lookup
  // the correct bitrate once you've dermined MPEG version, subtract one
  // from the integer formed by bits 15-12.
  unsigned int
  BITRATES_BPS[][14] = {
    { 32000,  40000,  48000,  56000,  64000,  80000, 96000, 112000,
     128000, 160000, 192000, 224000, 256000, 320000},
    { 8000, 16000, 24000,  32000,  40000,  48000, 56000,
     64000, 80000, 96000, 112000, 128000, 144000, 160000}
  };

  // The first index corresponds to MPEG versions 1, 2, & 2.5
  unsigned int
  SAMPLE_RATE_HZ[][3] = {
    { 44100, 48000, 32000 },
    { 22050, 24000, 16000 },
    { 11025, 12000,  8000 },
  };

  inline
  bool
  is_xing(char buf[]) {
    return buf[0] == 'X' && buf[1] == 'i' && buf[2] == 'n' && buf[3] == 'g';
  }

  inline
  bool
  is_info(char buf[]) {
    return buf[0] == 'I' && buf[1] == 'n' && buf[2] == 'f' && buf[3] == 'o';
  }

  inline
  bool
  is_vbri(char buf[]) {
    return buf[0] == 'V' && buf[1] == 'B' && buf[2] == 'R' && buf[3] == 'I';
  }

  inline
  bool
  is_lame(char buf[]) {
    return
      (buf[0] == 'L' && buf[1] == 'A' && buf[2] == 'M' && buf[3] == 'E' ) ||
      (buf[0] == 'L' && buf[1] == '3' && buf[2] == '.' && buf[4] == '9');
  }
}

////////////////////////////////////////////////////////////////////////////////
//                             class vbri_mp3_tag                             //
////////////////////////////////////////////////////////////////////////////////

scribbu::vbri_mp3_tag::vbri_mp3_tag(std::istream &is)
{
  uint16_t ns;
  is.read((char*)&ns, 2);
  version_ = ntohs(ns);

  is.read((char*)&ns, 2);
  delay_ = ntohs(ns);

  is.read((char*)&ns, 2);
  quality_ = ntohs(ns);

  uint32_t nl;
  is.read((char*)&nl, 4);
  file_size_bytes_ = ntohl(nl);

  is.read((char*)&nl, 4);
  num_frames_ = ntohl(nl);

  uint16_t num_entries;
  is.read((char*)&num_entries, 4);
  num_entries = ntohl(num_entries);

  is.read((char*)&ns, 2);
  scale_ = ntohs(ns);

  is.read((char*)&ns, 2);
  toc_entry_size_ = ntohs(ns);

  is.read((char*)&ns, 2);
  frames_per_toc_entry_ = ntohs(ns);

  toc_.resize(num_entries * toc_entry_size_);
  is.read((char*)toc_.data(), num_entries * toc_entry_size_);
}

////////////////////////////////////////////////////////////////////////////////
//                            class xing_mp3_frame                            //
////////////////////////////////////////////////////////////////////////////////

scribbu::xing_mp3_tag::xing_mp3_tag(std::istream &is)
{
  std::uint8_t buf[4];
  is.read((char*)buf, 4);

  bool frames_present = 0 != (buf[3] & 0x01);
  bool bytes_present  = 0 != (buf[3] & 0x02);
  bool toc_present    = 0 != (buf[3] & 0x04);
  bool qual_present   = 0 != (buf[3] & 0x08);

  uint32_t nl;
  if (frames_present) {
    is.read((char*)&nl, 4);
    num_frames_ = ntohl(nl);
  }

  if (bytes_present) {
    is.read((char*)&nl, 4);
    file_size_bytes_ = htonl(nl);
  }

  if (toc_present) {
    toc_ = std::array<std::uint8_t, 100>();
    is.read((char*)(*toc_).data(), 100);
  }

  if (qual_present) {
    is.read((char*)&nl, 4);
    quality_ = ntohl(nl);
  }
}

////////////////////////////////////////////////////////////////////////////////
//                             class lame_mp3_tag                             //
////////////////////////////////////////////////////////////////////////////////

scribbu::lame_mp3_tag::lame_mp3_tag(char tag_id[], std::istream &is)
{
  lame_id_[0] = tag_id[0]; lame_id_[1] = tag_id[1];
  lame_id_[2] = tag_id[2]; lame_id_[3] = tag_id[3];

  char rest[5];
  is.read(rest, 5);
  lame_id_[4] = rest[0]; lame_id_[5] = rest[1]; lame_id_[6] = rest[2];
  lame_id_[7] = rest[3]; lame_id_[8] = rest[4];

  char b;
  is.read(&b, 1);
  vbr_method_ = vbr_method_for_bits(b & 0xf);

  is.seekg(10, std::ios_base::cur);

  is.read(&b, 1);
  bitrate_ = b;

  is.seekg(7, std::ios_base::cur);

  uint32_t nl;
  is.read((char*)&nl, 4);
  size_bytes_ = ntohl(nl);
}

/*static*/
scribbu::lame_mp3_tag::vbr_method
scribbu::lame_mp3_tag::vbr_method_for_bits(std::uint8_t x)
{
  vbr_method y = vbr_method::unknown;
  switch (x) {
  case 1:
    y = vbr_method::constant;
    break;
  case 2:
    y = vbr_method::abr;
    break;
  case 3:
    y = vbr_method::vbr_1;
    break;
  case 4:
    y = vbr_method::vbr_2;
    break;
  case 5:
    y = vbr_method::vbr_3;
    break;
  case 6:
    y = vbr_method::vbr_4;
    break;
  case 8:
    y = vbr_method::cbr_two_pass;
    break;
  case 9:
    y = vbr_method::abr_two_pass;
    break;
  }

  return y;
}

////////////////////////////////////////////////////////////////////////////////
//                           class mp3_audio_frame                            //
////////////////////////////////////////////////////////////////////////////////

const char *
scribbu::mp3_audio_frame::error::what() const noexcept
{
  if ( ! pwhat_ ) {
    std::stringstream stm;
    switch (cause_) {
    case bad_mpeg_spec_version:
      stm << "bad MPEG spec version";
      break;
    case bad_layer:
      stm << "bad MPEG audio layer";
      break;
    case no_sync:
      stm << "no sync found";
      break;
    case free_bitrate:
      stm << "free bitrates not supported";
      break;
    case bad_bitrate:
      stm << "unsupported (or unknown) bitrate";
      break;
    case bad_sampling_rate:
      stm << "unsupported (or unknown) sampling rate";
      break;
    default:
      stm << "unknown MP3 audio frame error";
      break;
    }

    pwhat_.reset(new std::string(stm.str()));
  }

  return pwhat_->c_str();
}

scribbu::mp3_audio_frame::mp3_audio_frame(std::uint8_t hdr[4],
                                          std::istream &is,
                                          bool check_vbr /*= false*/):
  crc_(std::nullopt),
  vbri_(std::nullopt)
{
  parse_header(hdr, is);

  // at this point, `is' now points to the beginning of the MPEG audio samples,
  // or the VBR header(s).
  if (check_vbr) {
    parse_vbr(is);
  }

  // Either way, `is' now points just past the side information. Advance it
  // past the audio samples:
  std::istream::off_type off = size_ - 4 - (crc_.has_value() ? 2 : 0) - side_info_size_;
  is.seekg(off, std::ios_base::cur);
}

scribbu::mp3_audio_frame::mp3_audio_frame(std::istream &is, bool check_vbr):
  crc_(std::nullopt),
  vbri_(std::nullopt)
{
  // It is a pre-condition that an MPEG Audio Layer III header be present
  // in the 32 bits at the beginning of `is'...
  std::uint8_t hdr[4] = { 0, 0, 0, 0 };
  is.read((char*)hdr, 4);

  parse_header(hdr, is);

  // at this point, `is' now points to the beginning of the MPEG audio samples,
  // or the VBR header(s).
  if (check_vbr) {
    parse_vbr(is);
  }

  // Either way, `is' now points just past the side information. Advance it
  // past the audio samples:
  std::istream::off_type off = size_ - 4 - (crc_.has_value() ? 2 : 0) - side_info_size_;
  is.seekg(off, std::ios_base::cur);
}

/// Helper function-- just parse the MPEG Audio Header; should only be
/// called during construction
void
scribbu::mp3_audio_frame::parse_header(std::uint8_t hdr[4], std::istream &is)
{
  std::istream::pos_type here = is.tellg();

  try {
    // so the first eleven bits had better be 1!
    if (!is_sync(hdr)) {
      throw mp3_audio_frame::error(mp3_audio_frame::error::no_sync);
    }

    // OK-- next, parse out bits 20-19 (MPEG audio version)
    switch ((hdr[1] >> 3) & 0x03) {
    case 0:
      version_ = mpeg_version::mpeg_audio_version_2_5;
      break;
    case 1:
      throw mp3_audio_frame::error(mp3_audio_frame::error::bad_mpeg_spec_version);
    case 2:
      version_ = mpeg_version::mpeg_audio_version_2;
      break;
    case 3:
      version_ = mpeg_version::mpeg_audio_version_1;
      break;
    }

    // Nex, bits 18-17 represent the layer-- we only handle Layer III
    if (((hdr[1] >> 1) & 0x03) != 1) {
      is.seekg(here, std::ios_base::beg);
      throw mp3_audio_frame::error(mp3_audio_frame::error::bad_layer);
    }

    // protected?
    if ((hdr[1] & 0x01) == 0) {
      std::uint16_t crc;
      is.read((char*)&crc, 2);
      crc_ = ntohs(crc);
    }

    // bitrate
    size_t i = version_ == mpeg_version::mpeg_audio_version_1 ? 0 : 1,
      j = ((hdr[2] >> 4) & 0xff);
    if (j == 0) {
      is.seekg(here, std::ios_base::beg);
      throw mp3_audio_frame::error(mp3_audio_frame::error::free_bitrate);
    } else if (j == 15) {
      is.seekg(here, std::ios_base::beg);
      throw mp3_audio_frame::error(mp3_audio_frame::error::bad_bitrate);
    }

    bitrate_ = BITRATES_BPS[i][j - 1];

    // sampling rate
    switch (version_) {
    case mpeg_version::mpeg_audio_version_1:
      i = 0;
      break;
    case mpeg_version::mpeg_audio_version_2:
      i = 1;
      break;
    case mpeg_version::mpeg_audio_version_2_5:
      i = 2;
      break;
    }

    j = (hdr[2] >> 2) & 0x03;
    if (j == 3) {
      is.seekg(here, std::ios_base::beg);
      throw mp3_audio_frame::error(mp3_audio_frame::error::bad_sampling_rate);
    }

    sample_rate_ = SAMPLE_RATE_HZ[i][j];

    padded_ = (hdr[2] & 0x02) != 0;
    private_ = (hdr[2] & 0x01) != 0;

    switch ((hdr[3] >> 6) & 0x03) {
    case 0:
      channel_mode_ = channel_mode::stereo;
      break;
    case 1:
      channel_mode_ = channel_mode::joint_stereo;
      break;
    case 2:
      channel_mode_ = channel_mode::dual_channel;
      break;
    case 3:
      channel_mode_ = channel_mode::single_channel;
      break;
    }

    // version_ & channel_mode_ determine the amount of side info present
    if (version_ == mpeg_version::mpeg_audio_version_1) {
      if (channel_mode_ == channel_mode::single_channel) {
        side_info_size_ = 17;
      } else {
        side_info_size_ = 32;
      }
    } else {
      if (channel_mode_ == channel_mode::single_channel) {
        side_info_size_ = 9;
      } else {
        side_info_size_ = 17;
      }
    }
    // advance `is' past the side info
    is.seekg(side_info_size_, std::ios_base::cur);

    // skipping mode extension ATM

    copyright_ = (hdr[3] & 0x08) != 0;
    original_ = (hdr[3] & 0x04) != 0;

    // skipping emphasis ATM

    unsigned int samples_per_frame =
      version_ == mpeg_version::mpeg_audio_version_1 ? 1152 : 576;

    size_ = samples_per_frame * bitrate_ / 8 / sample_rate_;
    if (padded_) {
      size_ += 1;
    }
  }
  catch (const std::exception&) {
    if (!is.eof()) {
      is.seekg(here, std::ios_base::beg);
    }
    throw;
  }
}

/// Helper function for checking for VBR frames & parsing 'em if found;
/// should only be called during construction
void
scribbu::mp3_audio_frame::parse_vbr(std::istream &is)
{
  // At this point, there may be either an Xing or VBRI header.

  // The Xing header AFAIK always immediately follows the side information. The
  // VBRI header, however, is always found thirty-two bytes after the header
  // (i.e. at offset 36) per \ref scribbu_mp3_ref_10 "[10]". Reading the
  // BeagleBuddy source, however, it seems the author believes that it, too,
  // immediately follows the side information. In the test data I have, it
  // always appears at bye 36, so that's where this implementation looks. If I
  // can find an example where it appears elsewhere, I'll adapt this code.

  // Note, however, that the BeagleBuddy source contains the intriguing comment:

  //   "for some reason, when the MPEG frame header has the "protected by
  //   CRC", the 2 byte CRC that normally follows the MPEG frame header is
  //   sometimes not present. perhaps this is because the "protected by CRC"
  //   is true when the "protected by CRC" bit is 0, and some encoders are
  //   incorrectly setting it to 0. whatever the reason, we shift the index by
  //   two bytes to account for the CRC that was read in as part of the MPEG
  //   frame header and see if an Xing header can be found."

  // curiously, this workaround is only done when checking for the Xing header
  // & not the VBRI header. Perhaps this is because the author believes that
  // the Fraunhofer encoder is the only encoder that uses the VBRI frame, and
  // the Fraunhofer encoder is not afflicted by this particular bug
  // (indicating the presence of a CRC but not including it).

  // So: this implementation does the following:

  //   1. check `is' for "Xing" or "Info" (i.e. the Xing tag); if found, parse
  //      the tag, then check for a LAME tag

  //   2. failing that, seek backward by two bytes

  //   3. check `is' for "Xing" or "Info" (i.e. the Xing tag); if found, parse
  //      the tag, then check for a LAME tag

  //   4. failing that, skip forward by enough bytes to bring us to offset 36
  //      within this frame

  //   5. check for "VBRI" (i.e. the Fraunhofer tag); if found,
  //      parse the tag

  std::istream::pos_type here = is.tellg();

  char buf[4] = { 0, 0, 0, 0 };
  is.read(buf, 4);
  if (is_xing(buf) || is_info(buf)) {
    auto tag = xing_mp3_tag(is);
    is.read(buf, 4);
    if (is_lame(buf)) {
      auto lame = lame_t{ lame_mp3_tag(buf, is) };
      vbri_ = std::optional<vbri_t>{ vbri_t(std::make_pair(tag, lame)) };
    } else {
      vbri_ = std::optional<vbri_t>{ vbri_t(std::make_pair(tag, lame_t() )) };
    }
  } else {
    is.seekg(-6, std::ios_base::cur);
    is.read(buf, 4);
    if (is_xing(buf) || is_info(buf)) {
      auto tag = xing_mp3_tag(is);
      is.read(buf, 4);
      if (is_lame(buf)) {
        auto lame = lame_t{ lame_mp3_tag(buf, is) };
        vbri_ = std::optional<vbri_t>{ vbri_t(std::make_pair(tag, lame)) };
      } else {
        vbri_ = std::optional<vbri_t>{ vbri_t(std::make_pair(tag, lame_t())) };
      }
    } else {
      // At this point, the get ptr is two bytes from the end of the side info
      is.seekg(32 - side_info_size_ - 2, std::ios_base::cur);
      is.read(buf, 4);
      if (is_vbri(buf)) {
        vbri_ = std::optional<vbri_t>{ vbri_t(vbri_mp3_tag(is)) };
      }
    }
  }

  is.seekg(here, std::ios_base::beg);
}

std::pair<std::size_t, std::size_t>
scribbu::mp3_audio_frame::duration() const
{
  using namespace std;

  return make_pair(mpeg_version::mpeg_audio_version_1 == version_ ? 1152 : 576,
                   sample_rate_);
}

std::optional<std::pair<std::size_t, std::size_t>>
scribbu::mp3_audio_frame::vbri() const
{
  if (vbri_) {
    if (std::holds_alternative<vbri_mp3_tag>(*vbri_)) {
      std::size_t num_frames, num_bytes;
      vbri_mp3_tag tag = std::get<vbri_mp3_tag>(*vbri_);
      num_frames = tag.num_frames();
      num_bytes = tag.file_size_bytes();
      return std::make_pair(num_frames, num_bytes);
    } else {
      xing_mp3_tag tag = std::get<xing_t>(*vbri_).first;
      auto a = tag.num_frames();
      auto b = tag.file_size_bytes();
      if (a && b) {
        return std::make_pair(*a, *b);
      } else {
        return {};
      }
    }
  } else {
    return {};
  }
}

double
scribbu::get_mp3_duration(std::istream &is)
{
  const std::ios_base::iostate EXC_MASK = std::ios_base::badbit;

  // Copy off the stream's exception mask, in case the caller is
  // counting on it...
  std::ios_base::iostate exc_mask = is.exceptions();
  // and set it to a value convenient for our use.
  is.exceptions(EXC_MASK);

  std::istream::pos_type here = is.tellg();

  std::uint8_t hdr[4];
  is.read((char*)hdr, 4);
  if (!is || !is_sync(hdr)) {
    is.clear();
    is.seekg(here, std::ios_base::beg);
    return 0.0;
  }

  std::size_t num, den;
  mp3_audio_frame f(hdr, is, true);
  std::tie(num, den) = f.duration();

  double dur;

  auto vbri = f.vbri();
  if (vbri) {
    std::size_t num_frames, num_bytes;
    std::tie(num_frames, num_bytes) = *vbri;

    dur = double(num) * double(num_frames) / double(den);
    is.seekg(num_bytes - f.size(), std::ios_base::cur);
  } else {
    size_t num_frames = 1; // one frame so far

    for ( ; ; ++num_frames) {
      is.read((char*)hdr, 4);
      if (!is || !is_sync(hdr)) {
        is.clear();
        break;
      }
      // construct a "throwaway" frame just to skip the get ptr past a legit MP3
      // frame
      mp3_audio_frame h_(hdr, is);
    }
    dur = double(num) * double(num_frames) / double(den);
  }

  is.exceptions(exc_mask);
  return dur;
}

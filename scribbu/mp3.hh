/**
 * \file mp3.hh
 *
 * Copyright (C) 2021-2022 Michael Herstine <sp1ff@pobox.com>
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

#ifndef MP3_HH_INCLUDED
#define MP3_HH_INCLUDED 1
/**
 * \page scribbu_mp3 MPEG Layer III Support
 *
 * \section scribbu_mp3_intro Introduction
 *
 * This module contains scribbu's (minimal) support for MPEG Layer III. I have a
 * post about this <a
 * href="https://www.unwoundstack.com/blog/mp3-duration.html">here</a> that
 * duplicates some of what is here; this section is more detailed as befits
 * developer documentation. That post is also strictly concerned with the
 * calculation of duration whereas these docs are more comprehensive.
 *
 * \section scribbu_mp3_overview Overview
 *
 * The MPEG specification went through three reviesions. MPEG 1 (ISO/IEC
 * 13818-3) and MPEG 2 (ISO/IEC 11172-3) are ISO standards. MPEG 2.5 is an
 * unofficial extension of MPEG 2 to support lower sampling rates. MPEG 2/2.5 is
 * also known by the abbreviation LSF, which stands for Lower Sampling
 * Frequencies.
 *
 * MPEG audio data (a file or otherwise) is made up of frames.  There are three
 * "layers" to the spec; scribbu is concerned only with layer III, but in Layers
 * I & II the frames are completely independent of one another (meaning that you
 * could cut the file at any frame boundry & have the resulting parts play
 * correctly).
 *
 * Layer III can make use of a technique known as the "bit resevoir" wherein
 * unused space at the end of a frame can be used to hold data for subsequent
 * frames, meaning that decode a given frame may required previous frames. In
 * the worst case, a decoder may need to read nine frames before being able to
 * decode one (\ref scribbu_mp3_ref_03 "[3]").
 *
 * In all layers & all versions of the spec, each frame begins with a 32 bit
 * header, followed by, perhaps, a 16-bit CRC in big-endian format. Next comes a
 * block of information required by decoders to interpret the audio samples &
 * finally the audio samples themselves.
 *
 * The audio data contains a fixed number of samples across all frames (the
 * number is determined by the layer & version of the MPEG spec). The audio
 * data, in turn, is arranged into slots.  Layer I slots are four bytes in size,
 * while Layers II & III have slots that are one byte in size.
 *
 * Schematically, MPEG audio data looks like this:
 *
 \code

  +-------------------------+ -
  | MPEG audio frame header |  \
  +-------------------------+   |
  | side information        |   | MPEG audio frame 0
  +-------------------------+   |
  | audio samples           |  /
  +-------------------------+ -
  | MPEG audio frame header |  \
  +-------------------------+   |
  | side information        |   | MPEG audio frame 1
  +-------------------------+   |
  | audio samples           |  /
  +-------------------------+ -
  | MPEG audio frame header |  \
  +-------------------------+   |
  | side information        |   | MPEG audio frame 2
  +-------------------------+   |
  | audio samples           |  /
  +-------------------------+ -
              ....                ...

 \endcode
 *
 * \section scribbu_mp3_frame_header The MPEG Audio Frame Header
 *
 * In all layers & all versions of the spec, each frame begins with a 32 bit
 * header, followed by, perhaps, a 16-bit CRC in big-endian format (this
 * implementation considers the CRC to be part of the header). If the 32 bits
 * are denoted:
 *
 \code
     AAAAAAAA AAABBCCD EEEEFFGH IIJJKLMM
 \endcode
 *
 * their significance is broken out below:
 *
 \code
 letter length position description
 ------ ------ -------- -------------
 A      11     31-21    frame sync: all bits set
 B      2      20-19    MPEG audio version ID
                        00 - MPEG version 2.5 (an unofficial extension of MPEG 2)
                        01 - reserved
                        10 - MPEG version 2(ISO/IEC 13818-3)
                        11 - MPEG version 1 (ISO/IEC 11172-3)

                        NB. In versions 1 & 2, the first twelve bits are used
                        for frame sync (see below)

 C      2      18-17    layer description
                        00 - reserved
                        01 - Layer III
                        10 - Layer II
                        11 - Layer I
 D      1      16       protection bit
                        0 - protected by CRC (i.e. a 16-bit CRC follows this header)
                        1 - not protected
 E      4      15-12    bitrate index

                        +------+-------+-------+-------+-------+-------------+
                        | bits | V1,L1 | V1,L2 | V1,L3 | V2,L1 | V2, L2 & L3 |
                        +------+-------+-------+-------+-------+-------------+
                        | 0000 | free  | free  | free  | free  | free        |
                        | 0001 | 32    | 32    | 32    | 32    | 8           |
                        | 0010 | 64    | 48    | 40    | 48    | 16          |
                        | 0011 | 96    | 56    | 48    | 56    | 24          |
                        | 0100 | 128   | 64    | 56    | 64    | 32          |
                        | 0101 | 160   | 80    | 64    | 80    | 40          |
                        | 0110 | 192   | 96    | 80    | 96    | 48          |
                        | 0111 | 224   | 112   | 96    | 112   | 56          |
                        | 1000 | 256   | 128   | 112   | 128   | 64          |
                        | 1001 | 288   | 160   | 128   | 144   | 80          |
                        | 1010 | 320   | 192   | 160   | 160   | 96          |
                        | 1011 | 352   | 224   | 192   | 176   | 112         |
                        | 1100 | 384   | 256   | 224   | 192   | 128         |
                        | 1101 | 416   | 320   | 256   | 224   | 144         |
                        | 1110 | 448   | 384   | 320   | 256   | 160         |
                        | 1111 | bad   | bad   | bad   | bad   | bad         |
                        +------+-------+-------+-------+-------+-------------+

                        All values are in kbps (kilo, as in x1000, *not* x1024)
                        V1 - MPEG Version 1
                        V2 - MPEG Version 2 and Version 2.5
                        L1 - Layer I
                        L2 - Layer II
                        L3 - Layer III

                        "free" indicates that the file is encoded with a
                        constant bitrate, just not one of the predefined
                        values of bitrate. Very few decoders handle this.

  F     2      11-10    sampling rate frequency index (values are in Hz)
                        +------+-------+-------+---------+
                        | bits | MPEG1 | MPEG2 | MPEG2.5 |
                        +------+-------+-------+---------+
                        | 00   | 44100 | 22050 |  11025  |
                        | 01   | 48000 | 24000 |  12000  |
                        | 10   | 32000 | 16000 |  8000   |
                        | 11   | reserv| reserv| reserv  |
                        +------+-------+-------+---------+

  G     1      9        padding bit
                        0 - frame is not padded
                        1 - frame is padded with one extra slot

                        Padding is used to fit the bit rates exactly. For an
                        example: 128k 44.1kHz layer II uses a lot of 418 bytes
                        and some of 417 bytes long frames to get the exact 128k
                        bitrate.

  H     1      8        private bit-- application-specific, informational only

  I     2      7-6      channel mode

                        00 - Stereo
                        01 - Joint stereo (Stereo)
                        10 - Dual channel (Stereo)
                        11 - Single channel (Mono)

                        Dual channel files are made of two independent mono
                        channels. Each one uses exactly half the bitrate of the
                        file. Most decoders output them as stereo, but it might
                        not always be the case.

  J     2      5-4      Mode extension (only used with joint stereo)

                        Mode extension is used to join information that are of
                        no use for stereo effect, thus reducing needed
                        resources. These bits are dynamically determined by an
                        encoder in Joint stereo mode.

                        The frequency range is divided into 32 sub-bands. For
                        Layer I & II these two bits determine the frequency
                        ranges (bands) where intensity stereo is applied. For
                        Layer III these two bits determine which type of joint
                        stereo is used (intensity stereo or m/s
                        stereo). Frequency range is determined within
                        decompression algorithm.

                        +-------+----------------+-------------------------------+
                        | Value | Layers I & II  |        Layer III              |
                        |       |                | M/S stereo | Intensity stereo |
                        +-------+----------------+-------------------------------+
                        | 00    | bands 4 to 31  |    off     |     off          |
                        | 01    | bands 8 to 31  |    off     |     on           |
                        | 10    | bands 12 to 31 |    on      |     off          |
                        | 11    | bands 16 to 31 |    on      |     on           |
                        +-------+----------------+-------------------------------+

  K     1      3        copyright (informational)
                        0 - not copyrighted
                        1 - copyrighted
  L     1      2        original (informational)
                        0 - copy of original media
                        1 - original media
  M     2      1-0      emphasis

                        00 - none
                        01 - 50/15 ms
                        10 - reserved
                        11 - CCIT J.17

                        The emphasis indication is here to tell the decoder that
                        the file must be de-emphasized, that means the decoder
                        must 're-equalize' the sound after a Dolby-like noise
                        suppression. It is rarely used.

 \endcode
 *
 * With respect to B (version of the spec) \ref scribbu_mp3_ref_02 "[2]" notes
 * "MPEG Version 2.5 is not official standard. Bit No 20 in frame header is used
 * to indicate version 2.5. Applications that do not support this MPEG version
 * expect this bit always to be set, meaning that frame sync (A) is twelve bits
 * long, not eleve as stated here. Accordingly, B is one bit long (represents
 * only bit No 19). I recommend using methodology presented here, since this
 * allows you to distinguish all three versions and keep full compatibility."
 *
 * \subsection scribbu_mp3_frame_header_crc The CRC
 *
 * Per \ref scribbu_mp3_ref_07 "[7]", "This field will only exist if the
 * protection bit in the header is set and makes it possible check the most
 * sensitive data for transmission errors. Sensitive data is defined by the
 * standard to be bit 16 to 31 in both the header and the side information. If
 * these values are incorrect they will corrupt the whole frame whereas an error
 * in the main data only distorts a part of the frame. A corrupted frame can
 * either be muted or replaced by the previous frame"
 *
 * I have not yet implemented CRC checks, but this statement seems suspicious to
 * me: bit 16 in the side information, AFAIK, falls in the middle of a field. I
 * seem to recall reading somewhere else that the CRC is calculated on the
 * second half of the header & \em all of the side information.
 *
 * Also: "The CRC is calculated by applying the CRC-16 algorithm (with the
 * generator polynom 0x8005)" \ref scribbu_mp3_ref_01 "[1]".
 *
 * \section scribbu_mp3_frame_side_data Side Data
 *
 * scribbu does not explicitly model the side data at this time, but it
 * "...contains all the relevant information to decode the main data.  For
 * example it contains the main data begin pointer, scale factor selection
 * information, Huffman table information for both the granules etc."
 * \ref scribbu_mp3_ref_05 "[5]"
 *
 * The size of Layer III side information varies based on spec version & channel
 * mode:
 *
 \code
  |       | MPEG 1 | MPEG 2/2.5 (LSF) |
  +-------+--------+------------------+
  | Mono  |  17    |         9        |
  | other |  32    |        17        |
 \endcode
 *
 * \section scribbu_mp3_frame_header_vbri Variable Bitrate Information
 *
 * Of particular interest to this implementation is detrmining the track
 * time-length, or duration.  Files can be encoded at a constant bitrate (CBR)
 * wherein each frame uses the same bitrate, or at a variable bitrate (VBR) in
 * which the bitrate varies by frame. The former makes calculating the duration
 * simple (we can just divide the total file size by the bitrate), whereas the
 * second generally produces better sound quality at the cost of having to
 * examine each frame in order to determine the track duration.
 *
 * One \em could simply take the bitrate for the first frame & divide that into
 * the total file size (assuming, in effect, CBR). This approach is mentioned in
 * \ref scribbu_mp3_ref_1 "[1]", although the author notes that "the lowest
 * bitrate available is used for silence in music titles (especially at the
 * beginning)." MPD uses it as a first approximation for track length. In
 * practice, however, this can produce an estimate that is more than double the
 * correct number.
 *
 * The only reliable means I've come across of calculating duration, absent some
 * kind of additional information is found in the BeagleBuddy library (\ref
 * scribbu_mp3_ref_04 "[4]"): walk all the frames & compute the avarge bitrate
 * for the entire file (i.e. the summed bitrates from all frames divided by the
 * number of frames) and divide that into the file size.
 *
 * Xing and Fraunhofer separately developed a header meant to be included in the
 * first frame to help ameliorate the VBR situation; both (may) include the
 * total number of frames in the track, along with a lookup table intended to
 * allow easy seeking within the file by percentage of duration (i.e. "to go to
 * the 75% mark of this track, jump to frame N located at byte X").
 *
 * LAME added their own custom tag after the Xing frame, and finally an
 * encoder \em could add these frames to a CBR-encoded file (both the Xing &
 * LAME frames provide for explicitly marking the file as CBR).
 *
 * In all three cases, the first frame will contain no audio data (the side info
 * is all zero) so decoders that don't understand these tags will simply skip
 * the frame.
 *
 * Knowlege of the total number of frames enables readily calculating the
 * duration under the reasonable assumption that the sampling frequency doesn't
 * change: since the number of samples is the same for every frame in a valid
 * mp3 encoding, all frames will then have the same duration. One may simply
 * compute the duration of the first frame (either through the sampling rate or
 * via bitrate & frame-size) and multiply it by the number of frames.
 *
 * MPD, for instance, after the initial guess above, checks for an Xing
 * header, and if found, instead computes the duration by muliplying
 * the number of frames by the first frame's duration (computed, through
 * libmad, by the sample rate).
 *
 * See \ref scribbu_mp3_ref_07 "[7]" for an interesting discussion on sampling
 * rates; changing the sampling rate is apparently never done, but it has to be
 * advertised in each frame to enable decoders to work with streaming data (i.e.
 * a decoder can just start reading frames at any point, "lock on" to the next
 * frame using the frame sync, and start decoding).
 *
 * \ref scribbu_mp3_ref_09 "[9]" also recommends just reading each frame header,
 * computing duration from sampling rate & summing.
 *
 * \subsection scribbu_mp3_frame_header_vbri_vbri VBRI
 *
 * Perhaps because Fraunhofer has ended support for the MP3 standard, and seems
 * to have removed all documentation from their site, this frame is largely
 * undocumented. According \ref scribbu_mp3_ref_10 "[10]" it was only used by
 * the Fraunhofer encoder and is always located thirty-two bytes after the end
 * of the first frame header. Reading the BeagleBudy source, the author states
 * that it immediately follows the side information (which I find more
 * credible).
 *
 \code
  | Position | Length      | Meaning                      | Format                   |
  +----------+-------------+------------------------------+--------------------------+
  | 0        | 4           | header ID "VBRI"             | 4 ASCII chars            |
  | 4        | 2           | tag version ID               | 16-bit big-endian uint   |
  | 6        | 2           | delay*                       |                          |
  | 8        | 2           | quality indicator            |                          |
  | 10       | 4           | # of bytes in file           | 32-bit big-endian uint   |
  | 14       | 4           | # of frames in file          | 32-bit big-endian uint   |
  | 18       | 2           | # of entries in TOC          | 16-bit big-endian uint   |
  | 20       | 2           | scale factor for TOC entries |                          |
  | 22       | 2           | TOC entry size (max: 4)      | 16-bit big-endian uint   |
  | 24       | 2           | frames per table entry       | 16-bit big-endian uint   |
  | 26       | # entries * | TOC                          | each entry is big-endian |
  |          | entry size  |                              |                          |

  * in [10] this is described as a "big-endian float", which makes no sense
    as it is only 16-bits wide; BeagleBuddy interprets it as a ushort.
 \endcode
 *
 * taglib barely supports it-- it just parses out the size & number of
 * frames. The sample code in \ref scribbu_mp3_ref_01 "[1]" seems to treat the
 * entries as bytes per-n-frames, which experientially seems to be correct: the
 * value of TOC[i] is the number of bytes taken up by the i-th n-tuple of
 * frames, where n is the frames per table entry. I would guess that the scale
 * factor applies to the number of bytes, but I have yet to encounter a scale
 * factor not equal to one.
 *
 *
 * \subsection scribbu_mp3_frame_header_vbri_xing Xing
 *
 * I couldn't find documentation on the Xing frame as such, but since the LAME
 * encoder source is still available, as is the Xing SDK, I can at least
 * reverse-engineer it:
 *
 \code
  | Position     | Length | Meaning                        | Format                 |
  +----------    +--------+--------------------------------+------------------------+
  | 0            | 4      | header ID "Xing" or "Info" (*) | 4 ASCII chars          |
  | 4            | 4      | flags (+)                      | 32-bit big-endian uint |
  | 8            | 4      | (optional) # of frames         | 32-bit big-endian uint |
  | 8 or 12      | 4      | (optional) # of bytes in file  | 32-bit big-endian uint |
  | 8, 12, or 16 | 100    | TOC                            | 100 bytes              |
  | 8, 12, 16,   | 4      | quality: from 0 (best) to      | 32-bit big-endian uint |
  | 108, 112, or |        | 100 (worst)                    |                        |
  | 116          |        |                                |                        |

  (*) "Xing" indicates a VBR-encoded file, "Info" a CBR-encoded

  (+) flags:

    0x00000001 - # frames field is present
    0x00000002 - # bytes field is present
    0x00000004 - TOC is present
    0x00000008 - quality field is present

 \endcode
 *
 * The table of contents (TOC) gives seek points for random access into the
 * file. It always has 100 entries & the i-th entry gives the seek point for the
 * i-percent point of the track in terms of duration. Given the table value, the
 * offset in bytes is given by:
 *
 \code
  toc[i]
  ------ * total track size in bytes
   256
 \endcode
 *
 * Given the poor resolution of this method, I assume that seeking to an offset
 * given by the above formula will, in general, drop you into the middle of
 * a frame & you'll then have to seek to the next frame sync.
 *
 * This seems to me so un-ergonomic that I haven't modelled it in class
 * xing_mp3_header & will not until I have to.
 *
 *
 * \subsection scribbu_mp3_frame_header_vbri_lame LAME
 *
 * The LAME tag extends the Xing tag to add yet more information. Of particular
 * interest here: if the encoding used ABR, the average bitrate will be listed
 * here.
 *
 \code
  position  length  description
  --------  ------  -----------
  0         9       LAME encoder version

                    see [11] for details
                    "LAME" + major + "." + minor + flag until 3.100, then
                    "LAME" + major + minor + flag
                    flag is one of "a", "b", "r" or " " for alpha, beta,
                    release when the patch is > 0 or all other cases, resp.

  9         1       tag version & VBR method

                    the four MSB are the tag version:
                        0: rev. 0
                        1: rev. 1
                        2-15: reserved

                    the four LSB are the LAME VBR method:

                      - 0: unknown
                      - 1: constant bitrate
                      - 2: restricted VBR targeting a given average bitrate (ABR)
                      - 3: full VBR method 1
                      - 4: full VBR method 2
                      - 5: full VBR method 3
                      - 6: full VBR method 4
                      - 8: constant bitrate two-pass
                      - 9: ABR two-pass

  10        1       lowpass filter vlaue; multiply by 100 to get Hz
  11        8       replay gain; not modelled at this time
  19        1       encoding flags & ATH time; not modelled at this time

  20        1       bitrate

                        ABR: target bitrate
                        CBR: bitrate
                        else: minimum bitrate

                    0 means "unknown"; values > 255 are capped to 255

  21        3       encoder delays; not modelled at this time
  24        1       misc. info; not modelled at this time
  25        1       mp3 gain; not modelled at this time
  26        2       presets & surround information; not modelled at this time

  28        4       length in bytes

                        measured from the first byte of this tag

  32        2       CRC of the music data; not modelled at this time
  24        2       CRC of this tag up until this field; not modelled at this time

 \endcode
 *
 *
 * \section scribbu_mp3_references References
 *
 * 1. \anchor scribbu_mp3_ref_01 [1] Windszus, Konrad, MPEG Audio Frame Header
 *    https://www.codeproject.com/Articles/8295/MPEG-Audio-Frame-Header,
 *    (retrieved April 5, 2021)
 *
 * 2. \anchor scribbu_mp3_ref_02 [2] Supurovic, Predrag, MPEG AUDIO FRAME HEADER
 *    http://www.mpgedit.org/mpgedit/mpeg_format/mpeghdr.htm, (retrieved April
 *    5, 2021)
 *
 * 3. \anchor scribbu_mp3_ref_03 [3] Bouvigne, Gabriel, MPEG Audio Layer
 *    I/II/III frame header http://www.mp3-tech.org/programmer/frame_header.html
 *    (retrieved April 6, 2021)
 *
 * 4. \anchor scribbu_mp3_ref_04 [4] Wennerstrom, Joneric, BeagleBuddy
 *    http://www.beaglebuddy.com/frame/content.html
 *    (retrieved April 6, 2021)
 *
 * 5. \anchor scribbu_mp3_ref_05 [5] Sripada, Praveen, MP3 DECODER in Theory and
 *    Practice, Masters Thesis,
 *    https://www.diva-portal.org/smash/get/diva2:830195/FULLTEXT01.pdf
 *    (retrieved April 6, 2021)
 *
 * 6. \anchor scribbu_mp3_ref_06 [6] unknown, VBR header and LAME tag
 *     https://wiki.hydrogenaud.io/index.php?title=LAME#VBR_header_and_LAME_tag,
 *     (retrieved April 7, 2021).
 *
 * 7. \anchor scribbu_mp3_ref_07 [7] Raissi, Rassol, The Theory Behind Mp3
 *    http://www.mp3-tech.org/programmer/docs/mp3_theory.pdf (retrieved April 8,
 *    2021).
 *
 * 8. \anchor scribbu_mp3_ref_08 [8] unknown, "What sample rates can an MP3 file
 *    have"
 *    https://sound.stackexchange.com/questions/39568/what-sample-rates-can-an-mp3-file-have
 *    (retrieved April 8, 2021)
 *
 * 9. \anchor scribbu_mp3_ref_09 [9] unknown, "How can I get the duration of an
 *    mp3 file"
 *    https://stackoverflow.com/questions/3505575/how-can-i-get-the-duration-of-an-mp3-file-cbr-or-vbr-with-a-very-small-library
 *    (retrieved April 8, 2021).
 *
 * 10. \anchor scribbu_mp3_ref_10 [10] Li, Yaqi, Cao, Yizhen, "Identification of
 *     Different Patterns of MP3 and Duration Calculation", International Conference
 *     on Computer Science and Service System (CSSS 2014), pp653-656
 *
 * 11. \anchor scribbu_mp3_ref_11 [11] multiple, LAME version string
 *     https://wiki.hydrogenaud.io/index.php?title=LAME_version_string, (retrieved
 *     April 9, 2021).
 *
 *
 */

// Nb. No idea why, but if I don't include <array> here, this build fails in GHA
// on MacOS(?!)
#include <array>
#include <scribbu/errors.hh>

#include <cstdint>
#include <iostream>
#include <optional>
#include <variant>
#include <vector>

namespace scribbu {

  /**
   * \class vbri_mp3_tag
   *
   * \brief Fraunhofer VBRI tag
   *
   *
   * This frame's format is fully documented
   * \ref scribbu_mp3_frame_header_vbri_vbri "here".
   * Construct instances of this tag with an istream whose get pointer is just
   * past the "VBRI" tag identifier. This allows callers to determine for
   * themselves what tag (if any) is present & construct an instance of the
   * appropriate class: I prefer this to the "try to construct an Xing tag; if
   * that fails, try constructing a VBRI tag" which in effect substitutes
   * exception handling for a simple if statement.
   *
   *
   */

  class vbri_mp3_tag
  {
  public:
    /// Construct with the get ptr advanced past the tag ID "VBRI"
    vbri_mp3_tag(std::istream &is);
    std::size_t file_size_bytes() const
    { return file_size_bytes_; }
    std::size_t num_frames() const
    { return num_frames_; }
    std::size_t frames_per_toc_entry() const
    {return frames_per_toc_entry_; }
    /// Return the offset, in bytes, of frame N where N is
    /// \a i * frames_per_toc_entry()
    std::size_t offset_for_frame_multiple(std::size_t i) const;

  private:
    std::uint16_t version_;
    std::uint16_t delay_;
    std::uint16_t quality_;
    std::uint32_t file_size_bytes_;
    std::uint32_t num_frames_;
    std::uint16_t scale_;
    std::uint16_t toc_entry_size_;
    std::uint16_t frames_per_toc_entry_;
    std::vector<uint8_t> toc_;
  };

  /**
   * \class xing_mp3_tag
   *
   * \brief Xing VBR tag
   *
   *
   * This frame's format is fully documented
   * \ref scribbu_mp3_frame_header_vbri_xing "here"
   * Construct instances of this tag with an istream whose get pointer is just
   * past the "Xing" or "Info" tag identifiers. This allows callers to determine
   * for themselves what tag (if any) is present & construct an instance of the
   * appropriate class: I prefer this to the "try to construct an Xing tag; if
   * that fails, try constructing a VBRI tag" which in effect substitutes
   * exception handling for a simple if statement.
   *
   *
   */

  class xing_mp3_tag
  {
  public:
    /// Construct with the get ptr advanced past the tag ID
    xing_mp3_tag(std::istream &is);
    std::optional<std::size_t> file_size_bytes() const
    { return file_size_bytes_; }
    std::optional<std::size_t> num_frames() const
    { return num_frames_; }
    bool has_toc() const
    { return toc_.has_value(); }

  private:
    std::optional<std::uint32_t> num_frames_;
    std::optional<std::uint32_t> file_size_bytes_;
    std::optional<std::array<std::uint8_t, 100>> toc_;
    std::optional<std::uint32_t> quality_;

  };

  /**
   * \class lame_mp3_tag
   *
   * This frame's format is fully documented
   * \ref scribbu_mp3_frame_header_vbri_lame  "here"
   * Construct instances of this tag with an istream whose get pointer is just
   * past the "LAME"" tag identifier.. This allows callers to determine for
   * themselves whether the tag is present & construct an instance of this
   * class: I prefer this to the "try to construct a LAME tag & handle the
   * exception if it fails which in effect substitutes exception handling for a
   * simple if statement.
   *
   *
   */

  class lame_mp3_tag
  {
  public:
    enum class vbr_method {
      unknown,
      constant,
      abr,
      vbr_1,
      vbr_2,
      vbr_3,
      vbr_4,
      cbr_two_pass,
      abr_two_pass,
    };

  public:
    /// Construct with the get ptr advanced past the tag ID
    lame_mp3_tag(char tag_id[], std::istream &is);
    vbr_method get_vbr_method() const
    { return vbr_method_; }
    std::uint8_t get_bitrate() const
    { return bitrate_; }
    std::uint32_t get_size_bytes() const
    { return size_bytes_; }

  private:
    static vbr_method vbr_method_for_bits(std::uint8_t x);

  private:
    std::array<char, 9> lame_id_;
    vbr_method vbr_method_;
    std::uint8_t bitrate_;
    std::uint32_t size_bytes_;
  };

  inline
  bool
  is_sync(std::uint8_t hdr[]) {
    return (0xff == hdr[0] && 0xe0 <= hdr[1]);
  }

  /**
   * \class mp3_audio_frame
   *
   * \brief One frame in an MPEG Layer III audio stream
   *
   *
   */

  class mp3_audio_frame
  {
  public:
    class error: public scribbu::error
    {
    public:
      enum cause {
        bad_mpeg_spec_version,
        bad_layer,
        no_sync,
        free_bitrate,
        bad_bitrate,
        bad_sampling_rate,
      };

    public:
      error(cause c): cause_(c)
      { }
      virtual const char * what() const noexcept(true);

    private:
      cause cause_;
      mutable std::shared_ptr<std::string> pwhat_;
    };

    enum class mpeg_version {
      mpeg_audio_version_1,
      mpeg_audio_version_2,
      mpeg_audio_version_2_5
    };

    enum class channel_mode {
      stereo, joint_stereo, dual_channel, single_channel
    };

  public:
    /// Construct having already read the 32-bit frame header; \a hdr shall
    /// contain that while \a is shall point to the remainder of an MPEG Layer
    /// III audio frame; this is a convenience for callers that want to check
    /// for frame sync, or a valid header themselves
    mp3_audio_frame(std::uint8_t hdr[4], std::istream &is, bool check_vbr = false);
    /// \a is shall point to an MPEG Layer III audio frame
    mp3_audio_frame(std::istream &is, bool check_vbr = false);

  public:
    unsigned int bps() const
    { return bitrate_; }
    unsigned int sample_rate_hz() const
    { return sample_rate_; }
    std::size_t size() const
    { return size_; }
    mpeg_version version() const
    { return version_; }
    /// This frame's duration, in seconds, as a fraction (numerator, denominator)
    std::pair<std::size_t, std::size_t>
    duration() const;
    /// If VBRI is available, return num_frames, num_bytes
    std::optional<std::pair<std::size_t, std::size_t>>
    vbri() const;

  private:
    /// Helper function-- just parse the MPEG Audio Header; should only be
    /// called during construction
    void parse_header(std::uint8_t hder[4], std::istream &is);
    /// Helper function for checking for VBR frames & parsing 'em if found;
    /// should only be called during construction
    void parse_vbr(std::istream &is);

  private:
    /// MPEG specification version
    mpeg_version version_;
    /// CRC checksum
    std::optional<std::uint16_t> crc_;
    /// bitrate, in bits per second
    unsigned int bitrate_;
    /// sampling rate, in hertz
    unsigned int sample_rate_;
    bool padded_;
    bool private_;
    channel_mode channel_mode_;
    bool copyright_;
    bool original_;
    /// number of bytes for side info in this frame
    std::size_t side_info_size_;
    /// this frame's size, in bytes, including header & CRC (if present)
    std::size_t size_;
    /// variable bit-rate information; there may be none, so vbri_ is an
    /// optional. If there *is* vbri, it may either be the VBRI or the Xing
    /// tags, hence the value is a variant. If it's an Xing tag, there may be a
    /// LAME tag after, so the Xing tag is paired with another optional (of
    /// lame_mp3_tag).
    typedef std::optional<lame_mp3_tag> lame_t;
    typedef std::pair<xing_mp3_tag, lame_t> xing_t;
    typedef std::variant<vbri_mp3_tag, xing_t> vbri_t;
    std::optional<vbri_t> vbri_;

  };

  /**
   * \brief Compute the duration of an mp3 file, in seconds
   *
   *
   * \param is [in,out] an input stream; on entry the get pointer shall
   * reference the beginning of the first frame in the dataset; on exit, it will
   * point just past the end
   *
   *
   * In the presence of VBRI, this method will compute the duration by
   * multiplying the duration of the first frame by the number of frames. In its
   * absence, it will walk each frame, summing their durations.
   *
   *
   */

  double
  get_mp3_duration(std::istream &is);

} // End namespace scribbu.

#endif // not MP3_HH_INCLUDED

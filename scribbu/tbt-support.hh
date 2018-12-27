/**
 * \file tbt-support.hh
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

#ifndef TBT_SUPPORT_HH_INCLUDED
#define TBT_SUPPORT_HH_INCLUDED 1

#include <scribbu/scribbu.hh>
#include <scribbu/errors.hh>
#include <scribbu/id3v1.hh>
#include <scribbu/id3v2.hh>

#include <boost/any.hpp>
#include <boost/optional.hpp>

namespace scribbu
{
  namespace tbt_support
  {
    /// Root exception for all template evaluation errors
    class error: public scribbu::error
    { };

    enum class year_format {
      two_digits, four_digits
    };

    /// Preference for tag source when evaluating templates based on ID3 tags
    enum class all_source_preference {
      prefer_id3v2, // Prefer ID3v2, fall back to ID3v1
      prefer_id3v1, // Prefer ID3v1, fall back to ID3v2
      id3v2_only,   // Accept ID3v2 only
      id3v1_only    // Accept ID3v1 only
    };

    std::ostream& operator<<(std::ostream &os, all_source_preference pref);

    /// Thrown when a tag-based template can't be evaluated
    class missing_source_text: public error
    {
    public:
      missing_source_text(all_source_preference source):
        source_(source)
      { }
      all_source_preference get_source() const
      { return source_; }
      virtual const char * what() const noexcept;

    private:
      all_source_preference source_;
      mutable std::shared_ptr<std::string> pwhat_;

    };

    typedef boost::error_info<struct tag_name, std::string> tag_name_info;

    class invalid_template: public error
    {
    public:
      virtual const char * what() const noexcept
      { return "invalid template"; }
    };

    enum class v1_encoding {
      automatic, iso8859_1, ascii, cp1252, utf_8,
        utf_16_be, utf_16_le, utf_32,
    };
    enum class the_xform {
      nil, make_prefix, make_suffix
    };
    enum class capitalization {
      nil, capitalize, all_upper, all_lower
    };
    enum class output_encoding {
      utf_8, ascii, cp1252, iso8859_1
    };
    enum class base { decimal, hex };
    enum class hex_case { lower, upper };

    enum class size_opt { base, hex_case };

    std::ostream& operator<<(std::ostream &os, size_opt opt);

    enum class ws_xform { compress, replace };

    std::ostream& operator<<(std::ostream &os, ws_xform opt);

    enum class md5_opt { base, hex_case };

    std::ostream& operator<<(std::ostream &os, md5_opt opt);

    enum class file_opt { the_xform, cap_xform, ws_xform };

    std::ostream& operator<<(std::ostream &os, file_opt opt);

    enum class aacet_opt {
      all_source_preference, v1_encoding, the_xform,
      cap_xform, ws_xforms, output_encoding
    };

    std::ostream& operator<<(std::ostream &os, aacet_opt opt);

    class duplicate_option: public error
    {
    public:
      duplicate_option(aacet_opt opt);
      duplicate_option(file_opt opt);
      duplicate_option(md5_opt opt);
      duplicate_option(ws_xform opt);
      duplicate_option(size_opt opt);

    public:
      virtual const char * what() const noexcept
      { return pwhat_->c_str(); }

    private:
      std::shared_ptr<std::string> pwhat_;

    };

    std::string do_the_xform(const std::string &text, the_xform the);
    std::string do_cap_xform(const std::string &text, capitalization cap);
    std::string do_ws_xform(const std::string &text, bool compress, const std::string &replace);
    std::string encode(const std::string &text, output_encoding out);

    /// Interface for evaluating terms in a text-based template
    struct term
    {
      virtual std::string evaluate(const file_info  &fi,
                                   const id3v2_tag  *pid3v2,
                                   const track_data &ti,
                                   const id3v1_tag  *pid3v1) const = 0;
      virtual ~term()
      { }
    };

    class process_and_concatenate:
      public std::binary_function<std::string, std::shared_ptr<scribbu::tbt_support::term>, std::string>
    {
    public:
      process_and_concatenate(const scribbu::file_info  &fi,
                              const scribbu::id3v2_tag  *pid3v2,
                              const scribbu::track_data &ti,
                              const scribbu::id3v1_tag  *pid3v1):
        fi_(fi), pid3v2_(pid3v2), ti_(ti), pid3v1_(pid3v1)
      { }
      std::string
      operator()(const std::string &text, const std::shared_ptr<term> &pterm) const
      {
        return text + pterm->evaluate(fi_, pid3v2_, ti_, pid3v1_);
      }

    private:
      scribbu::file_info fi_;
      const scribbu::id3v2_tag *pid3v2_;
      scribbu::track_data ti_;
      const scribbu::id3v1_tag *pid3v1_;

    };

    class tag_based_term: public term
    {
    protected:
      template <typename forward_input_iterator>
      std::string source_text(forward_input_iterator              p0,
                              forward_input_iterator              p1,
                              const boost::optional<std::string> &v2,
                              all_source_preference               pref,
                              v1_encoding                         v1enc) const
      {
        if (pref == all_source_preference::prefer_id3v2) {
          if (v2 && !v2->empty()) {
            return *v2;
          } else if (p0 != p1) {
            std::string s = v1_text_to_utf8(p0, p1, v1enc);
            if (!s.empty()) {
              return s;
            }
          }
          throw missing_source_text(pref);
        } else if (pref == all_source_preference::id3v2_only) {
          if (v2 && !v2->empty()) {
            return *v2;
          }
          throw missing_source_text(pref);
        } else if (pref == all_source_preference::prefer_id3v1) {
          if (p0 != p1) {
            std::string s = v1_text_to_utf8(p0, p1, v1enc);
            if (!s.empty()) {
              return s;
            }
          } else if (v2 && !v2->empty()) {
            return *v2;
          }
          throw missing_source_text(pref);
        } else {
          if (p0 != p1) {
            std::string s = v1_text_to_utf8(p0, p1, v1enc);
            if (!s.empty()) {
              return s;
            }
          }
          throw missing_source_text(pref);
        }
      }

    private:
      template <typename forward_input_iterator>
      std::string v1_text_to_utf8(forward_input_iterator p0,
                                  forward_input_iterator p1,
                                  v1_encoding            v1enc) const
      {
        if (p0 == p1) {
          return std::string();
        }

        std::size_t cb = std::distance(p0, p1);
        std::unique_ptr<unsigned char[]> p(new unsigned char[cb]);
        std::copy(p0, p1, p.get());
        return v1_text_to_utf8(p.get(), cb, v1enc);
      }
      std::string v1_text_to_utf8(const unsigned char *pbuf,
                                  std::size_t          cbbuf,
                                  v1_encoding          v1enc) const;

    };

    class aacet_term: public tag_based_term
    {
    public:
      aacet_term(all_source_preference sp  = all_source_preference::prefer_id3v2,
                 v1_encoding           v1  = v1_encoding::automatic,
                 the_xform             the = the_xform::make_suffix,
                 capitalization        cap = capitalization::capitalize,
                 bool                  cmp = false,
                 std::string           rep = std::string(),
                 output_encoding       out = output_encoding::utf_8):
        sp_         (sp),
        v1_         (v1),
        the_        (the),
        cap_        (cap),
        compress_ws_(cmp),
        replace_ws_ (rep),
        out_        (out)
      { }
      template <typename forward_input_iterator>
      void set_options(forward_input_iterator p0,
                       forward_input_iterator p1)
      {
        using boost::any_cast;

        for ( ; p0 != p1; ++p0) {
          switch (p0->first) {
          case scribbu::tbt_support::aacet_opt::all_source_preference:
            sp_ = any_cast<all_source_preference>(p0->second);
            break;
          case scribbu::tbt_support::aacet_opt::v1_encoding:
            v1_ = any_cast<v1_encoding>(p0->second);
            break;
          case scribbu::tbt_support::aacet_opt::the_xform:
            the_ = any_cast<the_xform>(p0->second);
            break;
          case scribbu::tbt_support::aacet_opt::cap_xform:
            cap_ = any_cast<capitalization>(p0->second);
            break;
          case scribbu::tbt_support::aacet_opt::ws_xforms:
            {
              std::map<ws_xform, boost::any> *p =
                any_cast<std::map<ws_xform, boost::any>*>(p0->second);
              for (auto pr: *p) {
                if (ws_xform::compress == pr.first) {
                  compress_ws_ = any_cast<bool>(pr.second);
                }
                else if (ws_xform::replace == pr.first) {
                  replace_ws_ = any_cast<std::string>(pr.second);
                }
                else {
                  throw std::logic_error("aacet_term::set_options");
                }
              }
            }
            break;
          case scribbu::tbt_support::aacet_opt::output_encoding:
            out_ = boost::any_cast<output_encoding>(p0->second);
            break;
          default:
            throw std::logic_error("aacet_term::set_options");
          }
        }
      }

    protected:
      template <typename forward_input_iterator>
      std::string source_text(forward_input_iterator              p0,
                              forward_input_iterator              p1,
                              const boost::optional<std::string> &v1) const {
        return tag_based_term::source_text(p0, p1, v1, sp_, v1_);
      }
      std::string xform_and_encode(const std::string &text) const;

    private:
      all_source_preference sp_;
      v1_encoding           v1_;
      the_xform             the_;
      capitalization        cap_;
      bool                  compress_ws_;
      std::string           replace_ws_;
      output_encoding       out_;

    };

    class file_term: public term
    {
    public:
      file_term(the_xform          the = the_xform::nil,
                capitalization     cap = capitalization::nil,
                bool               cmp = false,
                const std::string &rep = std::string()):
        the_        (the),
        cap_        (cap),
        compress_ws_(false),
        replace_ws_ (rep)
      { }

    public:
      template <typename forward_input_iterator>
      void set_options(forward_input_iterator p0,
                       forward_input_iterator p1)
      {
        using boost::any_cast;

        for ( ; p0 != p1; ++p0) {
          switch (p0->first) {
          case scribbu::tbt_support::file_opt::the_xform:
            the_ = any_cast<the_xform>(p0->second);
            break;
          case scribbu::tbt_support::file_opt::cap_xform:
            cap_ = any_cast<capitalization>(p0->second);
            break;
          case scribbu::tbt_support::file_opt::ws_xform:
            {
              std::map<ws_xform, boost::any> *p =
                any_cast<std::map<ws_xform, boost::any>*>(p0->second);
              for (auto pr: *p) {
                if (ws_xform::compress == pr.first) {
                  compress_ws_ = any_cast<bool>(pr.second);
                }
                else if (ws_xform::replace == pr.first) {
                  replace_ws_ = any_cast<std::string>(pr.second);
                }
                else {
                  throw std::logic_error("aacet_term::set_options");
                }
              }
            }
            break;
          default:
            throw std::logic_error("aacet_term::set_options");
          }
        }
      }

    protected:
      std::string transform(const boost::filesystem::path &text) const;


    private:
        the_xform      the_;
        capitalization cap_;
        bool           compress_ws_;
        std::string    replace_ws_;
    };

    /// Term implementation that just returns text
    class text_term: public term
    {
    public:
      text_term(const std::string &text): text_(text)
      { }
      virtual std::string evaluate(const file_info  & /*fi    */,
                                   const id3v2_tag  * /*pid3v2*/,
                                   const track_data & /*ti    */,
                                   const id3v1_tag  * /*pid3v1*/) const
      { return text_; }

    private:
      std::string text_;
    };

    /// term implementation that attempts to evaulate a sub-clause (on failure,
    /// it will return an empty string)
    class subclause: public term
    {
    public:
      /// Take ownership of the pointers in the vector, but not the vector
      /// itself
      subclause(const std::vector<scribbu::tbt_support::term*> *pterms);
      virtual std::string evaluate(const file_info  &fi,
                                   const id3v2_tag  *pid3v2,
                                   const track_data &ti,
                                   const id3v1_tag  *pid3v1) const;
    private:
      std::vector<std::shared_ptr<scribbu::tbt_support::term>> terms_;
    };

    class album: public aacet_term
    {
    public:
      album(all_source_preference sp  = all_source_preference::prefer_id3v2,
             v1_encoding           v1  = v1_encoding::automatic,
             the_xform             the = the_xform::make_suffix,
             capitalization        cap = capitalization::capitalize,
             bool                  cmp = false,
             std::string           rep = std::string(),
             output_encoding       out = output_encoding::utf_8):
        aacet_term(sp, v1, the, cap, cmp, rep, out)
      { }
      virtual std::string evaluate(const file_info  & /*fi    */,
                                   const id3v2_tag  * pid3v2,
                                   const track_data & /*ti    */,
                                   const id3v1_tag  * pid3v1) const;

    };

    /// "artist" term
    class artist: public aacet_term
    {
    public:
      artist(all_source_preference sp  = all_source_preference::prefer_id3v2,
             v1_encoding           v1  = v1_encoding::automatic,
             the_xform             the = the_xform::make_suffix,
             capitalization        cap = capitalization::capitalize,
             bool                  cmp = false,
             std::string           rep = std::string(),
             output_encoding       out = output_encoding::utf_8):
        aacet_term(sp, v1, the, cap, cmp, rep, out)
      { }
      virtual std::string evaluate(const file_info  & /*fi    */,
                                   const id3v2_tag  * pid3v2,
                                   const track_data & /*ti    */,
                                   const id3v1_tag  * pid3v1) const;

    };

    class content_type: public aacet_term
    {
    public:
      content_type(all_source_preference sp  = all_source_preference::prefer_id3v2,
                   v1_encoding           v1  = v1_encoding::automatic,
                   the_xform             the = the_xform::make_suffix,
                   capitalization        cap = capitalization::capitalize,
                   bool                  cmp = false,
                   std::string           rep = std::string(),
                   output_encoding       out = output_encoding::utf_8):
        aacet_term(sp, v1, the, cap, cmp, rep, out)
      { }
      virtual std::string evaluate(const file_info  & /*fi    */,
                                   const id3v2_tag  * pid3v2,
                                   const track_data & /*ti    */,
                                   const id3v1_tag  * pid3v1) const;

    };

    class title: public aacet_term
    {
    public:
      title(all_source_preference sp  = all_source_preference::prefer_id3v2,
            v1_encoding           v1  = v1_encoding::automatic,
            the_xform             the = the_xform::make_suffix,
            capitalization        cap = capitalization::capitalize,
            bool                  cmp = false,
            std::string           rep = std::string(),
            output_encoding       out = output_encoding::utf_8):
        aacet_term(sp, v1, the, cap, cmp, rep, out)
      { }
      virtual std::string evaluate(const file_info  & /*fi    */,
                                   const id3v2_tag  * pid3v2,
                                   const track_data & /*ti    */,
                                   const id3v1_tag  * pid3v1) const;

    };

    class encoded_by: public tag_based_term
    {
    public:
      encoded_by(the_xform             the = the_xform::make_suffix,
                 capitalization        cap = capitalization::capitalize,
                 bool                  cmp = false,
                 std::string           rep = std::string(),
                 output_encoding       out = output_encoding::utf_8):
        the_(the),
        cap_(cap),
        cmp_(cmp),
        rep_(rep),
        out_(out)
      { }
      virtual std::string evaluate(const file_info  & /*fi    */,
                                   const id3v2_tag  * pid3v2,
                                   const track_data & /*ti    */,
                                   const id3v1_tag  * pid3v1) const;

    private:
      the_xform       the_;
      capitalization  cap_;
      bool            cmp_;
      std::string     rep_;
      output_encoding out_;

    };

    class year: public tag_based_term
    {
    public:
      year(year_format fmt = year_format::four_digits):
        fmt_(fmt)
      { }
      virtual std::string evaluate(const file_info  & fi,
                                   const id3v2_tag  * /*pid3v2*/,
                                   const track_data & /*ti    */,
                                   const id3v1_tag  * /*pid3v1*/) const;

    private:
      year_format fmt_;

    };

    class basename: public file_term
    {
    public:
      basename(the_xform          the = the_xform::nil,
               capitalization     cap = capitalization::nil,
               bool               cmp = false,
               const std::string &rep = std::string()):
        file_term(the, cap, cmp, rep)
      { }
      virtual std::string evaluate(const file_info  & fi,
                                   const id3v2_tag  * /*pid3v2*/,
                                   const track_data & /*ti    */,
                                   const id3v1_tag  * /*pid3v1*/) const;

    };

    class extension: public file_term
    {
    public:
      extension(the_xform          the = the_xform::nil,
                capitalization     cap = capitalization::nil,
                bool               cmp = false,
                const std::string &rep = std::string()):
        file_term(the, cap, cmp, rep)
      { }
      virtual std::string evaluate(const file_info  & fi,
                                   const id3v2_tag  * /*pid3v2*/,
                                   const track_data & /*ti    */,
                                   const id3v1_tag  * /*pid3v1*/) const;

    };

    class size: public term
    {
    public:
      size(base b = base::decimal, hex_case c = hex_case::lower):
        b_(b), c_(c)
      { }
      template <typename forward_input_iterator>
      size(forward_input_iterator p0, forward_input_iterator p1)
      {
        using boost::any_cast;

        for ( ; p0 != p1; ++p0) {
          if (size_opt::base == p0->first) {
            b_ = any_cast<base>(p0->second);
          }
          else if (size_opt::hex_case == p0->first) {
            c_ = any_cast<hex_case>(p0->second);
          }
          else {
            throw std::logic_error("size::size");
          }
        }
      }
      virtual std::string evaluate(const file_info  & fi,
                                   const id3v2_tag  * /*pid3v2*/,
                                   const track_data & /*ti    */,
                                   const id3v1_tag  * /*pid3v1*/) const;

    private:
      base b_;
      hex_case c_;
    };

    class md5: public term
    {
    public:
      md5(base b = base::decimal, hex_case c = hex_case::lower):
        b_(b), c_(c)
      { }
      template <typename forward_input_iterator>
      md5(forward_input_iterator p0, forward_input_iterator p1)
      {
        using boost::any_cast;

        for ( ; p0 != p1; ++p0) {
          if (md5_opt::base == p0->first) {
            b_ = any_cast<base>(p0->second);
          }
          else if (md5_opt::hex_case == p0->first) {
            c_ = any_cast<hex_case>(p0->second);
          }
          else {
            throw std::logic_error("size::size");
          }
        }
      }
      virtual std::string evaluate(const file_info  & /*fi    */,
                                   const id3v2_tag  * /*pid3v2*/,
                                   const track_data & ti,
                                   const id3v1_tag  * /*pid3v1*/) const;

    private:
      base b_;
      hex_case c_;
    };


  }

}

#endif // not TBT_SUPPORT_HH_INCLUDED

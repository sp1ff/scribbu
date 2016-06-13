#include "id3v2-utils.hh"
#include "id3v22.hh"
#include "id3v23.hh"
#include <scribbu/id3v24.hh>
#include <scribbu/tbt-parser.hh>
#include <scribbu/tbt-lexer.hh>

#include <numeric>


///////////////////////////////////////////////////////////////////////////////
//                 free functions exported from this module                  //
///////////////////////////////////////////////////////////////////////////////

std::unique_ptr<scribbu::id3v2_tag>
scribbu::maybe_read_id3v2(std::istream &is)
{
  id3v2_info H = looking_at_id3v2(is, false);
  if (!H.present_) {
    return std::unique_ptr<scribbu::id3v2_tag>();
  }

  if (2 == H.version_) {
    return std::unique_ptr<scribbu::id3v2_tag>(new id3v2_2_tag(is, H));
  } else if (3 == H.version_) {
    return std::unique_ptr<scribbu::id3v2_tag>(new id3v2_3_tag(is, H));
  } else if (4 == H.version_) {
    return std::unique_ptr<scribbu::id3v2_tag>(new id3v2_4_tag(is, H));
  } else {
    throw id3v2_tag::unknown_version(H.version_);
  }

}


///////////////////////////////////////////////////////////////////////////////
//                         class template_processor                          //
///////////////////////////////////////////////////////////////////////////////

/// Construct with the template in textual form
scribbu::template_processor::template_processor(const std::string &templat)
{
  YY_BUFFER_STATE buf = tbt_scan_string(templat.c_str());
  tbt_switch_to_buffer(buf); // switch flex to the buffer we just created

  // Un-comment to enable debugging:
  // tbtset_debug(1);

  std::vector<scribbu::tbt_support::term*> *pterms;
  int status = tbtparse(&pterms);

  if (0 != status) {
    // TODO: Enable better error reporting in the paraser
    throw scribbu::tbt_support::invalid_template();
  }

  for (auto p: *pterms) {
    terms_.push_back(std::shared_ptr<scribbu::tbt_support::term>(p));
  }

  delete pterms;

}

/// Given the path of the file, process our template
std::string scribbu::template_processor::operator()(const boost::filesystem::path &pth) const
{
  using scribbu::tbt_support::process_and_concatenate;

  scribbu::file_info info;
  std::unique_ptr<std::istream> pis;
  std::tie(pis, info) = scribbu::open_file(pth);

  std::istream &is = *pis.get();

  // and use the open istream to read the...
  std::unique_ptr<scribbu::id3v2_tag> pid3v2 = scribbu::maybe_read_id3v2(is); // ID3v2 tags...
  scribbu::track_data ti(is);                                                 // the track itself...
  std::unique_ptr<scribbu::id3v1_tag> pid3v1 = scribbu::process_id3v1(is);    // and the ID3v1 tag.

  // With this information, walk our terms, evaluating each & concatenating:
  return std::accumulate(terms_.begin(), terms_.end(), std::string(),
                         process_and_concatenate(info, pid3v2.get(), ti, pid3v1.get()));
}

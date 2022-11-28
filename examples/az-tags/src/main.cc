#include "config.h"

#include <getopt.h>
#include <stdlib.h>

#include <iostream>

#include <filesystem>
#include <fstream>

#include <scribbu/scribbu.hh>
#include <scribbu/id3v1.hh>
#include <scribbu/id3v2-utils.hh>
#include <scribbu/id3v2.hh>
#include <scribbu/id3v22.hh>
#include <scribbu/id3v23.hh>
#include <scribbu/id3v24.hh>
#include <scribbu/tagset.hh>

namespace fs = std::filesystem;

template <class tag_type>
struct tag_traits
{
};

template <>
struct tag_traits<scribbu::id3v2_2_tag> 
{
  typedef typename scribbu::frame_id3 frame_id_type;
  typedef typename scribbu::COM       comm_type;
  static comm_type replace(const comm_type &F) 
  {
    std::string txt = F.text<std::string>();
    return comm_type(scribbu::language::eng, txt, scribbu::encoding::UTF_8, 
                     scribbu::use_unicode::no, "amazon.com song id");
  }
  static const scribbu::frame_id3 COMMID; // ("COM");
};

const scribbu::frame_id3 tag_traits<scribbu::id3v2_2_tag>::COMMID("COM");

template <>
struct tag_traits<scribbu::id3v2_3_tag> 
{
  typedef typename scribbu::frame_id4 frame_id_type;
  typedef typename scribbu::COMM      comm_type;
  static comm_type replace(const comm_type &F) 
  {
    std::string txt = F.text<std::string>();
    return comm_type(scribbu::language::eng, txt, scribbu::encoding::UTF_8, 
                     scribbu::use_unicode::no, F.tag_alter_preserve(),
                     F.file_alter_preserve(), F.readonly(), 
                     boost::none, boost::none,
                     std::string("amazon.com song id"));
  }
  static const scribbu::frame_id4 COMMID; // ("COMM");
};

const scribbu::frame_id4 tag_traits<scribbu::id3v2_3_tag>::COMMID("COMM");

template <>
struct tag_traits<scribbu::id3v2_4_tag> 
{
  typedef typename scribbu::frame_id4 frame_id_type;
  typedef typename scribbu::COMM_2_4  comm_type;
  static comm_type replace(const comm_type &F) 
  {
    std::string txt = F.text<std::string>();
    return comm_type(scribbu::language::eng, txt, scribbu::encoding::UTF_8, 
                     scribbu::use_unicode::no, F.tag_alter_preserve(),
                     F.file_alter_preserve(), F.readonly(), 
                     boost::none, boost::none, false,
                     false, boost::none,
                     std::string("amazon.com song id"));
  }
  static const scribbu::frame_id4 COMMID; // ("COMM");
};

const scribbu::frame_id4 tag_traits<scribbu::id3v2_4_tag>::COMMID("COMM");

template <class tag_type>
void
process_tag(tag_type &T)
{
  using namespace std;
  using namespace scribbu;
  
  typedef tag_traits<tag_type> traits_type;
  
  typedef typename traits_type::comm_type comm_type;
  
  for (auto fp: T) {
    if (traits_type::COMMID == fp->id()) {

      id3v2_frame &F = fp;
      comm_type &C = dynamic_cast<comm_type&>(F);

      string dsc = C.template description<string>();
      if (dsc.empty()) {
        string txt = C.template text<string>();
        if ("Amazon.com Song ID" == txt.substr(0, 18)) {
          cout << "updating the comment frame containing " << txt << endl;
          fp = traits_type::replace(C);
        }
      }
    }
  }
}

int
main(int argc, char *argv[])
{
  struct option long_options[] = {
    {"version",   no_argument,       0, 'V'},
    {"help",      no_argument,       0, 'h'},
    {0, 0, 0, 0}
  };
  
  using namespace std;

  while (true) {
    
    int option_index = 0;
    int c = getopt_long(argc, argv, "Vvh",long_options, &option_index);

    if (-1 == c) { // end of options
      break;
    }
    
    switch (c) {
    case 'V':
      cout << PACKAGE_NAME << " " << PACKAGE_VERSION << endl;
      return 0;
    case 'h':
      cout << PACKAGE_NAME << " [OPTION...] [ARGUMENT...]" << endl;
      return 0;
    case '?':
      /* getopt_long already printed an error message. */
      return 2;
    default:
      cerr << "got unexpected char " << c << "  from getopt_long" << endl;
      abort();
    }

  }
  
  scribbu::static_initialize();

  for (int i = optind; i < argc; ++i) {

    std::ifstream ifs = scribbu::open_ifstream(argv[i], ios_base::binary);

    vector<unique_ptr<scribbu::id3v2_tag>> id3v2;
    scribbu::read_all_id3v2(ifs, back_inserter(id3v2));
    scribbu::track_data td((istream&)ifs);
    unique_ptr<scribbu::id3v1_tag> pid3v1 = scribbu::process_id3v1(ifs);
    
    ifs.close();
    
    cout << argv[i] << " has " << id3v2.size() << " ID3v2 tags, and " <<
      (pid3v1 ? "an ID3v1 tag" : "no ID3v1 tags") << endl;
    
    for (auto &ptag: id3v2) {
      switch (ptag->version()) {
      case 2: { // ID3v2.2 tag
        scribbu::id3v2_2_tag &p = dynamic_cast<scribbu::id3v2_2_tag&>(*ptag);
        process_tag(p);
        break;
      }
      case 3: { // ID3v2.3 tag
        scribbu::id3v2_3_tag &p = dynamic_cast<scribbu::id3v2_3_tag&>(*ptag);
        process_tag(p);
        break;
      }
      case 4: { // ID3v2.4 tag
        scribbu::id3v2_4_tag &p = dynamic_cast<scribbu::id3v2_4_tag&>(*ptag);
        process_tag(p);
        break;
      }
    default:
      cerr << "Unknown ID3v2 revision " << ptag->version() << endl;
      abort();
      }
    }
  
    cout << "all tags processed; emplacing new tagset..." << endl;
    scribbu::maybe_emplace_tagset(argv[i], id3v2.begin(), id3v2.end(),
                                  scribbu::apply_unsync::never,
                                  scribbu::emplace_strategy::reduce_padding_evenly,
                                  scribbu::padding_strategy::adjust_padding_evenly);
    cout << "emplacing new tagset...done." << endl;
  
    if (pid3v1) {
      string txt = pid3v1->template comment<string>();
      if ("Amazon.com Song ID" == txt.substr(0, 18)) {
        cout << "clearing ID3v1 comment" << endl;
        pid3v1->set_comment(string(""));
        scribbu::replace_id3v1(argv[i], *pid3v1);
      }
    }
  
  }
  
  scribbu::static_cleanup();
  
  return 0;
}

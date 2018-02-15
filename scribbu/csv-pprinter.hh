#ifndef CSV_PPRINTER_INCLUDED_HH
#define CSV_PPRINTER_INCLUDED_HH 1

#include <scribbu/pprinter.hh>

namespace scribbu {

  class csv_pprinter: public pprinter {

  public:

    static const std::size_t DEFAULT_NCOMMENTS = 4;
    static const boost::optional<encoding> DEFAULT_V1ENC;
    static const boost::optional<encoding> DEFAULT_V2ENC;

  public:
    csv_pprinter(std::size_t                      ncomm = DEFAULT_NCOMMENTS,
                 const boost::optional<encoding> &v1enc = DEFAULT_V1ENC,
                 const boost::optional<encoding> &v2enc = DEFAULT_V2ENC):
      ncomm_(ncomm), v1enc_(v1enc), v2enc_(v2enc)
    { }

  public:
    virtual std::ostream&
    pprint_v2_2_tag(const id3v2_2_tag&, std::ostream&);
    virtual std::ostream&
    pprint_v2_3_tag(const id3v2_3_tag&, std::ostream&);
    virtual std::ostream&
    pprint_v2_4_tag(const id3v2_4_tag&, std::ostream&);
    virtual std::ostream&
    pprint_track_data(const track_data&, std::ostream&);
    virtual std::ostream&
    pprint_v1_tag(const id3v1_tag&, std::ostream&);
    virtual std::ostream&
    pprint_unk_id3v2_2_frame(const unknown_id3v2_2_frame &, std::ostream&);
    virtual std::ostream&
    pprint_id3v2_2_text_frame(const id3v2_2_text_frame&, std::ostream&);
    virtual std::ostream&
    pprint_UFI(const UFI&, std::ostream&);
    virtual std::ostream&
    pprint_TXX(const TXX&, std::ostream&);
    virtual std::ostream&
    pprint_COM(const COM&, std::ostream&);
    virtual std::ostream&
    pprint_CNT(const CNT&, std::ostream&);
    virtual std::ostream&
    pprint_POP(const POP&, std::ostream&);
    virtual std::ostream&
    pprint_unk_id3v2_3_frame(const unknown_id3v2_3_frame&, std::ostream&);
    virtual std::ostream&
    pprint_id3v2_3_text_frame(const id3v2_3_text_frame&, std::ostream&);
    virtual std::ostream&
    pprint_UFID(const UFID&, std::ostream&);
    virtual std::ostream&
    pprint_ENCR(const ENCR&, std::ostream&);
    virtual std::ostream&
    pprint_TXXX(const TXXX&, std::ostream&);
    virtual std::ostream&
    pprint_COMM(const COMM&, std::ostream&);
    virtual std::ostream&
    pprint_PCNT(const PCNT&, std::ostream&);
    virtual std::ostream&
    pprint_POPM(const POPM&, std::ostream&);
    virtual std::ostream&
    pprint_unk_id3v2_4_frame(const unknown_id3v2_4_frame&, std::ostream&);
    virtual std::ostream&
    pprint_id3v2_4_text_frame(const id3v2_4_text_frame&, std::ostream&);
    virtual std::ostream&
    pprint_UFID_2_4(const UFID_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_ENCR_2_4(const ENCR_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_TXXX_2_4(const TXXX_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_COMM_2_4(const COMM_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_PCNT_2_4(const PCNT_2_4&, std::ostream&);
    virtual std::ostream&
    pprint_POPM_2_4(const POPM_2_4&, std::ostream&);

    virtual ~csv_pprinter()
    { }
    virtual pprinter* clone() {
      return new csv_pprinter(*this);
    }

  private:

    /**
     * \brief Escape a string for CSV output
     *
     *
     * \param s [in] the text to be escaped
     *
     * \param sep [in] the delimiter to be used
     *
     * \return \a s if \a sep does not appear in \a s, otherwise, "s" with all
     * occurences of '"' in \a s doubled
     *
     *
     * Take some text & escape it for output in CSV format. It is assumed that
     * the input & output text are UTF-8 encoded.
     *
     *
     */

    static std::string escape(const std::string &s, char sep = ',');


  private:
    std::size_t ncomm_;
    boost::optional<encoding> v1enc_;
    boost::optional<encoding> v2enc_;

  };

  class print_as_csv: public pprint_manipulator {
  public:
    print_as_csv(std::size_t ncomm = csv_pprinter::DEFAULT_NCOMMENTS,
                 const boost::optional<encoding> &v1enc =
                   csv_pprinter::DEFAULT_V1ENC,
                 const boost::optional<encoding> &v2enc =
                   csv_pprinter::DEFAULT_V2ENC):
      pprint_manipulator(new csv_pprinter(ncomm, v1enc, v2enc))
    { }
    print_as_csv(const csv_pprinter &that):
      pprint_manipulator(new csv_pprinter(that))
    { }
  };

}

#endif // not CSV_PPRINTER_INCLUDED_HH

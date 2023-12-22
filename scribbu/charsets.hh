/**
 * \file charsets.hh
 *
 * Copyright (C) 2015-2022 Michael Herstine <sp1ff@pobox.com>
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

#ifndef CHARSETS_HH_INCLUDED
#define CHARSETS_HH_INCLUDED

#include <locale>
#include <string>
#include <vector>

#include <boost/exception/all.hpp>

#include <iconv.h>

#include <scribbu/errors.hh>

namespace scribbu {

  /// Represents a libiconv error as a C++ exception
  class iconv_error: public virtual boost::exception,
                     public virtual std::runtime_error
  {
  public:
    iconv_error(int err): std::runtime_error(""), errno_(err)
    { }
    int get_errno() const
    { return errno_; }
    virtual const char * what() const noexcept;

  private:
    int errno_;
    mutable std::shared_ptr<std::string> pwhat_;

  };

  enum class encoding {
	// Eurpoean & Russian languages
    ASCII, ISO_8859_1, ISO_8859_2, ISO_8859_3, ISO_8859_4, ISO_8859_5,
    ISO_8859_7, ISO_8859_9, ISO_8859_10, ISO_8859_13, ISO_8859_14,
    ISO_8859_15, ISO_8859_16, KOI8_R, KOI8_U, KOI8_RU, CP1250, CP1251,
    CP1252, CP1253, CP1254, CP1257, CP850, CP866, CP1131, MacRoman,
    MacCentralEurope, MacIceland, MacCroatian, MacRomania, MacCyrillic,
    MacUkraine, MacGreek, MacTurkish, Macintosh,

    // Semitic languages
    ISO_8859_6, ISO_8859_8, CP1255, CP1256, CP862, MacHebrew, MacArabic,

    // Japanese
    EUC_JP, SHIFT_JIS, CP932, ISO_2022_JP, ISO_2022_JP_2, ISO_2022_JP_1,
    ISO_2022_JP_MS,

    // Chinese
    EUC_CN, HZ, GBK, CP936, GB18030, EUC_TW, BIG5, CP950, BIG5_HKSCS,
    BIG5_HKSCS_2004, BIG5_HKSCS_2001, BIG5_HKSCS_1999, ISO_2022_CN,
    ISO_2022_CN_EXT,

    // Korean
    EUC_KR, CP949, ISO_2022_KR, JOHAB,

    // Armenian
    ARMSCII_8,

    // Georgian
    Georgian_Academy, Georgian_PS,

    // Tajik
    KOI8_T,

    // Kazakh
    PT154, RK1048,

    // Thai
    TIS_620, CP874, MacThai,

    // Laotian
    MuleLao_1, CP1133,

    // Vietnamese
    VISCII, TCVN, CP1258,

    // Platform specifics
    HP_ROMAN8, NEXTSTEP,

    // Full Unicode
    UTF_8, UCS_2, UCS_2BE, UCS_2LE, UCS_4, UCS_4BE, UCS_4LE,
    UTF_16, UTF_16BE, UTF_16LE, UTF_32, UTF_32BE, UTF_32LE,
    UTF_7, C99, JAVA,

    MAX_ENCODING,
  };

  std::istream& operator>>(std::istream &is, encoding &x);
  std::ostream& operator<<(std::ostream &os, const encoding &x);

  template <typename char_type>
  struct char_traits
  {
    /// Returns true if char_type is a code unit for encoding \a x
    static bool is_code_unit(encoding x);
  };

  class bad_code_unit: public error {
  public:
    bad_code_unit(encoding enc, std::size_t cb);
    virtual const char * what() const noexcept(true);

  private:
    encoding enc_;
    std::size_t cb_;
    mutable std::shared_ptr<std::string> pwhat_;
  };

  /// Response when a byte sequence in the source encoding cannot
  /// be represented in the target encoding
  enum class on_no_encoding {
    fail, transliterate, ignore
  };

  std::istream& operator>>(std::istream &is, on_no_encoding &x);
  std::ostream& operator<<(std::ostream &os, const on_no_encoding &x);

  namespace detail {

    ///////////////////////////////////////////////////////////////////////////
    //                        iconv-specific details                         //
    ///////////////////////////////////////////////////////////////////////////

    namespace iconv_specific {

      std::string string_for_encoding(encoding enc,
                                      on_no_encoding rsp =
                                      on_no_encoding::fail);
      char* str_for_encoding(encoding enc,
                             on_no_encoding rsp =
                             on_no_encoding::fail);

      class descriptor {

      public:
        descriptor(encoding from, encoding to,
				   on_no_encoding rsp = on_no_encoding::fail):
          from_(from), to_(to) , rsp_(rsp)
        {
          dsc_ = iconv_open(string_for_encoding(to_, rsp_).c_str(),
                            string_for_encoding(from_).c_str());
          if ((iconv_t)-1 == dsc_) {
            throw iconv_error(errno);
          }
        }
        ~descriptor() {
          // Return value intentionally ignored, as we're in a dtor
          iconv_close(dsc_);
        }
        operator iconv_t() const {
          return dsc_;
        }

      private:
        descriptor(const descriptor&) = delete;
        descriptor& operator=(const descriptor&) = delete;

      private:
        encoding from_;
        encoding to_;
        on_no_encoding rsp_;
        iconv_t dsc_;

      };

    } // End namespace iconv_specific.

  } // End namespace detail.

  /**
   * \brief Derive an encoding from the current system locale
   *
   *
   * This function attempts to guess the character encoding in use on the
   * system currently in the following way:
   *
   *   - construct an std::locale with the empty string as argument
   *
   *   - if that instance's name appears to contain a character encoding, and
   *     that encoding can be mapped to a member of the scribbu::encoding
   *     enumeration, return it
   *
   *   - otherwise, examine the LANG environment variable in the same way
   *
   *
   */

  encoding encoding_from_system_locale();

  /**
   * Template parameters can't be deduced from return values, so you'll have
   * to call this like:
   *
   \code

     std::string x = convert_encoding<std::string>(...)

   \endcode
   *
   *
   */

  template<typename string_type>
  string_type convert_encoding(const unsigned char *pbuf,
                               std::size_t cbbuf,
                               encoding srcenc,
                               encoding dstenc,
                               on_no_encoding rsp = on_no_encoding::fail);

  template<typename string_type>
  string_type convert_encoding(const char *pbuf,
                               std::size_t cbbuf,
                               encoding srcenc,
                               encoding dstenc,
                               on_no_encoding rsp = on_no_encoding::fail);

  /**
   * \brief Convert encodings from std strings to buffers of char
   *
   *
   * \param text [in] source text in the form of an std basic_string
   *
   * \param srcenc [in] character encoding in use in \a text
   *
   * \param dstenc [in] target encoding
   *
   * \return a vector of unsigned char containing \a text encoded as \a dstenc
   *
   *
   */

  template <typename string_type>
  std::vector<unsigned char>
  convert_encoding(const string_type &text,
                   encoding srcenc,
                   encoding dstenc,
                   bool add_bom = false,
                   on_no_encoding rsp = on_no_encoding::fail);

  std::vector<unsigned char>
  convert_encoding(const char *ptext,
                   encoding srcenc,
                   encoding dstenc,
                   bool add_bom = false,
                   on_no_encoding rsp = on_no_encoding::fail);

  /// ISO-639-2 language codes; a few of these are (incongruously) in all caps--
  /// that's to keep them from colliding with assorted macros or reserved
  /// keywrods
  enum class language {
    /// deduce language from the program locale
    from_locale,
    aar, // aa, Afar
    abk, // ab, Abkhazian
    ace, //   , Achinese
    ach, //   , Acoli
    ada, //   , Adangme
    ady, //   , Adyghe; Adygei
    afa, //   , Afro-Asiatic languages
    afh, //   , Afrihili
    afr, // af, Afrikaans
    ain, //   , Ainu
    aka, // ak, Akan
    akk, //   , Akkadian
    alb, // sq, Albanian
    ale, //   , Aleut
    alg, //   , Algonquian languages
    alt, //   , Southern Altai
    amh, // am, Amharic
    ang, //   , Old English (ca. 450–1100)
    anp, //   , Angika
    apa, //   , Apache languages
    ara, // ar, Arabic
    arc, //   , Official Aramaic (700–300 BCE); Imperial Aramaic (700–300 BCE)
    arg, // an, Aragonese
    arm, // hy, Armenian
    arn, //   , Mapudungun; Mapuche
    arp, //   , Arapaho
    art, //   , Artificial languages
    arw, //   , Arawak
    ASM, // as, Assamese
    ast, //   , Asturian; Bable; Leonese; Asturleonese
    ath, //   , Athapascan languages
    aus, //   , Australian languages
    ava, // av, Avaric
    ave, // ae, Avestan
    awa, //   , Awadhi
    aym, // ay, Aymara
    aze, // az, Azerbaijani
    bad, //   , Banda languages
    bai, //   , Bamileke languages
    bak, // ba, Bashkir
    bal, //   , Baluchi
    bam, // bm, Bambara
    ban, //   , Balinese
    baq, // eu, Basque
    bas, //   , Basa
    bat, //   , Baltic languages
    bej, //   , Beja; Bedawiyet
    bel, // be, Belarusian
    bem, //   , Bemba
    ben, // bn, Bengali
    ber, //   , Berber languages
    bho, //   , Bhojpuri
    bih, // bh, Bihari languages
    bik, //   , Bikol
    bin, //   , Bini; Edo
    bis, // bi, Bislama
    bla, //   , Siksika
    bnt, //   , Bantu languages
    bod, // bo, Tibetan
    bos, // bs, Bosnian
    bra, //   , Braj
    bre, // br, Breton
    btk, //   , Batak languages
    bua, //   , Buriat
    bug, //   , Buginese
    bul, // bg, Bulgarian
    bur, // my, Burmese
    byn, //   , Bilin; Blin
    cad, //   , Caddo
    cai, //   , Central American Indian languages
    car, //   , Galibi Carib
    cat, // ca, Catalan; Valencian
    cau, //   , Caucasian languages
    ceb, //   , Cebuano
    cel, //   , Celtic languages
    ces, // cs, Czech
    cha, // ch, Chamorro
    chb, //   , Chibcha
    che, // ce, Chechen
    chg, //   , Chagatai
    chi, // zh, Chinese
    chk, //   , Chuukese
    chm, //   , Mari
    chn, //   , Chinook jargon
    cho, //   , Choctaw
    chp, //   , Chipewyan; Dene Suline
    chr, //   , Cherokee
    chu, // cu, Church Slavic; Old Slavonic; Church Slavonic; Old Bulgarian; Old Church Slavonic
    chv, // cv, Chuvash
    chy, //   , Cheyenne
    cmc, //   , Chamic languages
    cnr, //   , Montenegrin
    cop, //   , Coptic
    cor, // kw, Cornish
    cos, // co, Corsican
    cpe, //   , English based Creoles and pidgins
    cpf, //   , French-based Creoles and pidgins
    cpp, //   , Portuguese-based Creoles and pidgins
    cre, // cr, Cree
    crh, //   , Crimean Tatar; Crimean Turkish
    crp, //   , Creoles and pidgins
    csb, //   , Kashubian
    cus, //   , Cushitic languages
    cym, // cy, Welsh
    cze, // cs, Czech
    dak, //   , Dakota
    dan, // da, Danish
    dar, //   , Dargwa
    day, //   , Land Dayak languages
    del, //   , Delaware
    den, //   , Slave (Athapascan)
    deu, // de, German
    dgr, //   , Dogrib
    din, //   , Dinka
    DIV, // dv, Dhivehi; Dhivehi; Maldivian
    doi, //   , Dogri
    dra, //   , Dravidian languages
    dsb, //   , Lower Sorbian
    dua, //   , Duala
    dum, //   , Middle Dutch (ca. 1050–1350)
    dut, // nl, Dutch; Flemish
    dyu, //   , Dyula
    dzo, // dz, Dzongkha
    efi, //   , Efik
    egy, //   , Egyptian (Ancient)
    eka, //   , Ekajuk
    ell, // el, Modern Greek (1453–)
    elx, //   , Elamite
    eng, // en, English
    enm, //   , Middle English (1100–1500)
    epo, // eo, Esperanto
    est, // et, Estonian
    eus, // eu, Basque
    ewe, // ee, Ewe
    ewo, //   , Ewondo
    fan, //   , Fang
    fao, // fo, Faroese
    fas, // fa, Persian
    fat, //   , Fanti
    fij, // fj, Fijian
    fil, //   , Filipino; Pilipino
    fin, // fi, Finnish
    fiu, //   , Finno-Ugrian languages
    fon, //   , Fon
    fra, // fr, French
    fre, // fr, French
    frm, //   , Middle French (ca. 1400–1600)
    fro, //   , Old French (842–ca. 1400)
    frr, //   , Northern Frisian
    frs, //   , Eastern Frisian
    fry, // fy, Western Frisian
    ful, // ff, Fulah
    fur, //   , Friulian
    gaa, //   , Ga
    gay, //   , Gayo
    gba, //   , Gbaya
    gem, //   , Germanic languages
    geo, // ka, Georgian
    ger, // de, German
    gez, //   , Geez
    gil, //   , Gilbertese
    gla, // gd, Gaelic; Scottish Gaelic
    gle, // ga, Irish
    glg, // gl, Galician
    glv, // gv, Manx
    gmh, //   , Middle High German (ca. 1050–1500)
    goh, //   , Old High German (ca. 750–1050)
    gon, //   , Gondi
    gor, //   , Gorontalo
    got, //   , Gothic
    grb, //   , Grebo
    grc, //   , Ancient Greek (to 1453)
    gre, // el, Modern Greek (1453–)
    grn, // gn, Guarani
    gsw, //   , Swiss German; Alemannic; Alsatian
    guj, // gu, Gujarati
    gwi, //   , Gwichʼin
    hai, //   , Haida
    hat, // ht, Haitian; Haitian Creole
    hau, // ha, Hausa
    haw, //   , Hawaiian
    heb, // he, Hebrew
    her, // hz, Herero
    hil, //   , Hiligaynon
    him, //   , Himachali languages; Western Pahari languages
    hin, // hi, Hindi
    hit, //   , Hittite
    hmn, //   , Hmong; Mong
    hmo, // ho, Hiri Motu
    hrv, // hr, Croatian
    hsb, //   , Upper Sorbian
    hun, // hu, Hungarian
    hup, //   , Hupa
    hye, // hy, Armenian
    iba, //   , Iban
    ibo, // ig, Igbo
    ice, // is, Icelandic
    ido, // io, Ido
    iii, // ii, Sichuan Yi; Nuosu
    ijo, //   , Ijo languages
    iku, // iu, Inuktitut
    ile, // ie, Interlingue; Occidental
    ilo, //   , Iloko
    ina, // ia, Interlingua (International Auxiliary Language Association)
    inc, //   , Indic languages
    ind, // id, Indonesian
    ine, //   , Indo-European languages
    inh, //   , Ingush
    ipk, // ik, Inupiaq
    ira, //   , Iranian languages
    iro, //   , Iroquoian languages
    isl, // is, Icelandic
    ita, // it, Italian
    jav, // jv, Javanese
    jbo, //   , Lojban
    jpn, // ja, Japanese
    jpr, //   , Judeo-Persian
    jrb, //   , Judeo-Arabic
    kaa, //   , Kara-Kalpak
    kab, //   , Kabyle
    kac, //   , Kachin; Jingpho
    kal, // kl, Kalaallisut; Greenlandic
    kam, //   , Kamba
    kan, // kn, Kannada
    kar, //   , Karen languages
    kas, // ks, Kashmiri
    kat, // ka, Georgian
    kau, // kr, Kanuri
    kaw, //   , Kawi
    kaz, // kk, Kazakh
    kbd, //   , Kabardian
    kha, //   , Khasi
    khi, //   , Khoisan languages
    khm, // km, Central Khmer
    kho, //   , Khotanese; Sakan
    kik, // ki, Kikuyu; Gikuyu
    kin, // rw, Kinyarwanda
    kir, // ky, Kirghiz; Kyrgyz
    kmb, //   , Kimbundu
    kok, //   , Konkani
    kom, // kv, Komi
    kon, // kg, Kongo
    kor, // ko, Korean
    kos, //   , Kosraean
    kpe, //   , Kpelle
    krc, //   , Karachay-Balkar
    krl, //   , Karelian
    kro, //   , Kru languages
    kru, //   , Kurukh
    kua, // kj, Kuanyama; Kwanyama
    kum, //   , Kumyk
    kur, // ku, Kurdish
    kut, //   , Kutenai
    lad, //   , Ladino
    lah, //   , Lahnda
    lam, //   , Lamba
    lao, // [[bbdb://Larry%20Olivo][Larry]], Lao                                                                            |
    lat, // la, Latin
    lav, // lv, Latvian
    lez, //   , Lezghian
    lim, // [[http://www.linkedin.com][LinkedIn]], Limburgan; Limburger; Limburgish                                               |
    lin, // ln, Lingala
    lit, // lt, Lithuanian
    lol, //   , Mongo
    loz, //   , Lozi
    ltz, // lb, Luxembourgish; Letzeburgesch
    lua, //   , Luba-Lulua
    lub, // lu, Luba-Katanga
    lug, // lg, Ganda
    lui, //   , Luiseno
    lun, //   , Lunda
    luo, //   , Luo (Kenya and Tanzania)
    lus, //   , Lushai
    mac, // mk, Macedonian
    mad, //   , Madurese
    mag, //   , Magahi
    mah, // mh, Marshallese
    mai, //   , Maithili
    mak, //   , Makasar
    mal, // ml, Malayalam
    man, //   , Mandingo
    mao, // mi, Maori
    MAP, //   , Austronesian languages
    mar, // mr, Marathi
    mas, //   , Masai
    may, // ms, Malay
    mdf, //   , Moksha
    mdr, //   , Mandar
    men, //   , Mende
    mga, //   , Middle Irish (900–1200)
    mic, //   , Mi'kmaq; Micmac
    MIN, //   , Minangkabau
    mis, //   , Uncoded languages
    mkd, // mk, Macedonian
    mkh, //   , Mon-Khmer languages
    mlg, // mg, Malagasy
    mlt, // mt, Maltese
    mnc, //   , Manchu
    mni, //   , Manipuri
    mno, //   , Manobo languages
    moh, //   , Mohawk
    mon, // mn, Mongolian
    mos, //   , Mossi
    mri, // mi, Maori
    msa, // ms, Malay
    mul, //   , Multiple languages
    mun, //   , Munda languages
    mus, //   , Creek
    mwl, //   , Mirandese
    mwr, //   , Marwari
    mya, // my, Burmese
    myn, //   , Mayan languages
    myv, //   , Erzya
    nah, //   , Nahuatl languages
    nai, //   , North American Indian languages
    nap, //   , Neapolitan
    nau, // na, Nauru
    nav, // nv, Navajo; Navaho
    nbl, // nr, South Ndebele
    nde, // nd, North Ndebele
    ndo, // ng, Ndonga
    nds, //   , Low German; Low Saxon
    nep, // ne, Nepali
    NEW, //   , Nepal Bhasa; Newari
    nia, //   , Nias
    nic, //   , Niger-Kordofanian languages
    niu, //   , Niuean
    nld, // nl, Dutch; Flemish
    nno, // nn, Norwegian Nynorsk
    nob, // nb, Norwegian Bokmål
    nog, //   , Nogai
    non, //   , Old Norse
    nor, // no, Norwegian
    nqo, //   , N'Ko
    nso, //   , Pedi; Sepedi; Northern Sotho
    nub, //   , Nubian languages
    nwc, //   , Classical Newari; Old Newari; Classical Nepal Bhasa
    nya, // ny, Chichewa; Chewa; Nyanja
    nym, //   , Nyamwezi
    nyn, //   , Nyankole
    nyo, //   , Nyoro
    nzi, //   , Nzima
    oci, // oc, Occitan (post 1500)
    oji, // oj, Ojibwa
    ori, // or, Oriya
    orm, // om, Oromo
    osa, //   , Osage
    oss, // os, Ossetian; Ossetic
    ota, //   , Ottoman Turkish (1500–1928)
    oto, //   , Otomian languages
    paa, //   , Papuan languages
    pag, //   , Pangasinan
    pal, //   , Pahlavi
    pam, //   , Pampanga; Kapampangan
    pan, // pa, Panjabi; Punjabi
    pap, //   , Papiamento
    pau, //   , Palauan
    peo, //   , Old Persian (ca. 600–400 B.C.)
    per, // fa, Persian
    phi, //   , Philippine languages
    phn, //   , Phoenician
    pli, // pi, Pali
    pol, // pl, Polish
    pon, //   , Pohnpeian
    por, // pt, Portuguese
    pra, //   , Prakrit languages
    pro, //   , Old Provençal (to 1500); Old Occitan (to 1500)
    pus, // ps, Pushto; Pashto
    qaa, //   , Reserved for local use
    que, // qu, Quechua
    raj, //   , Rajasthani
    rap, //   , Rapanui
    rar, //   , Rarotongan; Cook Islands Maori
    roa, //   , Romance languages
    roh, // rm, Romansh
    rom, //   , Romany
    ron, // ro, Romanian; Moldavian; Moldovan
    rum, // ro, Romanian; Moldavian; Moldovan
    run, // rn, Rundi
    rup, //   , Aromanian; Arumanian; Macedo-Romanian
    rus, // ru, Russian
    sad, //   , Sandawe
    sag, // sg, Sango
    sah, //   , Yakut
    sai, //   , South American Indian languages
    sal, //   , Salishan languages
    sam, //   , Samaritan Aramaic
    san, // sa, Sanskrit
    sas, //   , Sasak
    sat, //   , Santali
    scn, //   , Sicilian
    sco, //   , Scots
    sel, //   , Selkup
    sem, //   , Semitic languages
    sga, //   , Old Irish (to 900)
    sgn, //   , Sign languages
    shn, //   , Shan
    sid, //   , Sidamo
    sin, // si, Sinhala; Sinhalese
    sio, //   , Siouan languages
    sit, //   , Sino-Tibetan languages
    sla, //   , Slavic languages
    slo, // sk, Slovak
    slk, // sk, Slovak
    slv, // sl, Slovenian
    sma, //   , Southern Sami
    sme, // se, Northern Sami
    smi, //   , Sami languages
    smj, //   , Lule Sami
    smn, //   , Inari Sami
    smo, // sm, Samoan
    sms, //   , Skolt Sami
    sna, // sn, Shona
    snd, // sd, Sindhi
    snk, //   , Soninke
    sog, //   , Sogdian
    som, // so, Somali
    son, //   , Songhai languages
    sot, // st, Southern Sotho
    spa, // es, Spanish; Castilian
    sqi, // sq, Albanian
    srd, // sc, Sardinian
    srn, //   , Sranan Tongo
    srp, // sr, Serbian
    srr, //   , Serer
    ssa, //   , Nilo-Saharan languages
    ssw, // ss,  Swati                                                                          |
    suk, //   , Sukuma
    sun, // su, Sundanese
    sus, //   , Susu
    sux, //   , Sumerian
    swa, // sw, Swahili
    swe, // sv, Swedish
    syc, //   , Classical Syriac
    syr, //   , Syriac
    tah, // ty, Tahitian
    tai, //   , Tai languages
    tam, // ta, Tamil
    tat, // tt, Tatar
    tel, // te, Telugu
    tem, //   , Timne
    ter, //   , Tereno
    tet, //   , Tetum
    tgk, // tg, Tajik
    tgl, // tl, Tagalog
    tha, // th, Thai
    tib, // bo, Tibetan
    tig, //   , Tigre
    tir, // ti, Tigrinya
    tiv, //   , Tiv
    tkl, //   , Tokelau
    tlh, //   , Klingon; tlhIngan-Hol
    tli, //   , Tlingit
    tmh, //   , Tamashek
    tog, //   , Tonga (Nyasa)
    ton, // to, Tonga (Tonga Islands)
    tpi, //   , Tok Pisin
    tsi, //   , Tsimshian
    tsn, // tn, Tswana
    tso, // ts, Tsonga
    tuk, // tk, Turkmen
    tum, //   , Tumbuka
    tup, //   , Tupi languages
    tur, // tr, Turkish
    tut, //   , Altaic languages
    tvl, //   , Tuvalua
    twi, // tw, Twi
    tyv, //   , Tuvinian
    udm, //   , Udmurt
    uga, //   , Ugaritic
    uig, // ug, Uighur; Uyghur
    ukr, // uk, Ukrainian
    umb, //   , Umbundu
    und, //   , Undetermined
    urd, // ur, Urdu
    uzb, // uz, Uzbek
    vai, //   , Vai
    ven, // ve, Venda
    vie, // vi, Vietnamese
    vol, // vo, Volapük
    vot, //   , Votic
    wak, //   , Wakashan languages
    wal, //   , Wolaitta; Wolaytta
    war, //   , Waray
    was, //   , Washo
    wel, // cy, Welsh
    wen, //   , Sorbian languages
    wln, // wa, Walloon
    wol, // without, Wolof                                                                          |
    xal, //   , Kalmyk; Oirat
    xho, // xh, Xhosa
    yao, //   , Yao
    yap, //   , Yapese
    yid, // yi, Yiddish
    yor, // yo, Yoruba
    ypk, //   , Yupik languages
    zap, //   , Zapotec
    zbl, //   , Blissymbols; Blissymbolics; Bliss
    zen, //   , Zenaga
    zgh, //   , Standard Moroccan Tamazight
    zha, // za, Zhuang; Chuang
    zho, // zh, Chinese
    znd, //   , Zande languages
    zul, // zu, Zulu
    zun, //   , Zuni
    zxx, //   , No linguistic content; Not applicable
    zza, //   , Zaza; Dimili; Dimli; Kirdki; Kirmanjki; Zazaki
  }; // End enum class language.

  std::istream& operator>>(std::istream &is, language &x);
  std::ostream& operator<<(std::ostream &os, const language &x);

  language language_from_iso_639_1(char code[2]);
  language language_from_locale();
  void language_to_iso_639_2(language lang, unsigned char code[3]);

} // End namespace scribbu.

namespace std {

  template <>
  class hash<scribbu::encoding> {
  public:
    std::size_t operator()(const scribbu::encoding &x) const noexcept {
      return static_cast<std::size_t>(x);
    }
  };

  template <>
  class hash<scribbu::on_no_encoding> {
  public:
    std::size_t
    operator()(const scribbu::on_no_encoding &x) const noexcept {
      return static_cast<std::size_t>(x);
    }
  };

  template <>
  class hash<scribbu::language> {
  public:
    std::size_t operator()(const scribbu::language &x) const noexcept {
      return static_cast<std::size_t>(x);
    }
  };

}

#endif // not CHARSETS_HH_INCLUDED

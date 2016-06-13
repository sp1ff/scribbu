#include <framesv24.hh>


/*static*/ std::unique_ptr<scrbbu::id3v2_4_frame>
scribbu::UFID_2_4::create(const frame_id4                      &/*id*/,
                          const unsigned char                  *p,
                          std::size_t                           cb,
                          tag_alter_preservation                tag_alter_preservation,
                          file_alter_preservation               file_alter_preservation,
                          read_only                             read_only,
                          const boost::optional<unsigned char> &encryption_method,
                          const boost::optional<unsigned char> &group_id,
                          bool                                  compressed,
                          bool                                  unsynchronised,
                          const boost::optional<std::size_t>   &data_len_indicator)
{
  return std::unique_ptr<scribbu::id3v2_4_frame>( new UFID_2_4( p, p + cb,
                                                                tag_alter_preservation,
                                                                file_alter_preservation,
                                                                read_only,
                                                                encryption_method,
                                                                group_id,
                                                                compressed,
                                                                unsynchronised,
                                                                data_len_indicator ) );
}

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::ENCR_2_4:::create(const frame_id4                      &/*id*/,
                           const unsigned char                  *p,
                           std::size_t                           cb,
                           tag_alter_preservation                tag_alter_preservation,
                           file_alter_preservation               file_alter_preservation,
                           read_only                             read_only,
                           const boost::optional<unsigned char> &encryption_method,
                           const boost::optional<unsigned char> &group_id,
                           bool                                  compressed,
                           bool                                  unsynchronised,
                           const boost::optional<std::size_t>   &data_len_indicator)
{
  return std::unique_ptr<scribbu::id3v2_4_frame>( new ENCR_2_4( p, p + cb,
                                                                tag_alter_preservation,
                                                                file_alter_preservation,
                                                                read_only,
                                                                encryption_method,
                                                                group_id,
                                                                compressed,
                                                                unsynchronised,
                                                                data_len_indicator ) );
}

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::TXXX_2_4::create(const frame_id4                      &id,
                          const unsigned char                  *p,
                          std::size_t                           cb,
                          tag_alter_preservation                tag_alter_preservation,
                          file_alter_preservation               file_alter_preservation,
                          read_only                             read_only,
                          const boost::optional<unsigned char> &encryption_method,
                          const boost::optional<unsigned char> &group_id,
                          bool                                  compressed,
                          bool                                  unsynchronised,
                          const boost::optional<std::size_t>   &data_len_indicator)
{
  return std::unique_ptr<scribbu::id3v2_4_frame>( new TXXX_2_4( p, p + cb,
                                                                tag_alter_preservation,
                                                                file_alter_preservation,
                                                                read_only,
                                                                encryption_method,
                                                                group_id,
                                                                compressed,
                                                                unsynchronised,
                                                                data_len_indicator ) );
}

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::COMM_2_4::create(const frame_id4                      &id,
                          const unsigned char                  *p,
                          std::size_t                           cb,
                          tag_alter_preservation                tag_alter_preservation,
                          file_alter_preservation               file_alter_preservation,
                          read_only                             read_only,
                          const boost::optional<unsigned char> &encryption_method,
                          const boost::optional<unsigned char> &group_id,
                          const boost::optional<std::size_t>   &decompressed_size)
{
  return std::unique_ptr<scribbu::id3v2_4_frame>( new COMM_2_4( p, p + cb,
                                                                tag_alter_preservation,
                                                                file_alter_preservation,
                                                                read_only,
                                                                encryption_method,
                                                                group_id,
                                                                compressed,
                                                                unsynchronised,
                                                                data_len_indicator ) );
}

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::PCNT_2_4::create(const frame_id4                      &id,
                          const unsigned char                  *p,
                          std::size_t                           cb,
                          tag_alter_preservation                tag_alter_preservation,
                          file_alter_preservation               file_alter_preservation,
                          read_only                             read_only,
                          const boost::optional<unsigned char> &encryption_method,
                          const boost::optional<unsigned char> &group_id,
                          const boost::optional<std::size_t>   &decompressed_size)
{
  return std::unique_ptr<scribbu::id3v2_4_frame>( new PCNT_2_4( p, p + cb,
                                                                tag_alter_preservation,
                                                                file_alter_preservation,
                                                                read_only,
                                                                encryption_method,
                                                                group_id,
                                                                compressed,
                                                                unsynchronised,
                                                                data_len_indicator ) );
}

/*static*/ std::unique_ptr<scribbu::id3v2_4_frame>
scribbu::POPM_2_4::create(const frame_id4                      &id,
                          const unsigned char                  *p,
                          std::size_t                           cb,
                          tag_alter_preservation                tag_alter_preservation,
                          file_alter_preservation               file_alter_preservation,
                          read_only                             read_only,
                          const boost::optional<unsigned char> &encryption_method,
                          const boost::optional<unsigned char> &group_id,
                          const boost::optional<std::size_t>   &decompressed_size)
{
  return std::unique_ptr<scribbu::id3v2_4_frame>( new POPM_2_4( p, p + cb,
                                                                tag_alter_preservation,
                                                                file_alter_preservation,
                                                                read_only,
                                                                encryption_method,
                                                                group_id,
                                                                compressed,
                                                                unsynchronised,
                                                                data_len_indicator ) );
}

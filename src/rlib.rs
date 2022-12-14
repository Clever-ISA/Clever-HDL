



#[repr(C,align(8))] // Note: The file is always present at offset 0, which is well-aligned. This is a hint to show that the file can be copied from memory that is aligned to 8 bytes
pub struct RManifestHeader{
    pub magic: [u8;4],
    pub format_ver: [u8;2],
    pub order: u16,
    pub abi_version: i64,
    pub file_contents: u32,
    pub stroff: u32,
    pub croff: u32,
    pub reftaboff: u32
}
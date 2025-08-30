use bevy::prelude::*;
use std::collections::HashMap;
use std::fs::{File, OpenOptions};
use std::io::*;
use std::path::{Path, PathBuf};

/// Side length of a region in chunks (region is `REGION_SIZE x REGION_SIZE`).
pub const REGION_SIZE: i32 = 64;

pub const GBW_MAGIC: [u8; 4] = *b"GBW1";

/// Number of addressable slots per region file (`REGION_SIZE^2`).
const REGION_SLOTS: usize = (REGION_SIZE as usize) * (REGION_SIZE as usize);

/// Header entry for one chunk payload within a region file.
///
/// Serialized as 12 bytes in the file header:
/// - `off: u64` — byte offset of the payload from the start of the file (0 = empty).
/// - `len: u32` — payload length in bytes (0 = empty).
#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct Slot { pub off: u64, pub len: u32 }

/// Open a file, in-memory header, and path for a region.
///
/// Use [`RegionFile::read_chunk`] and [`RegionFile::write_chunk`] to access
/// chunk payloads; they map chunk coordinates to header slots internally.
pub struct RegionFile {
    /// Backing file handle.
    pub f: File,
    /// In-memory copy of the fixed-size header (one [`Slot`] per chunk).
    pub hdr: Vec<Slot>,
    /// Absolute or relative filesystem path to the region file.
    pub path: PathBuf,
}

impl RegionFile {
    /// Opens (or creates) a region file at `path`, initializing the header if missing.
    ///
    /// Behavior:
    /// - Ensures the parent directory exists.
    /// - Creates the file if it does not exist.
    /// - If the file is smaller than the header (`REGION_SLOTS * 12`), writes a zeroed header.
    /// - Otherwise, reads the header into memory.
    ///
    /// # Errors
    /// Returns any I/O error encountered while creating directories, opening the file,
    /// reading/writing the header, or querying metadata.
    pub fn open(path: &Path) -> std::io::Result<Self> {
        std::fs::create_dir_all(path.parent().unwrap())?;
        let mut f = OpenOptions::new().read(true).write(true).create(true).open(path)?;
        let mut hdr_bytes = vec![0u8; REGION_SLOTS * 12];
        let file_len = f.metadata()?.len();
        if file_len < hdr_bytes.len() as u64 {
            f.write_all(&hdr_bytes)?;
            f.flush()?;
        } else {
            f.seek(SeekFrom::Start(0))?;
            f.read_exact(&mut hdr_bytes)?;
        }
        let mut hdr = vec![Slot::default(); REGION_SLOTS];
        for i in 0..REGION_SLOTS {
            let b = &hdr_bytes[i*12..i*12+12];
            let off = u64::from_le_bytes(b[0..8].try_into().unwrap());
            let len = u32::from_le_bytes(b[8..12].try_into().unwrap());
            hdr[i] = Slot { off, len };
        }
        Ok(Self { f, hdr, path: path.to_path_buf() })
    }

    /// Writes the in-memory header back to disk at offset 0.
    ///
    /// This updates all slot offsets/lengths in one contiguous writing.
    ///
    /// # Errors
    /// Propagates any I/O error during seek or write?
    pub fn flush_header(&mut self) -> std::io::Result<()> {
        let mut hdr_bytes = vec![0u8; self.hdr.len() * 12];
        for (i, s) in self.hdr.iter().enumerate() {
            hdr_bytes[i*12..i*12+8].copy_from_slice(&s.off.to_le_bytes());
            hdr_bytes[i*12+8..i*12+12].copy_from_slice(&s.len.to_le_bytes());
        }
        self.f.seek(SeekFrom::Start(0))?;
        self.f.write_all(&hdr_bytes)?;
        self.f.flush()
    }

    /// Reads the payload stored in the header slot `idx`.
    ///
    /// Returns `Ok(None)` when the slot is empty (`off==0 || len==0`).
    ///
    /// # Preconditions
    /// - `idx < REGION_SLOTS`. Out-of-bounds will panic.
    ///
    /// # Errors
    /// Propagates any I/O error during seek or read?
    pub fn read_slot(&mut self, idx: usize) -> std::io::Result<Option<Vec<u8>>> {
        if idx >= self.hdr.len() {
            return Ok(None);
        }
        let s = self.hdr[idx];
        if s.off == 0 || s.len == 0 { return Ok(None); }
        self.f.seek(SeekFrom::Start(s.off))?;
        let mut buf = vec![0u8; s.len as usize];
        self.f.read_exact(&mut buf)?;
        Ok(Some(buf))
    }

    /// Appends `data` to the end of the file and updates header slot `idx`.
    ///
    /// Older payload data (if any) is orphaned; there is no compaction.
    ///
    /// # Preconditions
    /// - `idx < REGION_SLOTS`. Out-of-bounds will panic.
    ///
    /// # Errors
    /// Propagates any I/O error during a seek, write, or header flush.
    pub fn write_slot_append(&mut self, idx: usize, data: &[u8]) -> std::io::Result<()> {
        if idx >= self.hdr.len() {
            return Err(Error::new(ErrorKind::Other, "slot OOB"));
        }
        let end = self.f.seek(SeekFrom::End(0))?;
        self.f.write_all(data)?;
        self.hdr[idx] = Slot { off: end, len: data.len() as u32 };
        self.flush_header()
    }

    /// Reads a chunk payload by its **world** chunk coordinate `coord`.
    ///
    /// Internally computes the region-relative slot index and reads that slot.
    ///
    /// # Errors
    /// Propagates I/O errors from [`read_slot`].
    pub fn read_chunk(&mut self, coord: IVec2) -> std::io::Result<Option<Vec<u8>>> {
        self.read_slot(Self::slot_index_for_chunk(coord))
    }

    /// Writes a chunk payload by its **world** chunk coordinate `coord`.
    ///
    /// Appends data and updates the appropriate header slot.
    ///
    /// # Errors
    /// Propagates I/O errors from [`write_slot_append`].
    pub fn write_chunk(&mut self, coord: IVec2, data: &[u8]) -> std::io::Result<()> {
        self.write_slot_append(Self::slot_index_for_chunk(coord), data)
    }

    /// Computes the header slot index for the world chunk `coord`.
    ///
    /// Equivalent to [`region_slot_index`].
    #[inline]
    fn slot_index_for_chunk(coord: IVec2) -> usize {
        region_slot_index(coord)
    }
}

/// Root folder for the world's safe data and helpers to locate region files.
///
/// Region files are stored under `<root>/region/r.<rx>.<ry>.region`.
#[derive(Resource, Clone)]
pub struct WorldSave { pub root: PathBuf }

impl WorldSave {
    /// Creates a new world save pointing at `root`.
    pub fn new(root: impl Into<PathBuf>) -> Self { Self { root: root.into() } }

    /// Returns `<root>/region`.
    pub fn region_dir(&self) -> PathBuf { self.root.join("region") }

    /// Returns the full path to the region file for region coordinate `r`.
    ///
    /// File name pattern: `r.<x>.<y>.region`.
    pub fn region_path(&self, r: IVec2) -> PathBuf {
        self.region_dir().join(format!("r.{}.{}.region", r.x, r.y))
    }
}

/// In-memory cache of open region files, keyed by **region** coordinates.
///
/// Lazily opens files on first access via [`RegionCache::get_or_open`].
#[derive(Resource, Default)]
pub struct RegionCache(pub HashMap<IVec2, RegionFile>);

impl RegionCache {
    /// Returns a mutable handle to the region file for region `rc`,
    /// opening it if necessary.
    ///
    /// # Errors
    /// Propagates I/O errors from [`RegionFile::open`].
    pub fn get_or_open(
        &mut self,
        ws: &WorldSave,
        rc: IVec2,
    ) -> std::io::Result<&mut RegionFile> {
        if !self.0.contains_key(&rc) {
            let path = ws.region_path(rc);
            let rf = RegionFile::open(&path)?;
            self.0.insert(rc, rf);
        }
        Ok(self.0.get_mut(&rc).unwrap())
    }

    /// Reads a chunk payload at **world** chunk coordinate `coord`.
    ///
    /// # Errors
    /// Propagate I/O errors from region open or slot read.
    pub fn read_chunk(
        &mut self,
        ws: &WorldSave,
        coord: IVec2,
    ) -> std::io::Result<Option<Vec<u8>>> {
        let rc = chunk_to_region(coord);
        let rf = self.get_or_open(ws, rc)?;
        rf.read_chunk(coord)
    }

    /// Writes a chunk payload at **world** chunk coordinate `coord`.
    ///
    /// # Errors
    /// Propagates I/O errors from region open or slot write.
    pub fn write_chunk(
        &mut self,
        ws: &WorldSave,
        coord: IVec2,
        data: &[u8],
    ) -> std::io::Result<()> {
        let rc = chunk_to_region(coord);
        let rf = self.get_or_open(ws, rc)?;
        rf.write_chunk(coord, data)
    }
}

/// Maps a **world** chunk coordinate to its **region** coordinate (`REGION_SIZE` grid).
#[inline]
pub fn chunk_to_region(coord: IVec2) -> IVec2 {
    IVec2::new(
        coord.x.div_euclid(REGION_SIZE),
        coord.y.div_euclid(REGION_SIZE),
    )
}

/// Computes the header slot index for a **world** chunk coordinate.
///
/// The index is the row-major offset inside the `REGION_SIZE x REGION_SIZE`
/// region tile that contains `coord`.
#[inline]
pub fn region_slot_index(coord: IVec2) -> usize {
    let rx = coord.x.rem_euclid(REGION_SIZE) as usize; // 0..REGION_SIZE-1
    let rz = coord.y.rem_euclid(REGION_SIZE) as usize;
    rz * (REGION_SIZE as usize) + rx
}

#[inline]
pub fn pack_slot_bytes(chunk_bytes: Option<&[u8]>, water_bytes: Option<&[u8]>) -> Vec<u8> {
    let cb = chunk_bytes.unwrap_or(&[]);
    let wb = water_bytes.unwrap_or(&[]);
    let mut out = Vec::with_capacity(12 + cb.len() + wb.len());
    out.extend_from_slice(&GBW_MAGIC);
    out.extend_from_slice(&(cb.len() as u32).to_le_bytes());
    out.extend_from_slice(&(wb.len() as u32).to_le_bytes());
    out.extend_from_slice(cb);
    out.extend_from_slice(wb);
    out
}

#[inline]
pub fn unpack_slot_bytes(buf: &[u8]) -> (Option<&[u8]>, Option<&[u8]>) {
    if buf.len() >= 12 && &buf[0..4] == &GBW_MAGIC {
        let cl = u32::from_le_bytes(buf[4..8].try_into().unwrap()) as usize;
        let wl = u32::from_le_bytes(buf[8..12].try_into().unwrap()) as usize;
        let need = 12 + cl + wl;
        if need <= buf.len() {
            let c = if cl > 0 { Some(&buf[12..12+cl]) } else { None };
            let w = if wl > 0 { Some(&buf[12+cl..12+cl+wl]) } else { None };
            return (c, w);
        }
        return (None, None);
    }
    (Some(buf), None)
}
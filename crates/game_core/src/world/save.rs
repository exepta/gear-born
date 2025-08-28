use std::collections::HashMap;
use std::fs::{File, OpenOptions};
use std::io::*;
use std::path::{Path, PathBuf};
use bevy::prelude::*;

pub const REGION_SIZE: i32 = 64;
const REGION_SLOTS: usize = (REGION_SIZE as usize) * (REGION_SIZE as usize);

#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct Slot { pub off: u64, pub len: u32 }

pub struct RegionFile {
    pub f: File,
    pub hdr: Vec<Slot>,
    pub path: PathBuf,
}

impl RegionFile {
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

    pub fn read_slot(&mut self, idx: usize) -> std::io::Result<Option<Vec<u8>>> {
        let s = self.hdr[idx];
        if s.off == 0 || s.len == 0 { return Ok(None); }
        self.f.seek(SeekFrom::Start(s.off))?;
        let mut buf = vec![0u8; s.len as usize];
        self.f.read_exact(&mut buf)?;
        Ok(Some(buf))
    }

    pub fn write_slot_append(&mut self, idx: usize, data: &[u8]) -> std::io::Result<()> {
        let end = self.f.seek(SeekFrom::End(0))?;
        self.f.write_all(data)?;
        self.hdr[idx] = Slot { off: end, len: data.len() as u32 };
        self.flush_header()
    }

    pub fn read_chunk(&mut self, coord: IVec2) -> std::io::Result<Option<Vec<u8>>> {
        self.read_slot(Self::slot_index_for_chunk(coord))
    }

    pub fn write_chunk(&mut self, coord: IVec2, data: &[u8]) -> std::io::Result<()> {
        self.write_slot_append(Self::slot_index_for_chunk(coord), data)
    }

    #[inline]
    fn slot_index_for_chunk(coord: IVec2) -> usize {
        region_slot_index(coord)
    }
}



#[derive(Resource, Clone)]
pub struct WorldSave { pub root: PathBuf }

impl WorldSave {
    pub fn new(root: impl Into<PathBuf>) -> Self { Self { root: root.into() } }
    pub fn region_dir(&self) -> PathBuf { self.root.join("region") }
    pub fn region_path(&self, r: IVec2) -> PathBuf {
        self.region_dir().join(format!("r.{}.{}.region", r.x, r.y))
    }
}

#[derive(Resource, Default)]
pub struct RegionCache(pub HashMap<IVec2, RegionFile>);

impl RegionCache {
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

    pub fn read_chunk(
        &mut self,
        ws: &WorldSave,
        coord: IVec2,
    ) -> std::io::Result<Option<Vec<u8>>> {
        let rc = chunk_to_region(coord);
        let rf = self.get_or_open(ws, rc)?;
        rf.read_chunk(coord)
    }

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

#[inline]
pub fn chunk_to_region(coord: IVec2) -> IVec2 {
    IVec2::new(
        coord.x.div_euclid(REGION_SIZE),
        coord.y.div_euclid(REGION_SIZE),
    )
}

#[inline]
pub fn region_slot_index(coord: IVec2) -> usize {
    let rx = coord.x.rem_euclid(REGION_SIZE) as usize; // 0..REGION_SIZE-1
    let rz = coord.y.rem_euclid(REGION_SIZE) as usize;
    rz * (REGION_SIZE as usize) + rx
}
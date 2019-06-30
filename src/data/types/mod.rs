use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use std::cmp::Eq;

use std::fmt::Debug;

use yaxpeax_arch::{Arch, LengthedInstruction};

use serde::{Serialize, Serializer};
use serde::ser::{SerializeMap, SerializeStruct, SerializeSeq};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Field {
    pub size: u32,
    pub ty: Option<TypeSpec>,
    pub name: Option<String>
}

impl Field {
    pub fn size(size: u32) -> Field {
        Field { size, ty: None, name: None }
    }
    pub fn with_ty(mut self, ty: TypeSpec) -> Self {
        self.ty = Some(ty);
        self
    }
    pub fn with_name<T: Into<String>>(mut self, name: T) -> Self {
        self.name = Some(name.into());
        self
    }
    pub fn type_of(&self) -> TypeSpec {
        self.ty.to_owned().unwrap_or(TypeSpec::Unknown)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeLayout {
    pub name: String,
    fields: Vec<Field>,
    size: u32
}

impl TypeLayout {
    pub fn new(name: String, fields: Vec<Field>) -> Self {
        let size = fields.iter().map(|f| f.size).sum();

        TypeLayout { name, fields, size }
    }

    pub fn size(&self) -> u32 {
        self.size
    }

    pub fn field_for(&self, mut offset: u32) -> Option<&Field> {
        for f in self.fields.iter() {
            if offset == 0 {
                return Some(f);
            } else {
                if f.size > offset {
                    return None;
                } else {
                    offset -= f.size;
                }
            }
        }

        None
    }
}

pub struct TypeAtlas {
    types: Vec<TypeLayout>
}

pub const KPCR: usize = 0;
pub const U64: usize = 1;
pub const I64: usize = 2;
pub const U32: usize = 3;
pub const I32: usize = 4;
pub const U16: usize = 5;
pub const I16: usize = 6;
pub const KTSS64: usize = 7;
pub const KTHREAD: usize = 8;
pub const KPCRB: usize = 9;
pub const AVFrame: usize = 10;

impl TypeAtlas {
    pub fn new() -> TypeAtlas {
        let KPCR_Layout = TypeLayout::new(
            "KPCR".to_string(),
            vec![
                // 0x0000
                Field::size(8).with_ty((TypeSpec::Unknown.pointer_to())).with_name("GdtBase"),
                // 0x0008
                Field::size(8).with_ty(TypeSpec::struct_pointer(KTSS64)).with_name("TssBase"),
                // 0x0010
                Field::size(8).with_ty(TypeSpec::LayoutId(U64)).with_name("UserRsp"),
                // 0x0018
                Field::size(8).with_ty(TypeSpec::struct_pointer(KPCR)).with_name("Self"),
                // 0x0020
                Field::size(8).with_ty(TypeSpec::struct_pointer(KPCRB)).with_name("CurrentPcrb"),
                // 0x0028
                Field::size(0x20).with_name("TODO:KPCRB_data"),
                // 0x0048
                Field::size(8).with_ty(TypeSpec::Unknown.pointer_to()).with_name("unknown_kpcr_field_0x48"),
                // 0x0050
                Field::size(0x10).with_name("TODO:KPCR_data"),
                // 0x0060
                Field::size(2).with_name("MajorVersion"),
                Field::size(2).with_name("MinorVersion"),
                // 0x0064
                Field::size(4).with_name("TODO:KPCR_data"),
                // 0x0068
                Field::size(0x118).with_name("TODO:KPCR_data"),
                // 0x0180
                Field::size(8).with_name("Unknown_KPCRB_field"),
                // 0x0188
                Field::size(8).with_ty(TypeSpec::struct_pointer(KTHREAD)).with_name("CurrentThread"),
                // 0x0190
                Field::size(8).with_ty(TypeSpec::struct_pointer(KTHREAD)).with_name("NextThread"),
                // 0x0198
                Field::size(8).with_ty(TypeSpec::struct_pointer(KTHREAD)).with_name("IdleThread"),
                // 0x01a0
                Field::size(8).with_ty(TypeSpec::LayoutId(U64)).with_name("UserRsp"),
                // 0x01a8
                Field::size(8).with_ty(TypeSpec::LayoutId(U64)).with_name("RspBase"),
                // 0x01b0
                Field::size(0x5e50).with_name("TODO:KPCRB_data"),
                // 0x6000
                Field::size(8).with_name("Unknown_KPCRB_field"),
                // 0x6008
                Field::size(8).with_ty(TypeSpec::Unknown.pointer_to()).with_name("Unknown_KPCRB_field"),
            ]
        );

        let U64_Layout = TypeLayout::new(
            "U64".to_string(),
            vec![Field::size(8)]
        );

        let I64_Layout = TypeLayout::new(
            "I64".to_string(),
            vec![Field::size(8)]
        );

        let U32_Layout = TypeLayout::new(
            "U32".to_string(),
            vec![Field::size(4)]
        );

        let I32_Layout = TypeLayout::new(
            "I32".to_string(),
            vec![Field::size(4)]
        );

        let U16_Layout = TypeLayout::new(
            "U16".to_string(),
            vec![Field::size(2)]
        );

        let I16_Layout = TypeLayout::new(
            "I16".to_string(),
            vec![Field::size(2)]
        );

        let AVFrame_Layout = TypeLayout::new(
            "AVFrame".to_string(),
            vec![
                Field::size(64).with_name("data"),
                Field::size(4 * 8).with_name("linesize"),
                // 0x60
                Field::size(8).with_name("extended_data"),
                Field::size(4).with_name("width"),
                Field::size(4).with_name("height"),
                // 0x70
                Field::size(4).with_name("nb_samples"),
                Field::size(4).with_name("format"),
                Field::size(4).with_name("key_frame"),
                Field::size(4).with_name("pict_type"),
                // 0x80
                Field::size(4).with_name("sample_aspect_ratio.num"),
                Field::size(4).with_name("sample_aspect_ratio.den"),
                Field::size(8).with_name("pts"),
                Field::size(8).with_name("pkt_pts"),
                Field::size(8).with_name("pkt_dts"),
                // 0x98
                Field::size(4).with_name("coded_picture_number"),
                Field::size(4).with_name("display_picture_number"),
                // 0xa0
                Field::size(4).with_name("quality"),
                Field::size(4).with_name("pad_0"),
                Field::size(8).with_name("opaque"),
                Field::size(8 * 8).with_name("error(deprecated)"),
                Field::size(4).with_name("repeat_pict"),
                // 0xf0
                Field::size(4).with_name("interlaced_frame"),
                Field::size(4).with_name("top_field_first"),
                Field::size(4).with_name("palette_has_changed"),
                Field::size(8).with_name("reordered_opaque"),
                Field::size(4).with_name("sample_rate"),
                Field::size(4).with_name("pad_1"),
                Field::size(8).with_name("channel_layout"),
                // 0x110
                Field::size(8 * 8).with_name("buf"),
                // 0x150
                Field::size(8).with_name("extended_buf"),
                Field::size(4).with_name("nb_extended_buf"),
                Field::size(4).with_name("pad_2"),
                Field::size(8).with_name("side_data"),
                Field::size(4).with_name("nb_side_data"),
                // 0x168
                Field::size(4).with_name("flags"),
                Field::size(4).with_name("color_range"),
                // 0x170
                Field::size(4).with_name("color_primaries"),
                Field::size(4).with_name("color_trc"),
                Field::size(4).with_name("colorspace"),
                Field::size(4).with_name("chroma_location"),
                // 0x180
                Field::size(4).with_name("pad_3"),
                Field::size(8).with_name("best_effort_timestamp"),
                Field::size(8).with_name("pkt_pos"),
                Field::size(8).with_name("pkt_duration"),
                Field::size(8).with_name("metadata"),
                // 0x1a0
                Field::size(4).with_name("decode_error_flags"),
                Field::size(4).with_name("channels"),
                Field::size(4).with_name("pkt_size"),
                Field::size(4).with_name("pad_4"),
                Field::size(8).with_name("hw_frames_ctx"),
                Field::size(8).with_name("opaque_ref"),
                Field::size(8).with_name("crop_top"),
                Field::size(8).with_name("crop_bottom"),
                Field::size(8).with_name("crop_left"),
                Field::size(8).with_name("crop_right"),
            ]
        );

        // https://github.com/ntdiff/headers/blob/master/Win10_1507_TS1/x64/System32/hal.dll/Standalone/_KTSS64.h
        let KTSS64_Layout = TypeLayout::new(
            "KTSS64".to_string(),
            vec![
                // 0x0000
                Field { size: 4, ty: Some(TypeSpec::LayoutId(U32)), name: Some("Reserved0".to_string()) },
                // 0x0004
                Field { size: 8, ty: Some(TypeSpec::LayoutId(I64)), name: Some("Rsp0".to_string()) },
                // 0x000c
                Field { size: 8, ty: Some(TypeSpec::PointerTo(Box::new(TypeSpec::LayoutId(I64)))), name: Some("Rsp1".to_string()) },
                // 0x0014
                Field { size: 8, ty: Some(TypeSpec::PointerTo(Box::new(TypeSpec::LayoutId(I64)))), name: Some("Rsp2".to_string()) },
                // 0x001c
                Field { size: 8, ty: Some(TypeSpec::PointerTo(Box::new(TypeSpec::LayoutId(I64)))), name: Some("Ist[0]".to_string()) },
                Field { size: 8, ty: Some(TypeSpec::PointerTo(Box::new(TypeSpec::LayoutId(I64)))), name: Some("Ist[1]".to_string()) },
                Field { size: 8, ty: Some(TypeSpec::PointerTo(Box::new(TypeSpec::LayoutId(I64)))), name: Some("Ist[2]".to_string()) },
                Field { size: 8, ty: Some(TypeSpec::PointerTo(Box::new(TypeSpec::LayoutId(I64)))), name: Some("Ist[3]".to_string()) },
                Field { size: 8, ty: Some(TypeSpec::PointerTo(Box::new(TypeSpec::LayoutId(I64)))), name: Some("Ist[4]".to_string()) },
                Field { size: 8, ty: Some(TypeSpec::PointerTo(Box::new(TypeSpec::LayoutId(I64)))), name: Some("Ist[5]".to_string()) },
                Field { size: 8, ty: Some(TypeSpec::PointerTo(Box::new(TypeSpec::LayoutId(I64)))), name: Some("Ist[6]".to_string()) },
                Field { size: 8, ty: Some(TypeSpec::PointerTo(Box::new(TypeSpec::LayoutId(I64)))), name: Some("Ist[7]".to_string()) },
                // 0x005c
                Field { size: 8, ty: Some(TypeSpec::PointerTo(Box::new(TypeSpec::LayoutId(I64)))), name: Some("Reserved1".to_string()) },
                // 0x0064
                Field { size: 2, ty: Some(TypeSpec::PointerTo(Box::new(TypeSpec::LayoutId(I16)))), name: Some("Reserved2".to_string()) },
                // 0x0066
                Field { size: 2, ty: Some(TypeSpec::PointerTo(Box::new(TypeSpec::LayoutId(I16)))), name: Some("IoMapBase".to_string()) },

            ]
        );

        let KTHREAD_Layout = TypeLayout::new(
            "KTHREAD".to_string(),
            vec![
            ]
        );

        let types = vec![KPCR_Layout.clone(), U64_Layout, I64_Layout, U32_Layout, I32_Layout, U16_Layout, I16_Layout, KTSS64_Layout, KTHREAD_Layout, KPCR_Layout, AVFrame_Layout];

        TypeAtlas { types }
    }

    pub fn name_of(&self, type_spec: &TypeSpec) -> String {
        match type_spec {
            TypeSpec::Top => "[any]".to_string(),
            TypeSpec::LayoutId(id) => { self.types[*id].name.to_string() },
            TypeSpec::PointerTo(spec) => { format!("{}*", self.name_of(spec)) },
            TypeSpec::Unknown => "[unknown]".to_string(),
            TypeSpec::Bottom => "[!]".to_string(),
        }
    }

    pub fn layout_of(&self, type_id: usize) -> &TypeLayout {
        &self.types[type_id]
    }

    pub fn get_field(&self, ty: &TypeSpec, offset: u32) -> Option<&Field> {
        if let TypeSpec::PointerTo(inner) = ty {
            if let TypeSpec::LayoutId(id) = **inner {
                let inner_layout = self.layout_of(id);
                return inner_layout.field_for(offset);
            }
        }

        None
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub enum TypeSpec {
    Top,
    LayoutId(usize),
    PointerTo(Box<TypeSpec>),
    Unknown,
    Bottom,
}

impl TypeSpec {
    pub fn struct_pointer(id: usize) -> Self {
        TypeSpec::PointerTo(Box::new(TypeSpec::LayoutId(id)))
    }
    pub fn pointer_to(self) -> Self {
        TypeSpec::PointerTo(Box::new(self))
    }
}

pub trait Typed {
    fn type_of(&self, type_atlas: &TypeAtlas) -> TypeSpec;
}

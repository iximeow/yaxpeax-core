use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use std::cmp::Eq;
use petgraph::graphmap::GraphMap;
use petgraph;
use petgraph::visit::Bfs;

use std::cell::RefCell;
use std::rc::Rc;

use std::fmt::Debug;

use yaxpeax_arch::{Arch, LengthedInstruction};
use analyses::control_flow::{BasicBlock, ControlFlowGraph};
use memory::MemoryRange;

use serde::{Serialize, Serializer};
use serde::ser::{SerializeMap, SerializeStruct, SerializeSeq};

use serialize::{Memoable, Memos, MemoizingSerializer};

use num_traits::Zero;

pub type DFGRef<A> = Rc<RefCell<Value<A>>>;
pub type RWMap<A> = HashMap<(<A as SSAValues>::Location, Direction), DFGRef<A>>;
pub type PhiLocations<A> = HashMap<<A as SSAValues>::Location, (DFGRef<A>, Vec<DFGRef<A>>)>;

// Look. Just rewrite this as a graph (one day). Vertices are DFGRef, edges are data
// dependences. Secondary graph with vertices (DFGRef | Address) where edges are Address -> DFGRef
// (define) -> Address (use of DFGRef)
//
// in the mean time, DFGRef are growing an (Address, Location) field lol
#[derive(Debug)]
pub struct SSA<A: Arch + SSAValues> where A::Location: Hash + Eq, A::Address: Hash + Eq {
    // TODO: Fairly sure these Rc<RefCell<...>> could all just be raw pointers
    // these aren't individually freed so Rc shouldn't be necessary?
    pub values: HashMap<A::Address, RWMap<A>>,
    pub phi: HashMap<A::Address, PhiLocations<A>>
}

impl <'a, 'b, A: Arch + SSAValues> Serialize for MemoizingSerializer<'a, 'b, HashMap<A::Address, PhiLocations<A>>, HashedValue<DFGRef<A>>> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut phi_map = serializer.serialize_map(Some(self.inner.len()))?;
        for (addr, phis) in self.inner.iter() {
            phi_map.serialize_entry(addr, &MemoizingSerializer::new(unsafe { *self.memos.as_ptr() }, phis))?;
        }
        phi_map.end()
    }
}

impl <'a, 'b, A: Arch + SSAValues> Serialize for MemoizingSerializer<'a, 'b, PhiLocations<A>, HashedValue<DFGRef<A>>> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut location_phis_map = serializer.serialize_map(Some(self.inner.len()))?;
        for (loc, phispec) in self.inner.iter() {
            let new_phiargs: Vec<u32> = phispec.1.iter().map(|v| {
                self.id_of(HashedValue { value: Rc::clone(v) })
            }).collect();
            let newvalue = (self.id_of(HashedValue { value: Rc::clone(&phispec.0) }), new_phiargs);
            location_phis_map.serialize_entry(&format!("{:?}", loc), &newvalue)?;
        }
        location_phis_map.end()
    }
}

impl <'a, 'b, A: Arch + SSAValues> Serialize for MemoizingSerializer<'a, 'b, HashMap<A::Address, RWMap<A>>, HashedValue<DFGRef<A>>> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut version_maps = serializer.serialize_map(Some(self.inner.len()))?;
        for (k, m) in self.inner.iter() {
            version_maps.serialize_entry(k, &MemoizingSerializer::new(unsafe { *self.memos.as_ptr() }, m))?;
        }
        version_maps.end()
    }
}

impl <'a, 'b, A: Arch + SSAValues> Serialize for MemoizingSerializer<'a, 'b, RWMap<A>, HashedValue<DFGRef<A>>> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut version_map = serializer.serialize_map(Some(self.inner.len()))?;
        for ((loc, dir), v) in self.inner.iter() {
            let s = format!("loc={:?}:dir={:?}", loc, dir);
            version_map.serialize_entry(&s, &self.id_of(HashedValue { value: Rc::clone(v) })).unwrap();
        };
        version_map.end()
    }
}

impl <A: Arch + SSAValues> Serialize for Memos<HashedValue<DFGRef<A>>> where HashedValue<DFGRef<A>>: Memoable {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut seq = serializer.serialize_seq(Some(self.node_ids.len()))?;
        for i in 0..self.node_ids.len() {
            for (k, v) in self.node_ids.iter() {
                if (i as u32) == *v {
                    seq.serialize_element(&k.memoize(&self.node_ids))?;
                }
            }
        }
        seq.end()
    }
}

impl <A: Arch + SSAValues + 'static> Serialize for SSA<A> where HashedValue<DFGRef<A>>: Memoable {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut memoizer: Memos<HashedValue<DFGRef<A>>> = Memos::new();

        let mut ssa_serializer = serializer.serialize_struct("SSA", 3)?;

        {
            let values = MemoizingSerializer::new(&mut memoizer, &self.values);
            ssa_serializer.serialize_field("values", &values)?;
        }

        {
            let phis = MemoizingSerializer::new(&mut memoizer, &self.phi);
            ssa_serializer.serialize_field("phis", &phis)?;
        }

        ssa_serializer.serialize_field("values", &memoizer)?;

        ssa_serializer.end()

    }
}

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone, Serialize, Deserialize)]
pub enum Direction {
    Read,
    Write
}

#[derive(Debug)]
pub struct Value<A: SSAValues> {
    // Temporarily necessary to map from some use back to a def site
    pub location: (A::Address, A::Location),
    // None indicates "not written anywhere in this dfg", which indicates this value can
    // be considered an input from some enclosing control flow
    pub version: Option<u32>,
    pub data: Option<A::Data>
}

impl <A: SSAValues> Hash for Value<A> where A::Location: Hash, A::Data: Hash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.location.hash(state);
        self.version.hash(state);
        self.data.hash(state);
    }
}

pub struct DFGLValue<A: SSAValues> {
    pub value: DFGRef<A>
}

impl <A: SSAValues> DFGLValue<A> {
    pub fn update(&self, new_data: A::Data) {
        self.value.borrow_mut().data.replace(new_data);
    }
    pub fn get_data(&self) -> Option<A::Data> {
        self.value.borrow().data.clone()
    }
    pub fn as_rc(self) -> DFGRef<A> {
        self.value
    }
}

#[derive(Debug)]
pub struct HashedValue<A> {
    pub value: A
}

use std::hash::Hasher;
impl <A: SSAValues> Hash for HashedValue<DFGRef<A>> where Value<A>: Hash {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let v: &RefCell<Value<A>> = &*self.value;
        (v.borrow()).hash(state);
    }
}

impl <A: SSAValues> Eq for HashedValue<DFGRef<A>> { }

impl <A: SSAValues> PartialEq for HashedValue<DFGRef<A>> {
    fn eq(&self, other: &HashedValue<DFGRef<A>>) -> bool {
        Rc::ptr_eq(&self.value, &other.value)
    }
}

impl <A: SSAValues> PartialEq for Value<A> {
    fn eq(&self, rhs: &Value<A>) -> bool {
        self as *const Value<A> == rhs as *const Value<A>
    }
}
impl <A: SSAValues> Eq for Value<A> {}

impl <A> Value<A> where A: SSAValues {
    pub fn version(&self) -> Option<u32> {
        self.version
    }
}

impl <A: SSAValues + Arch> Value<A> {
    fn new(addr: A::Address, location: A::Location, version: Option<u32>) -> Value<A> {
        Value {
            location: (addr, location),
            version: version,
            data: None
        }
    }
}

pub trait NoAliasing { }

impl <T> AliasInfo for T where T: NoAliasing + Clone + Copy {
    fn aliases_of(&self) -> Vec<Self> { vec![] }
    fn maximal_alias_of(&self) -> Self { self.clone() }
}

pub trait AliasInfo where Self: Sized {
    fn aliases_of(&self) -> Vec<Self>;
    fn maximal_alias_of(&self) -> Self;
}

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

pub trait SSAValues where Self: Arch {
    type Location: Debug + AliasInfo + Hash + Eq + Serialize;
    type Data: Debug + Hash + Clone + Typed;

    fn decompose(op: &Self::Instruction) -> Vec<(Option<Self::Location>, Direction)>;
}

impl <A: SSAValues> SSA<A> where A::Address: Hash + Eq, A::Location: Hash + Eq {
    fn get_value(&self, addr: A::Address, loc: A::Location, dir: Direction) -> Option<DFGRef<A>> {
        self.values.get(&addr)
            .and_then(|addr_values| addr_values.get(&(loc, dir)))
            .map(|x| Rc::clone(x))
    }
    pub fn try_get_def(&self, addr: A::Address, loc: A::Location) -> Option<DFGRef<A>> {
        self.get_value(addr, loc, Direction::Write)
    }
    pub fn try_get_use(&self, addr: A::Address, loc: A::Location) -> Option<DFGRef<A>> {
        self.get_value(addr, loc, Direction::Read)
    }
    // TODO: These should have a #[cfg()] flag to use after heavy fuzzing that does
    // unreachable_unchecked!() for the None case here.
    //
    // that flag should also remove the try_get_* variants
    pub fn get_def(&self, addr: A::Address, loc: A::Location) -> DFGLValue<A> {
        DFGLValue { value: self.get_value(addr, loc, Direction::Write).unwrap() }
    }
    pub fn get_use(&self, addr: A::Address, loc: A::Location) -> DFGLValue<A> {
        DFGLValue { value: self.get_value(addr, loc, Direction::Read).unwrap() }
    }
}

#[test]
fn test_immediate_dominators_construction() {
    let start = 83u16;
    let map = GraphMap::<u16, (), petgraph::Directed>::from_edges(&[
        (start, 89), (89, 90),
        (89, 91), (90, 89),
        (91, 93), (91, 94)
    ]);
    let idom = petgraph::algo::dominators::simple_fast(&map, start);
    let frontier = compute_dominance_frontiers_from_idom(&map, start, &idom);

    // not distinguishing between something not reachable from the start and something with no
    // frontier
    let mut map = HashMap::new();
    map.insert(89, vec![89]);
    map.insert(90, vec![89]);
    assert!(frontier == map);

    let start = 1u16;
    let map = GraphMap::<u16, (), petgraph::Directed>::from_edges(&[
        (start, 94), (94, 83),
        (83, 88), (88, 90),
        (88, 91), (90, 88),
        (91, 93), (91, 94)
    ]);
    let idom = petgraph::algo::dominators::simple_fast(&map, start);
    let frontier = compute_dominance_frontiers_from_idom(&map, start, &idom);

    // not distinguishing between something not reachable from the start and something with no
    // frontier
    let mut map = HashMap::new();
    // this should be a HashMap<A, HashSet<A>>, not vec.
    map.insert(83, vec![94]);
    map.insert(88, vec![94, 88]);
    map.insert(90, vec![88]);
    map.insert(91, vec![94]);
    map.insert(94, vec![94]);
    assert!(frontier == map);
}

pub fn compute_dominance_frontiers_from_idom<A>(graph: &GraphMap<A, (), petgraph::Directed>, start: A, idom: &petgraph::algo::dominators::Dominators<A>) -> HashMap<A, Vec<A>> where A: Eq + Hash + Copy + Ord + Debug {
    // first thing we do is compute dominance frontiers..
    let mut dominance_frontiers: HashMap<A, Vec<A>> = HashMap::new();

    let mut bfs = Bfs::new(&graph, start);
    while let Some(u) = bfs.next(&graph) {
        let u_idom = match idom.immediate_dominator(u) {
            Some(value) => value,
            None => continue
        };

        /*
        // Not entirely sure how true this one is
        // and even if it's not the case that u_idom <idom> u implies u_idom being present,
        // the loop down on 96 might be adjustable to fit...
        let u_idom = match idom.immediate_dominator(u) {
            Some(value) => value,
            None => continue
        };*/

        // this COULD be rewritten to avoid collecting, since we're just iterating...
        let preds: Vec<A> = graph.neighbors_directed(u, petgraph::Direction::Incoming).collect();
        if preds.len() >= 2 {
            for pred in preds {
                let mut v = pred;
                match idom.immediate_dominator(v) {
                    Some(_value) => (),
                    None => continue // this predecessor is not reachable from start
                };
                while v != u_idom {
                    dominance_frontiers.entry(v).or_insert_with(|| Vec::new()).push(u);
                    v = idom.immediate_dominator(v)
                        .expect("V was reachable, so its immeidate dominator must also be");
                }
            }
        }
    }

    dominance_frontiers
}

pub fn generate_ssa<A: Arch + SSAValues, M: MemoryRange<A::Address>>(
    data: &M,
    entry: A::Address,
    basic_blocks: &ControlFlowGraph<A::Address>,
    cfg: &GraphMap<A::Address, (), petgraph::Directed>
) -> SSA<A> where A::Address: Copy + Ord + Hash + Eq, A::Location: Copy + Hash + Eq {
    let idom = petgraph::algo::dominators::simple_fast(&cfg, entry);

    let dominance_frontiers = compute_dominance_frontiers_from_idom(cfg, entry, &idom);

    // extract out dominance frontiers .... one day...

    let mut all_locations: HashSet<A::Location> = HashSet::new();
    let mut assignments: HashMap<A::Location, HashSet<A::Address>> = HashMap::new();

    use arch::InstructionSpan;

    let mut has_already: HashMap<A::Address, u32> = HashMap::new();
    let mut work: HashMap<A::Address, u32> = HashMap::new();

    let mut bfs = Bfs::new(&cfg, entry);
    while let Some(k) = bfs.next(&cfg) {
        let block = basic_blocks.get_block(k);
        has_already.insert(k, 0);
        work.insert(k, 0);
        let mut iter = data.instructions_spanning::<A::Instruction>(block.start, block.end);
        while let Some((address, instr)) = iter.next() {
            for (maybeloc, direction) in A::decompose(&instr).into_iter() {
                match (maybeloc, direction) {
                    (Some(loc), Direction::Write) => {
                        let widening = loc.maximal_alias_of();
                        all_locations.insert(widening);
                        assignments.entry(widening).or_insert_with(|| HashSet::new()).insert(address);
                    }
                    (None, Direction::Write) => {
                        // TODO: this is a write to something, we don't know what
                        // this should set a bit to indicate potential all-clobber or something
                    }
                    (Some(loc), Direction::Read) => {
                        let widening = loc.maximal_alias_of();
                        all_locations.insert(widening);
                    }
                    (None, Direction::Read) => {
                        // TODO: this is a read from something, but we don't know what
                        // not sure if there's anything we can do with this, really
                    }
                }
            }
        }
    }

    // TODO: some nice abstraction to look up by (Address, Location) but also
    // find all Location for an Address
    let mut values: HashMap<A::Address, RWMap<A>> = HashMap::new();
    let mut phi: HashMap<A::Address, PhiLocations<A>> = HashMap::new();

    let mut iter_count = 0;

    #[allow(non_snake_case)]
    let mut W: VecDeque<A::Address> = VecDeque::new();


//    for each variable in vars {
    for loc in all_locations.iter() {
        iter_count += 1;
//        for each X in A(variable) {
        if ! assignments.contains_key(loc) {
            continue;
        }
        #[allow(non_snake_case)]
        for X in assignments[loc].iter() {
            work.insert(*X, iter_count);
            W.push_back(*X);
        }

        while let Some(x) = W.pop_front() {
            if let Some(frontier) = dominance_frontiers.get(&x) {
                #[allow(non_snake_case)]
                for Y in frontier {
                    if has_already[Y] < iter_count {
                        // versioned at 0xffffffff to indicate a specialness to them.
                        // These should never be present after search().
                        phi.entry(*Y).or_insert_with(|| HashMap::new())
                            .insert(*loc, (Rc::new(RefCell::new(Value::new(*Y, *loc, Some(0xffffffff)))), vec![]));
                        // TODO: phi nodes are assignments too! this is definitely a bug.
                        has_already.insert(*Y, iter_count);
                        if work[Y] < iter_count {
                            work.insert(*Y, iter_count);
                            if !W.contains(Y) {
                                W.push_back(*Y);
                            }
                        }
                    }
                }
            }
        }
    }

    /* phi nodes placed, now number.. */

    /* and for memory
     * track writes to `unknown` w.r.t the current basic block, SSA that forward
     * as a global opaque value. then track if it is set in the current basic block wherein it
     * invalidates reads as a fallback. then ssa resolution goes specific memory first,
     * with fallback to global unknown. global unknown satisfies a write to any specific address.
     * SSA memory::unknown first so we know when looking at precise memory variables if
     * memory should be forgotten (eg marked written w/ no data propagation possible)
     */

    #[allow(non_snake_case)]
    fn search<A: Arch + SSAValues, M: MemoryRange<A::Address>>(
        data: &M,
        block: &BasicBlock<A::Address>,
        values: &mut HashMap<A::Address, RWMap<A>>,
        phi: &mut HashMap<A::Address, PhiLocations<A>>,
        basic_blocks: &ControlFlowGraph<A::Address>,
        cfg: &GraphMap<A::Address, (), petgraph::Directed>,
        dominance_frontiers: &HashMap<A::Address, Vec<A::Address>>,
        idom: &petgraph::algo::dominators::Dominators<A::Address>,
        C: &mut HashMap<A::Location, u32>,
        S: &mut HashMap<A::Location, Vec<DFGRef<A>>>
    ) where <A as SSAValues>::Location: Copy + Hash + Eq, <A as Arch>::Address: Copy + Ord + Hash + Eq, <A as Arch>::Instruction: Debug + LengthedInstruction<Unit=<A as Arch>::Address>, <A as SSAValues>::Location: AliasInfo {
        let mut assignments: Vec<A::Location> = Vec::new();
        // for each statement in block {
        // also check phis at start of the block...
        if let Some(phis) = phi.get(&block.start) {
            for (loc, _data) in phis {
                // these are very clear reads vs assignments:
                let widening = loc.maximal_alias_of();
                let i = C[&widening];
                let mut phi_dest = Rc::clone(&phi[&block.start][loc].0);
                phi_dest.replace(Value::new(block.start, *loc, Some(i)));
                S.get_mut(&widening).expect("S should have entries for all locations.").push(Rc::clone(&phi_dest));
                C.entry(widening).and_modify(|x| *x += 1);
                // for very assignment-heavy blocks maybe there's a good way to summarize multiple of the same location being assigned
                // eg is it faster to store thsi and pop it back or is it faster to just
                // decode again..?
                assignments.push(widening); // ???
            }
        }
        let mut iter = data.instructions_spanning::<A::Instruction>(block.start, block.end);
        while let Some((address, instr)) = iter.next() {
            for (maybeloc, direction) in A::decompose(&instr).into_iter() {
                match (maybeloc, direction) {
                    (Some(loc), Direction::Read) => {
                        let widening = loc.maximal_alias_of();
                        let at_address = values.entry(address).or_insert_with(||
                            HashMap::new()
                        );
                        at_address.insert((loc, Direction::Read), Rc::clone(&S[&widening][S[&widening].len() - 1]));
                    },
                    (None, Direction::Read) => {
                        // it's a read of something, but we don't know what, 
                    },
                    (Some(loc), Direction::Write) => {
                        let widening = loc.maximal_alias_of();
                        let i = C[&widening];
                        let new_value = Rc::new(RefCell::new(Value::new(address, loc, Some(i))));
                        let at_address = values.entry(address).or_insert_with(||
                            HashMap::new()
                        );
                        at_address.insert((loc, Direction::Write), Rc::clone(&new_value));
                        S.get_mut(&widening).expect("S should have entries for all locations.").push(new_value);
                        C.entry(widening).and_modify(|x| *x += 1);
                        // for very assignment-heavy blocks maybe there's a good way to summarize multiple of the same location being assigned
                        // eg is it faster to store thsi and pop it back or is it faster to just
                        // decode again..?
                        assignments.push(widening); // ???
                    },
                    (None, Direction::Write) => {
                        // a write to somewhere, we don't know where, this should def ...
                        // everything
                    }
                }
            }
        }

        // succ == traditional successors-in-cfg
        for Y in cfg.neighbors(block.start) {
//            j = whichpred(Y, block);
//            for each phi in Y {
            if let Some(block_phis) = phi.get_mut(&Y) {
//                for loc in phi.get(Y)
                for (loc, (_dest, args)) in block_phis.iter_mut() {
                    let widening = loc.maximal_alias_of();
//                    phi.operands[j] = .. /* value for S[V] */
//                    // not quite perfect, but good enough
                    args.push(S[&widening][S[&widening].len() - 1].clone());
                }
            }
        }

        // children == nodes immediately dominated by block
//        for each Y in children(block) {
        let mut bfs = Bfs::new(&cfg, block.start);
        while let Some(u) = bfs.next(&cfg) {
            if let Some(u_idom) = idom.immediate_dominator(u) {
                if u_idom == block.start && u != block.start {
                search(
                    data,
                    basic_blocks.get_block(u),
                    values,
                    phi,
                    basic_blocks,
                    cfg,
                    dominance_frontiers,
                    idom,
                    C,
                    S
                );
                }
            }
        }

        for assignment in assignments.into_iter() {
            S.get_mut(&assignment).expect("S should have entries for all locations, right now.").pop();
        }
    }

    #[allow(non_snake_case)]
    let mut C: HashMap<A::Location, u32> = HashMap::new();
    #[allow(non_snake_case)]
    let mut S: HashMap<A::Location, Vec<DFGRef<A>>> = HashMap::new();

    // all_locations should be the widest aliases ONLY
//    for each variable in vars {
    for loc in all_locations {
        C.insert(loc, 0);
        S.insert(loc, vec![Rc::new(RefCell::new(Value::new(A::Address::zero(), loc, None)))]);
    }

    search(
        data,
        basic_blocks.get_block(entry),
        &mut values,
        &mut phi,
        basic_blocks,
        cfg,
        &dominance_frontiers,
        &idom,
        &mut C,
        &mut S
    );

    SSA { values: values, phi: phi }
}

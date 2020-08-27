//! Pretty-printer with the following notable points (which may be good or bad depending on what you want) :
//!
//! (1) Uses a trait (`Doclike`) to allow insertion of different string types (and already-allocated docs)
//! inline. IE `"a".concat(format!("c"), printer).concat(d, printer)` where `d` is some doc that 
//! was previously allocated. "a" gets allocated as a &str, and format!("c") as an owned string automatically.
//!
//! (2) Allows use of borrowed or owned strings (using `Cow<'_, str>`) by default
//!
//! (3) Allows rendering to be done with format/write macros, instead of exclusively to a target string.
//!
//! (4) Includes macros allowing users to cut down on syntactic noise when composing docs.
//!
//! (5) Uses an explicit allocator (`IndexSet`) for efficiency.
//!
//! (6) Has explicit support for Rust Object Notation items. The motivating use case for this crate
//! was writing debug implementations for types that have some degree of indirection (like index pointers)
//! keeping them from using the #[derive(Debug)] macro effectively.
//!
//! Taking the printer as the last argument instead of being the `self` item both allows for 
//! more idiomatic dot composition and plays nicer with the idea that the items of interest are Docs, and
//! the Printer is just a thing we have to eventually tether our docs/text to.
#![allow(unused_parens)]
use std::fmt::{ Display, Formatter, Result as FmtResult };
use std::convert::TryFrom;
use std::borrow::Cow;
use std::hash::{ Hash, BuildHasherDefault };
use std::marker::PhantomData;
use indexmap::IndexSet;
use rustc_hash::FxHasher;

pub mod immut;
pub mod brackets;
pub mod object;
pub mod ron;

pub type FxIndexSet<A> = IndexSet<A, BuildHasherDefault<FxHasher>>;

use Doc::*;

///```
/// use shoebill::{ concat, Printer, Doclike };
/// let mut store = Printer::new();
/// let d = concat!(["a", "b", "c", "d"], &mut store);
/// assert_eq!(format!("{}", d.render(80, &mut store)), format!("abcd"));
///```
#[macro_export]
macro_rules! concat {
    ( [], $ctx:expr ) => {
        Nil.alloc($ctx)
    };
    ( [$d:expr], $ctx:expr ) => {
        $d.alloc($ctx)
    };
    ( [$d1:expr, $d2:expr], $ctx:expr ) => {
        $d1.concat($d2, $ctx)
    };
    ( [$d1:expr, $d2:expr, $($tl:expr),*], $ctx:expr) => {
        {
            let d = $d1.concat($d2, $ctx);
            concat!([d, $($tl),*], $ctx)
        }
    };
}

///```
/// use shoebill::{ concat_w, Printer, Doclike, Doc::* };
/// let mut store = Printer::new();
/// let d = concat_w!(["a", "b", "c", "d"], NewlineZero, &mut store);
/// assert_eq!(format!("{}", d.render(80, &mut store)), format!("a\nb\nc\nd"));
///```
#[macro_export]
macro_rules! concat_w {
    ( [] , $c:expr, $ctx:expr ) => {
        Nil.alloc($ctx)
    };
    ( [$d:expr], $c:expr, $ctx:expr ) => {
        $d.alloc($ctx)
    };
    ( [$d1:expr, $d2:expr], $c:expr, $ctx:expr ) => {
        $d1.concat($c, $ctx).concat($d2, $ctx)
    };
    ( [$d1:expr, $d2:expr, $($tl:expr),*], $c:expr, $ctx:expr) => {
        {
            let d = $d1.concat($c, $ctx).concat($d2, $ctx);
            concat_w!([d, $($tl),*], $c , $ctx)
        }
    };
}

/// Intercalate
#[macro_export]
macro_rules! inter {
    ( [], $sep:expr, $ctx:expr ) => {
        Nil.alloc($ctx)
    };
    ( [$d:expr], $sep:expr, $ctx:expr ) => {
        $d.alloc($ctx)
    };
    ( [$d1:expr, $d2:expr], $sep:expr, $ctx:expr ) => {
        $d1.concat($sep, $ctx).concat($d2, $ctx)
    };
    ( [$d1:expr, $d2:expr, $($tl:expr),*], $sep:expr, $ctx:expr) => {
        {
            let d = $d1.concat($sep, $ctx).concat($d2, $ctx);
            inter!([d, $($tl),*], $sep, $ctx)
        }
    };
}

/// Intercalate, adding a trailing `sep` chracter as well.
#[macro_export]
macro_rules! inter_trailing {
    ( [], $sep:expr, $ctx:expr ) => {
        Nil.alloc($ctx)
    };
    ( [$d:expr], $sep:expr, $ctx:expr ) => {
        $d.alloc($ctx).concat($sep, $ctx)
    };
    ( [$d1:expr, $d2:expr], $sep:expr, $ctx:expr ) => {
        $d1.concat($sep, $ctx).concat($d2, $ctx).concat($sep, $ctx)
    };
    ( [$d1:expr, $d2:expr, $($tl:expr),*], $sep:expr, $ctx:expr) => {
        {
            let d = $d1.concat($sep, $ctx).concat($d2, $ctx);
            inter_trailing!([d, $($tl),*], $sep, $ctx)
        }
    };
}

/// Macro for allowing more pleasant composition items that implement `Doclike` using
/// more familiar operators.
/// The infix operators (which are all left-associtive, but allow for grouping w/ parentheses)
///```
/// //`a <> b` : Concat(a, b)
/// //`a <n> b` : Concat(a, Concat(newline, b))
/// //`a <s> b` : Concat(a, Concat(" ", b)) (concat with a space)
/// //`n @ ds` : prefix ds with a newline
/// //`g @ ds` : make ds a group
/// //`<any u32> @ ds` : nest ds by whatever the amount specified by the integer literal.
///```
/// There are some issues I need to come back to
/// that aren't bugs, but certain things like `"x" (n @ (<s> "y"))` don't work like
/// one might expect them to.
///```rust
///use shoebill::{ compose, Printer, Doclike };
///let mut store = Printer::new();
///let d = compose!(&mut store ; "this" <s> "is" <s> "text");
///assert_eq!(format!("{}", d.render(80, &mut store)), format!("this is text"));
///```
#[macro_export]
macro_rules! compose {
    // Try to collect prefixes first since they're the most specific matches.
    ( $pr:expr; g @ $($rest:tt)* ) => {
        {
            let tl = compose!($pr ; $($rest)*);
            tl.group($pr)
        }
    
    };
    ( $pr:expr; n @ $($rest:tt)* ) => {
        {
            let tl = compose!($pr ; $($rest)*);
            Newline(None).concat(tl, $pr)
        }
    };
    ( $pr:expr; $x:tt @ $($rest:tt)* ) => {
        {
            let tl = compose!($pr ; $($rest)*);
            tl.nest($x, $pr)
        }
    };
    ( $pr:expr; ($($x:tt)*) ) => {
        {
            compose!($pr; $($x)*)
        }
    };
    ( $pr:expr; $l:tt <> $r:tt $($rest:tt)* ) => {
        {
            let l = compose!($pr ; $l);
            let r = compose!($pr ; $r);
            let x = l.concat(r, $pr);
            compose!($pr ; x $($rest)*)
        }
    };
    ( $pr:expr; $l:tt <s> $r:tt $($rest:tt)* ) => {
        {
            let ll = compose!($pr ; $l);
            let rr = compose!($pr ; $r);
            let ll = ll.concat(" ", $pr).concat(rr, $pr);
            compose!($pr ; ll $($rest)*)
        }

    };
    ( $pr:expr; $l:tt <n> $r:tt $($rest:tt)* ) => {
        {
            let ll = compose!($pr ; $l);
            let rr = compose!($pr ; $r);
            let ll = ll.concat(Newline(None), $pr).concat(rr, $pr);
            compose!($pr ; ll $($rest)*)
        }
    };
    ( $pr:expr; $l:tt <z> $r:tt $($rest:tt)* ) => {
        {
            let ll = compose!($pr ; $l);
            let rr = compose!($pr ; $r);
            let ll = ll.concat(NewlineZero, $pr).concat(rr, $pr);
            compose!($pr ; ll $($rest)*)
        }
    };
    ( $pr:expr ; $d:expr ) => { 
        {
            $d.alloc($pr) 
        }
    };
}

pub type CowStr<'p> = Cow<'p, str>;

/// A tagged union of either a Cow string, or a DocPtr. By using
/// this type in conjuction with the `Doclike` trait, we get a pretty
/// large amount of code reuse while cutting down on syntax noise
/// since in many cases we can treat string slices, owned strings,
/// and DocPtr items in the exact same way with the only overhead
/// being that of dealing with the enum (IE there are no extra
/// allocations or clones). 
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StrOrDoc<'p> {
    S(Cow<'p, str>),
    D(DocPtr<'p>)
}

/// Can create a StrOrDoc from anything that can become a Cow string
/// (so a CowStr, a string slice, or an owned string)
impl<'p, A> From<A> for StrOrDoc<'p>
where CowStr<'p> : From<A> {
    fn from(a : A) -> StrOrDoc<'p> {
        StrOrDoc::S(CowStr::from(a))
    }
}

/// Can create a StrOrDoc from a DocPtr
impl<'p> From<DocPtr<'p>> for StrOrDoc<'p> {
    fn from(d : DocPtr<'p>) -> StrOrDoc<'p> {
        StrOrDoc::D(d)
    }
}

/// StrOrDoc is doclike; in the case of a doc, you just get the doc
/// back. In the case of a Cow string, we allocate defer
/// to the implementation that's generic over any A such that A
/// can become a Cow string
impl<'p, P : HasPrinter<'p>> Doclike<'p, P> for StrOrDoc<'p> {
    fn alloc(self, pr : &mut P) -> DocPtr<'p> {
        match self {
            StrOrDoc::S(s) => s.alloc(pr),
            StrOrDoc::D(d) => d
        }
    }
}

/// Anything that can be turned into a Cow string (a CowStr, a string slice,
/// or an owned string) is Doclike.
impl<'p, A, P : HasPrinter<'p>> Doclike<'p, P> for A 
where CowStr<'p> : From<A> {
    fn alloc(self, pr : &mut P) -> DocPtr<'p> {
        let as_cow = CowStr::from(self);
        let (s_idx, _) = pr.printer_mut().strings.insert_full(as_cow);
        let s_idx : u32 = u32::try_from(s_idx).expect("idx > u32::MAX");
        let s_ptr = StringPtr(PhantomData, s_idx);
        let (idx, _) = pr.printer_mut().docs.insert_full(Doc::Text(s_ptr));
        let idx : u32 = u32::try_from(idx).expect("idx > u32::MAX");
        DocPtr(PhantomData, idx as u32)
    }
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StringPtr<'p>(PhantomData<CowStr<'p>>, u32);

impl<'p> StringPtr<'p> {
    pub fn read(self, store : &impl HasPrinter<'p>) -> &CowStr<'p> {
        match self {
            StringPtr(_, idx) => store.printer().strings.get_index(idx as usize).expect("failed to retrieve arena String")
        }
    }
}


pub trait Doclike<'p, P> : Sized 
where P : HasPrinter<'p> {
    fn alloc(self, pr : &mut P) -> DocPtr<'p>;

    fn concat(self, other : impl Doclike<'p, P>, pr : &mut P) -> DocPtr<'p> {
        let l = self.alloc(pr);
        let r = other.alloc(pr);
        Concat {
            l,
            r,
            has_newline : l.has_newline(pr) || r.has_newline(pr),
            dist_next_newline : l.dist_next_newline(pr) + r.dist_next_newline(pr),
            flat_len : l.flat_len(pr) + r.flat_len(pr)
        }.alloc(pr)
    }

    fn nest(self, amt : u32, pr: &mut P) -> DocPtr<'p> {
        let doc = self.alloc(pr);
        Nest {
            amt,
            doc,
            has_newline : doc.has_newline(pr),
            dist_next_newline : doc.dist_next_newline(pr),
            flat_len : doc.flat_len(pr),
        }.alloc(pr)
    }

    fn nest_line(self, other : impl Doclike<'p, P>, amt: u32, pr : &mut P) -> DocPtr<'p> {
        self
        .concat(Newline(None).alloc(pr).nest(amt, pr), pr)
        .concat(other, pr)
    }

    fn nest_doc(self, other : impl Doclike<'p, P>, amt: u32, pr: &mut P) -> DocPtr<'p> {
        //compose!(pr ; self <> (amt @ ((Newline(None)) <> other)))
        let mut d = Newline(None).concat(other, pr);
        d = d.nest(amt, pr);
        d = self.concat(d, pr);
        d
    }

    fn nest_doc_zero(self, other : impl Doclike<'p, P>, amt: u32, pr: &mut P) -> DocPtr<'p> {
        compose!(pr ; self <> (amt @ (NewlineZero <> other)))
    }

    fn group(self, pr : &mut P) -> DocPtr<'p> {
        let doc = self.alloc(pr);
        Group {
            doc,
            has_newline : doc.has_newline(pr),
            dist_next_newline : doc.dist_next_newline(pr),
            flat_len : doc.flat_len(pr),
        }.alloc(pr)
    }
}

impl<'p, P : HasPrinter<'p>> Doclike<'p, P> for DocPtr<'p> {
// This lets us keep blanket implementations of concat/nest/etc since they use 
// 'text' to actually get a DocPtr.
    fn alloc(self, _ : &mut P) -> DocPtr<'p> {
        self
    }
}


impl<'p, P : HasPrinter<'p>> Doclike<'p, P> for Doc<'p> {
    fn alloc(self, pr : &mut P) -> DocPtr<'p> {
        let (idx, _) = pr.printer_mut().docs.insert_full(self);
        let idx : u32 = u32::try_from(idx).expect("doc alloction exceeds u32::MAX");
        DocPtr(PhantomData, idx as u32)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DocPtr<'p>(PhantomData<Doc<'p>>, u32);

/// Renderable is a separate thing so that we can use it with 
/// format! and write! style macros. This has two lifetimes so that you can have multiple
/// Renderable structs in one scope as long as their lifetimes don't overlap. If we ONLY had
/// the <'p> lifetime, we wouldn't be able to do something like:
///```
/// //let s1 = format!("{}", d1.render(printer));
/// //let s2 = format!("{}", d2.render(printer));
///```
/// without getting an error that printer was still borrwed by the first statement,
/// since the borrow by render would be believed to have duration <'p>
pub struct Renderable<'x, 'p : 'x, P : HasPrinter<'p>> {
    doc : DocPtr<'p>,
    printer : &'x P,
    line_width : u16,
}

/// By implementing this as an instance of `Display` for `Renderable`, 
/// you can render something directly to some sink (as a `Formatter`)
/// without having to build the actual string first and then write it.
impl<'x, 'p : 'x, P : HasPrinter<'p>> Display for Renderable<'x, 'p, P>  {
    fn fmt(&self, f : &mut Formatter) -> FmtResult {
        let mut stack = vec![(self.doc, RenderInfo::new(false, 0, self.line_width))];
        let mut size = 0usize;
        let mut eol = self.line_width as usize;
        while let Some((top, info)) = stack.pop() {
            match top.read(self.printer) {
                // Skip all Nil elements.
                Nil => continue,
                // If we get a NewlineZero in flatmode, just skip it entirely.
                NewlineZero if info.flat => continue,
                Newline(Some(alt)) if info.flat => {
                    stack.push((alt, info));
                },
                // If we get a Newline with no specified flatmode repr, just insert a space.
                Newline(None) if info.flat => {
                    eol += 1;
                    size += 1;
                    write!(f, " ")?;
                }
                Newline(_) | NewlineZero => {
                    assert!(!info.flat);
                    write!(f, "\n")?;
                    eol += 1;
                    size += 1;
                    eol += (size + self.line_width as usize);
                    for _ in 0..info.nest {
                        write!(f, " ")?;
                        size += 1;
                    }
                },
                Text(ptr) => {
                    let inner = ptr.read(self.printer.printer());
                    eol += inner.len();
                    size += inner.len();
                    write!(f, "{}", inner)?
                },
                Concat { l, r, .. } => {
                    let lhs_dist_next_newline = if r.has_newline(self.printer) {
                        r.dist_next_newline(self.printer) as u16
                    } else {
                        r.dist_next_newline(self.printer) as u16 + info.dist_next_newline
                    };

                    stack.push((r, info));
                    stack.push((l, info.new_dist_next_newline(lhs_dist_next_newline)));
                },
                Nest { amt, doc, .. } => {
                    let info = info.add_nest(amt as u16);
                    stack.push((doc, info));
                },
                Group { doc, flat_len, dist_next_newline, .. } => {
                    if info.flat || (size + flat_len as usize + dist_next_newline as usize <= eol) {
                        stack.push((doc, info.with_flat(true)))
                    } else {
                        stack.push((doc, info.with_flat(false)))
                    }
                },
            }
        }
        Ok(())
    }
}

impl<'x, 'p : 'x> DocPtr<'p> {
    pub fn render<P : HasPrinter<'p>>(self, line_width : u16, store : &'x P) -> Renderable<'x, 'p, P> {
        Renderable { doc : self, line_width, printer : store }
    }
}

/// The actual thing what holds the information used in composing
/// and printing documents. The Cow strings are held in `strings`, and
/// the doc info is in `Doc`. 
#[derive(Debug)]
pub struct Printer<'p> {
    strings : FxIndexSet<CowStr<'p>>,
    docs : FxIndexSet<Doc<'p>>,
    dedup : bool,
}

/// By implementing these two methods on your structure, you can
/// use it as both a document/string allocator, and to render documents.
/// The idea is that if you have some larget structure that holds the state
/// you want to reference while building and rendering docs, you just give
/// it some way of accessing a Printer storage unit and then you no longer
/// really have to think about it.
impl<'p> HasPrinter<'p> for Printer<'p> {
    fn printer(&self) -> &Printer<'p> {
        self
    }

    fn printer_mut(&mut self) -> &mut Printer<'p> {
        self
    }
}

pub trait HasPrinter<'p> {
    fn printer(&self) -> &Printer<'p>;
    fn printer_mut(&mut self) -> &mut Printer<'p>;
}

impl<'p> Printer<'p> {
    pub fn new() -> Self {
        Printer {
            strings : FxIndexSet::with_hasher(Default::default()),
            docs : FxIndexSet::with_hasher(Default::default()),
            dedup : true,
        }
    }
}

/// The inner document type. Newline gives you the option of specifying
/// an alternate representation (as an Option<StringPtr>) in cases where
/// the renderer is in flat mode, where being in flat mode means you've 
/// either elected to specifically render something as flat, or you elected to
/// group elements together (as a `Group` node), and all of the grouped elements
/// will fit onto the current line without exceeding the line width specified 
/// for rendering.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Doc<'p> {
    Nil,
    NewlineZero,
    Newline(Option<DocPtr<'p>>),
    Text(StringPtr<'p>),
    Concat {
        l : DocPtr<'p>,
        r : DocPtr<'p>,
        flat_len : u32,
        has_newline : bool,
        dist_next_newline : u32,
    },
    Nest {
        amt : u32, 
        doc : DocPtr<'p>,
        flat_len : u32,
        has_newline : bool,
        dist_next_newline : u32,
    },
    Group {
        doc : DocPtr<'p>,
        flat_len : u32,
        has_newline : bool,
        dist_next_newline : u32,
    },
}

impl<'p> DocPtr<'p> {
    pub fn read(self, store : &impl HasPrinter<'p>) -> Doc<'p> {
        match self {
            DocPtr(_, idx) => *store.printer().docs.get_index(idx as usize).expect("DocPtr idx exceeds u32::MAX")
        }
    }

    fn has_newline(&self, pr : &impl HasPrinter<'p>) -> bool {
        match self.read(pr.printer()) {
            Nil => false,
            Newline(..) 
            | NewlineZero => true,
            Concat { has_newline, .. } => has_newline,
            Nest   { has_newline, .. } => has_newline,
            Group  { has_newline, .. } => has_newline,
            Text   { .. }              => false,
        }
    }   
    
    pub fn dist_next_newline(&self, pr : &impl HasPrinter<'p>) -> u32 {
        match self.read(pr.printer()) {
            Nil 
            | Newline(..) 
            | NewlineZero                 => 0,
            Concat   { dist_next_newline, .. }
            | Nest   { dist_next_newline, .. } 
            | Group  { dist_next_newline, .. } => dist_next_newline,
            Text(_)          => self.flat_len(pr),
        }
    }
    
    pub fn flat_len(self, pr : &impl HasPrinter<'p>) -> u32 {
        match self.read(pr.printer()) {
            Nil 
            | NewlineZero               => 0,
            Concat { flat_len, .. }     => flat_len,
            Nest   { flat_len, .. }     => flat_len,
            Group  { flat_len, .. }     => flat_len,
            Newline(None) => 1,
            Newline(Some(d)) => d.flat_len(pr),
            Text(s)          => u32::try_from(s.read(pr).len()).expect("flat_len got a string whose length was > u32::MAX"),
        }
    }
}

/// Just used to move rendering info between nodes.
#[derive(Debug, Clone, Copy)]
pub struct RenderInfo {
    flat : bool,
    nest : u16,
    dist_next_newline : u16,
}

impl Default for RenderInfo {
    fn default() -> Self {
        RenderInfo {
            flat : false,
            nest : 0,
            dist_next_newline : 0
        }
    }
}

impl RenderInfo {
    pub fn new(
        flat : bool, 
        nest : u16, 
        dist_next_newline : u16 
    ) -> Self {
        RenderInfo {
            flat,
            nest,
            dist_next_newline,
        }
    }

    pub fn add_nest(self, addition : u16) -> Self {
        RenderInfo {
            flat : self.flat,
            nest : self.nest + addition,
            dist_next_newline : self.dist_next_newline,
        }
    }

    pub fn new_dist_next_newline(self, dist_next_newline : u16) -> Self {
        RenderInfo {
            flat : self.flat,
            nest : self.nest,
            dist_next_newline,
        }
    }

    pub fn with_flat(self, flat : bool) -> Self {
        RenderInfo {
            flat,
            nest : self.nest,
            dist_next_newline : self.dist_next_newline,
        }
    }
}

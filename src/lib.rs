//! Pretty-printer with the following notable points (which may be good or bad depending on what you want) :
//!
//! (1) Uses a trait [`Doclike`] to allow insertion of different string types (and already-allocated docs)
//! inline. IE `"a".concat(format!("c"), printer).concat(d, printer)` where `d` is some doc that 
//! was previously allocated. "a" gets allocated as a &str, and format!("c") as an owned string automatically.
//!
//! (2) Allows use of borrowed or owned strings (using `Cow<'_, str>`) by default
//!
//! (3) Allows rendering to be done with format/write macros, instead of exclusively to a target string.
//!
//! (4) Includes macros allowing users to cut down on syntactic noise when composing docs.
//!
//! (5) Uses an explicit allocator [`IndexSet`] for efficiency.
//!
//! (6) Has explicit support for [`Rust Object Notation`] items. The motivating use case for this crate
//! was writing debug implementations for types that have some degree of indirection (like index pointers)
//! keeping them from using the #[derive(Debug)] macro effectively.
//!
//! Taking the printer as the last argument instead of being the `self` item both allows for 
//! more idiomatic dot composition and plays nicer with the idea that the items of interest are Docs, and
//! the Printer is just a thing we have to eventually tether our docs/text to.
//! 
//! # Safety
//!
//! There's no unsafe in this crate, but users should be aware of the following two safety
//! points with respect to runtime errors:
//!
//! 1. This crate uses u32 to keep track of Doc elements the length of documents.
//! Attempting to allocate more than u32::MAX unique docs, or attempting
//! to render a string with a length greater than u32::MAX will cause a runtime error.
//! When I have some free time I'll make the use of u64 in those positions an opt-in feature.
//!
//! 2. the API presented by this crate is safe with respect to memory lifetimes; you won't
//! be able to create a situation where you have a DocPtr to a dropped arena/bad memory, but there's
//! no guard rail to prevent you from creating multiple printers and accidentally trying to render a Doc
//! with some printer A when it was actually allocated in printer B. If you happen to do this, 
//! you'll get a runtime error (panic).
//!
//! [`Doclike`]: trait.Doclike.html
//! [`IndexSet`]: ../indexmap/set/struct.IndexSet.html
//! [`Rust Object Notation`]: https://github.com/ron-rs/ron
//! [`Printer`]: struct.Printer.html

#![allow(unused_parens)]
use std::fmt::{ Display, Formatter, Result as FmtResult };
use std::convert::TryFrom;
use std::borrow::Cow;
use std::hash::{ Hash, BuildHasherDefault };
use std::marker::PhantomData;
use indexmap::IndexSet;
use rustc_hash::FxHasher;

pub mod brackets;
pub mod object;
pub mod ron;

type FxIndexSet<A> = IndexSet<A, BuildHasherDefault<FxHasher>>;

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
/// let d = concat_w!(["a", "b", "c", "d"], Hardline, &mut store);
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

/// Macro for allowing more pleasant composition items that implement [`Doclike`] using
/// more familiar operators.
/// The infix operators (which are all left-associtive, but allow for grouping w/ parentheses)
///```
/// //`a <> b` : Concat(a, b)
/// //`a <n> b` : Concat(a, Concat(newline, b))
/// //`a <s> b` : Concat(a, Concat(" ", b)) (concat with a space)
/// //`a <h> b` : Concat(a, Concat(" ", b)) (concat with Hardline)
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
/// [`Doclike`]: trait.Doclike.html
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
            let newline_zero = Newline(Some("".alloc($pr)));
            let ll = ll.concat(newline_zero, $pr).concat(rr, $pr);
            compose!($pr ; ll $($rest)*)
        }
    };
    ( $pr:expr; $l:tt <h> $r:tt $($rest:tt)* ) => {
        {
            let ll = compose!($pr ; $l);
            let rr = compose!($pr ; $r);
            let ll = ll.concat(Hardline, $pr).concat(rr, $pr);
            compose!($pr ; ll $($rest)*)
        }
    };
    ( $pr:expr ; $d:expr ) => { 
        {
            $d.alloc($pr) 
        }
    };
}

type CowStr<'p> = Cow<'p, str>;

/// A tagged union of either a Cow string, or a [`DocPtr`]. By using
/// this type in conjuction with the [`Doclike`] trait, we get a pretty
/// large amount of code reuse while cutting down on syntax noise
/// since in many cases we can treat string slices, owned strings,
/// and DocPtr items in the exact same way with the only overhead
/// being that of dealing with the enum (IE there are no extra
/// allocations or clones). 
///
/// [`DocPtr`]: struct.DocPtr.html
/// [`Doclike`]: trait.Doclike.html
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StrOrDoc<'p> {
    S(Cow<'p, str>),
    D(DocPtr<'p>)
}

/// Can create a [`StrOrDoc`] from anything that can become a Cow string
/// (so a CowStr, a string slice, or an owned string)
///
/// [`StrOrDoc`]: enum.StrOrDoc.html
impl<'p, A> From<A> for StrOrDoc<'p>
where CowStr<'p> : From<A> {
    fn from(a : A) -> StrOrDoc<'p> {
        StrOrDoc::S(CowStr::from(a))
    }
}

/// Can create a StrOrDoc from a DocPtr
///
/// [`StrOrDoc`]: enum.StrOrDoc.html
/// [`DocPtr`]: struct.DocPtr.html
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
/// 
/// [`Doclike`]: trait.Doclike.html
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


/// A pointer to an allocated string or string slice. You should never have to deal
/// with this. It's only public since `Doc` would leak it otherwise.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StringPtr<'p>(PhantomData<CowStr<'p>>, u32);

impl<'p> StringPtr<'p> {
    pub fn read(self, store : &impl HasPrinter<'p>) -> &CowStr<'p> {
        match self {
            StringPtr(_, idx) => store.printer().strings.get_index(idx as usize).unwrap_or_else(|| {
                if idx > u32::MAX {
                    panic!("StringPtr idx cannot exceed u32::MAX. This shouldn't be possible, so please file an issue.")
                } else {
                    panic!(
                        "StringPtr idx not found. The most likely reason for this is that the doc was rendered
                        with the wrong Printer. If you only have one Printer, or you think this is not the case,
                        please file an issue."
                    )
                }
            })
        }
    }
}


/// Trait that defines items we can treat as Doc elements. We can use methods like
/// alloc, concat, group, nest, etc. directly on types implementing Doclike.
pub trait Doclike<'p, P> : Sized 
where P : HasPrinter<'p> {
    /// Generic over any Doclike, so you can use this for text as well.
    fn alloc(self, pr : &mut P) -> DocPtr<'p>;

    fn concat(self, other : impl Doclike<'p, P>, pr : &mut P) -> DocPtr<'p> {
        let l = self.alloc(pr);
        let r = other.alloc(pr);
        Concat {
            l,
            r,
            has_newline : match (l.has_newline(pr), r.has_newline(pr)) {
                (Some(true), _) | (_, Some(true)) => Some(true),
                (Some(false), _) | (_, Some(false)) => Some(false),
                _ => None
            },
            dist_next_newline : if l.has_newline(pr).is_some() {
                l.dist_next_newline(pr)
            } else {
                l.dist_next_newline(pr) + r.dist_next_newline(pr)
            },
            flat_len : if l.has_newline(pr) == Some(true) {
                l.flat_len(pr)
            } else {
                l.flat_len(pr) + r.flat_len(pr)
            }
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
        let newline_zero = Newline(Some("".alloc(pr)));
        compose!(pr ; self <> (amt @ ((newline_zero) <> other)))
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
        let idx : u32 = u32::try_from(idx).expect("number of unique docs allocted cannot exceed u32::MAX");
        DocPtr(PhantomData, idx as u32)
    }
}

/// A cheap (copy-able) pointer to an allocated Doc.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DocPtr<'p>(PhantomData<Doc<'p>>, u32);

/// [`Renderable`] is a separate thing so that we can use it with 
/// format! and write! style macros. This has two lifetimes so that you can have multiple
/// [`Renderable`] structs in one scope as long as their lifetimes don't overlap. If we ONLY had
/// the <'p> lifetime, we wouldn't be able to do something like:
///```
/// //let s1 = format!("{}", d1.render(printer));
/// //let s2 = format!("{}", d2.render(printer));
///```
/// without getting an error that printer was still borrwed by the first statement,
/// since the borrow by render would be believed to have duration <'p>
///
/// [`Renderable`]: struct.Renderable.html
pub struct Renderable<'x, 'p : 'x, P : HasPrinter<'p>> {
    doc : DocPtr<'p>,
    printer : &'x P,
    line_width : u32,
    flat : bool,
}

/// By implementing this as an instance of `Display` for `Renderable`, 
/// you can render something directly to some sink (as a `Formatter`)
/// without having to build the actual string first and then write it.
///
/// [`Renderable`]: struct.Renderable.html
impl<'x, 'p : 'x, P : HasPrinter<'p>> Display for Renderable<'x, 'p, P>  {
    fn fmt(&self, f : &mut Formatter) -> FmtResult {
        let mut stack = vec![(self.doc, RenderInfo::new(self.flat, 0, 0))];
        let mut size = 0usize;
        let mut eol = self.line_width as usize;
        while let Some((top, info)) = stack.pop() {
            match top.read(self.printer) {
                // Skip all Nil elements.
                Nil => continue,
                Newline(Some(alt)) if info.flat => {
                    stack.push((alt, info));
                },
                // If we get a Newline with no specified flatmode repr, just insert a space.
                Newline(None) if info.flat => {
                    eol += 1;
                    size += 1;
                    write!(f, " ")?;
                }
                Hardline | Newline(_) => {
                    assert!(!info.flat || top.read(self.printer) == Hardline);
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
                    let lhs_dist_next_newline = if r.has_newline(self.printer).is_some() {
                        r.dist_next_newline(self.printer)
                    } else {
                        r.dist_next_newline(self.printer) + info.dist_next_newline
                    };

                    stack.push((r, info));
                    stack.push((l, info.new_dist_next_newline(lhs_dist_next_newline)));
                },
                Nest { amt, doc, .. } => {
                    let info = info.add_nest(amt as u16);
                    stack.push((doc, info));
                },
                Group { doc, flat_len, .. } => {
                    if info.flat || (size + flat_len as usize + info.dist_next_newline as usize <= eol) {
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
    pub fn render<P : HasPrinter<'p>>(self, line_width : u32, store : &'x P) -> Renderable<'x, 'p, P> {
        Renderable { doc : self, line_width, printer : store, flat : false }
    }

    pub fn render_flat<P : HasPrinter<'p>>(self, line_width : u32, store : &'x P) -> Renderable<'x, 'p, P> {
        Renderable { doc : self, line_width, printer : store, flat : true }
    }
}

/// The actual thing what holds the information used in composing
/// and printing documents. The Cow strings are held in `strings`, and
/// Holds [`CowStr`] elements and Doc elements in separate [`IndexSet`]s.
/// 
/// [`CowStr`]: type.CowStr.html
/// [`Doc`]: enum.Doc.html
/// [`IndexSet`]: ../indexmap/set/struct.IndexSet.html
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

/// Trait that allows types to allocate and render Docs. See the trait documentation
/// for more details.
///
/// The type `Printer` is itself an instance of `HasPrinter` so you're free to 
/// use that, but the intended purpose of this trait is to allow users to turn one 
/// of their own types into a printer by adding a `Printer` field, then implementing 
/// `HasPrinter` for their type. For example:
///```
/// //pub struct MyState<'p> {
/// //    ast_elems : Vec<AstElems>,
/// //    modifiers : HashMap<Modifier, Defn>,
/// //    printer : Printer<'p>
/// //}
/// //
/// //impl<'p> HasPrinter<'p> for MyState<'p> {
/// //    fn printer(&self) -> &Printer<'p> {
/// //        self
/// //    }
/// //
/// //    fn printer_mut(&mut self) -> &mut Printer<'p> {
/// //        self
/// //    }
/// //}
/// // 
/// // You can now pass an element of MyState anywhere you need to handle documents.
/// // let my_state : &mut MyState<'p> = ...;
/// // let doc1 = ...;
/// // let doc2 = ...;
/// // doc3 = doc1.concat(doc2, my_state);
/// // println!("{}", doc3.render(80, my_state));
///```

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
///
/// The default behavior of `Newline(None)` in flatmode is to render a space.
/// `Hardline` will always render as a linebreak, no matter what.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Doc<'p> {
    Nil,
    Hardline,
    Newline(Option<DocPtr<'p>>),
    Text(StringPtr<'p>),
    Concat {
        l : DocPtr<'p>,
        r : DocPtr<'p>,
        flat_len : u32,
        has_newline : Option<bool>,
        dist_next_newline : u32,
    },
    Nest {
        amt : u32, 
        doc : DocPtr<'p>,
        flat_len : u32,
        has_newline : Option<bool>,
        dist_next_newline : u32,
    },
    Group {
        doc : DocPtr<'p>,
        flat_len : u32,
        has_newline : Option<bool>,
        dist_next_newline : u32,
    },
}

impl<'p> DocPtr<'p> {
    pub fn read(self, store : &impl HasPrinter<'p>) -> Doc<'p> {
        match self {
            DocPtr(_, idx) => store.printer().docs.get_index(idx as usize).copied().unwrap_or_else(|| {
                if idx > u32::MAX {
                    panic!("DocPtr idx cannot exceed u32::MAX. This shouldn't be possible, so please file an issue.")
                } else {
                    panic!(
                        "DocPtr idx not found. The most likely reason for this is that the doc was rendered
                        with the wrong Printer. If you only have one Printer, or you think this is not the case,
                        please file an issue."
                    )
                }
            })
        }
    }

    fn has_newline(&self, pr : &impl HasPrinter<'p>) -> Option<bool> {
        match self.read(pr.printer()) {
            Nil => None,
            Hardline => Some(true),
            Newline(..) => Some(false),
            Concat { has_newline, .. } 
            | Nest   { has_newline, .. }
            | Group  { has_newline, .. } => has_newline,
            Text   { .. }              => None,
        }
    }   
    
    fn dist_next_newline(&self, pr : &impl HasPrinter<'p>) -> u32 {
        match self.read(pr.printer()) {
            Nil 
            | Hardline 
            | Newline(..) => 0,
            Concat   { dist_next_newline, .. }
            | Nest   { dist_next_newline, .. } 
            | Group  { dist_next_newline, .. } => dist_next_newline,
            Text(_)          => self.flat_len(pr),
        }
    }
    
    pub(crate) fn flat_len(self, pr : &impl HasPrinter<'p>) -> u32 {
        match self.read(pr.printer()) {
            Nil 
            | Hardline => 0,
            Concat { flat_len, .. }
            | Nest { flat_len, .. }
            | Group { flat_len, .. } => flat_len,
            Newline(None) => 1,
            Newline(Some(d)) => d.flat_len(pr),
            Text(s)          => u32::try_from(s.read(pr).len()).expect("flat_len got a string whose length was > u32::MAX"),
        }
    }
}

/// Just used to move rendering info between nodes.
#[derive(Debug, Clone, Copy)]
struct RenderInfo {
    flat : bool,
    nest : u16,
    dist_next_newline : u32,
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
    fn new(
        flat : bool, 
        nest : u16, 
        dist_next_newline : u32 
    ) -> Self {
        RenderInfo {
            flat,
            nest,
            dist_next_newline,
        }
    }

    fn add_nest(self, addition : u16) -> Self {
        RenderInfo {
            flat : self.flat,
            nest : self.nest + addition,
            dist_next_newline : self.dist_next_newline,
        }
    }

    fn new_dist_next_newline(self, dist_next_newline : u32) -> Self {
        RenderInfo {
            flat : self.flat,
            nest : self.nest,
            dist_next_newline,
        }
    }

    fn with_flat(self, flat : bool) -> Self {
        RenderInfo {
            flat,
            nest : self.nest,
            dist_next_newline : self.dist_next_newline,
        }
    }
}

// Needs to be here so I don't have to make flat_len() public.
#[cfg(test)]
mod unit_tests {
    use super::*;
    #[test]
    fn flat_len_test1() {
        let mut store = Printer::new();
        let _x = compose!(&mut store ; "four" <h> "five5" <> "six___");
        assert_eq!(_x.flat_len(&store), 4);
        assert_eq!(_x.dist_next_newline(&store), 4);
    }       

    #[test]
    fn flat_len_test2() {
        let mut store = Printer::new();
        let _x = compose!(&mut store ; "four" <n> "five5" <> "six___");
        assert_eq!(_x.flat_len(&store), 16);
        assert_eq!(_x.dist_next_newline(&store), 4);
    }

    #[test]
    fn flat_len_test3() {
        let mut store = Printer::new();
        let _x = compose!(&mut store ; "four" <z> "five5" <> "six___");
        assert_eq!(_x.flat_len(&store), 15);
        assert_eq!(_x.dist_next_newline(&store), 4);
    }

    #[test]
    fn flat_len_test4() {
        let mut store = Printer::new();
        let _x = compose!(&mut store ; "333" <> "four" <h> "five5" <> "six___");
        assert_eq!(_x.flat_len(&store), 7);
        assert_eq!(_x.dist_next_newline(&store), 7);
    }       

    #[test]
    fn flat_len_test5() {
        let mut store = Printer::new();
        let _x = compose!(&mut store ; "333" <> "four" <n> "five5" <> "six___");
        assert_eq!(_x.flat_len(&store), 19);
        assert_eq!(_x.dist_next_newline(&store), 7);
    }

    #[test]
    fn flat_len_test6() {
        let mut store = Printer::new();
        let _x = compose!(&mut store ; "333" <> "four" <z> "five5" <> "six___");
        assert_eq!(_x.flat_len(&store), 18);
        assert_eq!(_x.dist_next_newline(&store), 7);
    }

    #[test]
    fn flat_len_test7() {
        let mut store = Printer::new();
        let _x = compose!(&mut store ; g @ "333" <> "four" <h> "five5" <> "six___");
        assert_eq!(_x.flat_len(&store), 7);
        assert_eq!(_x.dist_next_newline(&store), 7);
    }       

    #[test]
    fn flat_len_test8() {
        let mut store = Printer::new();
        let _x = compose!(&mut store ; g @ "333" <> "four" <n> "five5" <> "six___");
        assert_eq!(_x.flat_len(&store), 19);
        assert_eq!(_x.dist_next_newline(&store), 7);
    }

    #[test]
    fn flat_len_test9() {
        let mut store = Printer::new();
        let _x = compose!(&mut store ; g @ "333" <> "four" <z> "five5" <> "six___");
        assert_eq!(_x.flat_len(&store), 18);
        assert_eq!(_x.dist_next_newline(&store), 7);
    }

}

//! The phantom type parameter `M` is used to prevent mixing up more than one
//! doc arena. In the event that you only have one, or you're confident you won't
//! mix them up, you can just give everything type <'s, ()>. Otherwise, users are
//! encouraged to use IE the type of thing being stored or create their own zero-sized
//! type to tag the arena. Because the parameter is only used as a phantom type, you can use
//! any type for no performance penalty.
//! The phantom lifetime <'s> is used to represent the lifetime of the underlying data
//! stored in the printer prior to rendering. It guarantees that we don't have pointers
//! to data in the arena that outlive the data in the arena.

#![allow(unused_parens)]
use std::fmt::{ Display, Formatter, Result as FmtResult };
use std::hash::{ Hash, BuildHasherDefault };
use std::marker::PhantomData;
use indexmap::IndexSet;
use rustc_hash::FxHasher;

mod impls;
pub mod html;
pub mod brackets;

use Doc::*;

pub mod consts {
    pub const SPACE: &'static str = " ";
    pub const SPACE_LEN: usize = SPACE.len();
    pub const NEWLINE: &'static str = "\n";
    pub const NEWLINE_LEN: usize = NEWLINE.len();
    pub const OPEN_PAREN: &'static str = "(";
    pub const CLOSE_PAREN: &'static str = ")";
    pub const LT: &'static str = "<";
    pub const GT: &'static str = ">";
    pub const EQUALS: &'static str = "=";
    pub const QUOTE: &'static str = "\"";
}

/// # Examples
///
///```
/// # #[macro_use] extern crate shoebill;
/// # fn main() {
/// use shoebill::{ concat_, DocStore, Doclike };
/// let mut store = &mut DocStore::<()>::new();
/// let d = concat_!("a", "b", "c", "d"; store);
/// assert_eq!(format!("{}", d.render(80, store)), format!("abcd"));
/// # }
///```
#[macro_export]
macro_rules! concat_ {
    ( ; $ctx:expr ) => {
        Nil.alloc($ctx)
    };
    ( $d:expr ; $ctx:expr ) => {
        $d.alloc($ctx)
    };
    ( $d1:expr, $d2:expr ; $ctx:expr ) => {
        $d1.concat($d2, $ctx)
    };
    ( $d1:expr, $d2:expr, $($tl:expr),*; $ctx:expr) => {
        {
            let d = $d1.concat($d2, $ctx);
            concat_!(d, $($tl),*; $ctx)
        }
    };
}

/// Concatenate a list of items, intercalating a doc between them.
///
/// Takes `[docs*, item_to_intercalate, printer]`
///
/// # Examples
///
///```
/// # #[macro_use] extern crate shoebill;
/// # fn main() {
/// use shoebill::{ intercal, DocStore, Doclike, Doc::* };
/// let mut store = &mut DocStore::<()>::new();
/// let d1 = intercal!("a", "b", "c", "d"; " ", store);
/// assert_eq!(format!("{}", d1.render(80, store)), format!("a b c d"));
///
/// let d2 = intercal!("a", "b", "c", "d"; Hardline, store);
/// assert_eq!(format!("{}", d2.render(80, store)), format!("a\nb\nc\nd"));
/// # }
///```
#[macro_export]
macro_rules! intercal {
    ( ; $sep:expr, $ctx:expr ) => {
        Nil.alloc($ctx)
    };
    ( $d:expr ;  $sep:expr, $ctx:expr ) => {
        $d.alloc($ctx)
    };
    ( $d1:expr, $d2:expr ; $sep:expr, $ctx:expr ) => {
        $d1.concat($sep, $ctx).concat($d2, $ctx)
    };
    ( $d1:expr, $d2:expr, $($tl:expr),* ; $sep:expr, $ctx:expr) => {
        {
            let d = $d1.concat($sep, $ctx).concat($d2, $ctx);
            intercal!(d, $($tl),* ; $sep, $ctx)
        }
    };
}

/// Concatenate a list of items, intercalating a doc between them, and adding
/// an instance of the separating doc to the tail. That is, "a, inter, b, inter, c, inter"
/// instead of "a, inter, b, inter, c"
/// 
/// Takes `[docs*, item_to_intercalate, printer]`
///
/// # Examples
///
///```
/// # #[macro_use] extern crate shoebill;
/// # fn main() {
/// use shoebill::{ intercal_trailing, DocStore, Doclike, Doc::* };
/// let mut store = &mut DocStore::<()>::new();
/// let d1 = intercal_trailing!("a", "b", "c", "d"; " ", store);
/// assert_eq!(format!("{}", d1.render(80, store)), format!("a b c d "));
///
/// let d2 = intercal_trailing!("a", "b", "c", "d"; Hardline, store);
/// assert_eq!(format!("{}", d2.render(80, store)), format!("a\nb\nc\nd\n"));
/// # }
///```
#[macro_export]
macro_rules! intercal_trailing {
    ( ; $sep:expr, $last:expr, $ctx:expr ) => {
        Nil.alloc($ctx)
    };
    ( $d:expr ;  $sep:expr, $last:expr, $ctx:expr ) => {
        $d.alloc($ctx).concat($last, $ctx)
    };
    ( $d1:expr, $d2:expr ; $sep:expr, $last:expr, $ctx:expr ) => {
        $d1.concat($sep, $ctx).concat($d2, $ctx).concat($last, $ctx)
    };
    ( $d1:expr, $d2:expr, $($tl:expr),* ; $sep:expr, $last:expr, $ctx:expr) => {
        {
            let d = $d1.concat($sep, $ctx).concat($d2, $ctx);
            intercal_trailing!(d, $($tl),* ; $sep, $last, $ctx)
        }
    };
    ( ; $sep:expr, $ctx:expr ) => {
        Nil.alloc($ctx)
    };
    ( $d:expr ;  $sep:expr, $ctx:expr ) => {
        $d.alloc($ctx).concat($sep, $ctx)
    };
    ( $d1:expr, $d2:expr ; $sep:expr, $ctx:expr ) => {
        $d1.concat($sep, $ctx).concat($d2, $ctx).concat($sep, $ctx)
    };
    ( $d1:expr, $d2:expr, $($tl:expr),* ; $sep:expr, $ctx:expr) => {
        {
            let d = $d1.concat($sep, $ctx).concat($d2, $ctx);
            intercal_trailing!(d, $($tl),* ; $sep, $ctx)
        }
    };
}

pub(crate) type FxIndexSet<A> = IndexSet<A, BuildHasherDefault<FxHasher>>;

#[cfg(not(feature="u64_indices"))]
type IDX = u32;

#[cfg(feature="u64_indices")]
type IDX = u64;

/// This comes up when creating a new `Concat` doc. The calculation of flat len
/// changes depending on whether the line break is hard or soft; if it's hard, you know
/// you can discount the length following the hard linebreak. If it's soft, 
///```text
/// from `concat()
///flat_len : if l.has_newline() == Some(NewlineKind::Hard) {
///    l.flat_len(pr)
///} else {
///    l.flat_len(pr) + r.flat_len(pr)
///}
///```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NewlineKind {
    Soft,
    Hard
}

/// The structure of `StringPtr<'s, M>` and the implementations on it are such that
/// string slices which are known to live at least as long as the printer can be
/// put in docs without allocating, and without having to later look them up 
/// during rendering/retrieval.
/// A larger purpose of this is that for users who have large which are going to get used
/// very frequently, are or are very large, you avoid the negatives of the vec approach and
/// the hashmap approach; you don't get the duplication and memory consumption of the vec approach,
/// and you don't have the performance overhead of hashing/equality comparison that you get
/// with the deduplicated collection.
#[derive(Debug)]
pub enum StringPtr<'s, M> {
    Str(&'s str),
    Idx(IDX, PhantomData<&'s M>),
}

#[derive(Debug)]
pub struct DocPtr<'s, M>(IDX, PhantomData<&'s M>);

impl<'s, M> DocPtr<'s, M> {
    pub fn read(self, store: &impl HasDocs<'s, M>) -> Doc<'s, M> {
        // We can unwrap this since the existence of a DocPtr means that the index is in-bounds.
        store.store().docs.get_index(self.0 as usize).copied().unwrap()
    }
}

impl<'a, 's : 'a, M> StringPtr<'s, M> {
    pub fn read(self, store: &'a impl HasDocs<'s, M>) -> &'a str {
        match self {
            // We can unwrap this since the existence of a StringPtr means that the index is in-bounds.
            StringPtr::Idx(idx, _) => store.store().strings.get_index(idx as usize).unwrap(),
            StringPtr::Str(s) => s
        }
    }
}

#[derive(Debug)]
pub enum Doc<'s, M> {
    Nil,
    // A line break that will always render as a line break, even if it's in a larger `Group` doc,
    // or if the printer has specifically been told to print flat.
    Hardline,
    // A doc that depending on the rendering circumstances either renders as a line break, or as
    // the alternate inner doc.
    // The circumstances under which this will render as the inner text are:
    // 1. The printer has specifically been told to render everything flat.
    // 2. The Newline doc is part of a `Group` doc, and all of that `Group`'s contents will fit 
    //    on the current line.
    // In all other cases, this will print as a "\n" character, and the inner Doc will be discarded.
    // 
    // If no alternate Doc is specified (Newline(None)) and the rendering state is flat, this will
    // be rendered as a space, counting for 1 character width.
    Newline(Option<DocPtr<'s, M>>),
    // Because users won't have to ever see the pointer type for Ptr<Text>, it's fine to inline
    // the pointer elements
    Text(StringPtr<'s, M>),
    // Intended to allow insertion of things like escape codes or special formatting characters
    // without affecting the layout.
    ZeroWidthText(StringPtr<'s, M>),    
    Concat {
        l : DocPtr<'s, M>,
        r : DocPtr<'s, M>,
        flat_len : usize,
        has_newline : Option<NewlineKind>,
        dist_next_newline : usize,
    },
    // Increase the nest amt for any later newlines that get printed.
    Nest {
        doc : DocPtr<'s, M>,
        nest_amt : u16, 
        flat_len : usize,
        has_newline : Option<NewlineKind>,
        dist_next_newline : usize,
    },
    // Try to print docs together on the same line despite the presence of soft newlines.
    // If you have G(d), where d is some other tree of docs, the render function
    // will check whether the content of `d` will fit on the same line. If it will, it will
    // all be rendered on the same line, and any `Newline` elements will be replaced by their
    // alternate representation. Hardlines will always be printed as line breaks even if grouped.
    Group {
        doc : DocPtr<'s, M>,
        flat_len : usize,
        has_newline : Option<NewlineKind>,
        dist_next_newline : usize,
    },    
}

pub trait HasDocs<'s, M> {
    fn store(&self) -> &DocStore<'s, M>;
    fn store_mut(&mut self) -> &mut DocStore<'s, M>;
}

pub trait Doclike<'s, M>: Sized {
    fn alloc_full(self, store: &mut impl HasDocs<'s, M>) -> (Doc<'s, M>, DocPtr<'s, M>);

    fn alloc(self, store: &mut impl HasDocs<'s, M>) -> DocPtr<'s, M> {
        self.alloc_full(store).1
    }
    
    fn scope(self, open: impl Doclike<'s, M>, close: impl Doclike<'s, M>, nest_amt: u16, pr: &mut impl HasDocs<'s, M>) -> Doc<'s, M> {
        open.concat_newline(self.nest(nest_amt, pr), pr).concat_newline(close, pr)
    }

    fn concat(self, other : impl Doclike<'s, M>, pr : &mut impl HasDocs<'s, M>) -> Doc<'s, M> {
        let (l_d, l) = self.alloc_full(pr);
        let (r_d, r) = other.alloc_full(pr);
        Concat {
            l,
            r,
            // If either the left or right doc has a hardline, then the concatenated doc
            // is characterized by the hardline. If there's only a softline, then the concatenated
            // doc is characterized by that. Otherwise it just has no newline elements.
            has_newline : match (l_d.has_newline(), r_d.has_newline()) {
                (Some(NewlineKind::Hard), _) | (_, Some(NewlineKind::Hard)) => Some(NewlineKind::Hard),
                (Some(NewlineKind::Soft), _) | (_, Some(NewlineKind::Soft)) => Some(NewlineKind::Soft),
                _ => None
            },
            dist_next_newline : if l_d.has_newline().is_some() {
                l_d.dist_next_newline(pr)
            } else {
                l_d.dist_next_newline(pr) + r_d.dist_next_newline(pr)
            },
            flat_len : if l_d.has_newline() == Some(NewlineKind::Hard) {
                l_d.flat_len(pr)
            } else {
                l_d.flat_len(pr) + r_d.flat_len(pr)
            }
        }
    }

    fn concat_space(self, other: impl Doclike<'s, M>, pr: &mut impl HasDocs<'s, M>) -> Doc<'s, M> {
        self
        .concat(" ", pr)
        .concat(other, pr)
    }    

    fn concat_newline(self, other: impl Doclike<'s, M>, pr: &mut impl HasDocs<'s, M>) -> Doc<'s, M> {
        self
        .concat(Doc::Newline(None), pr)
        .concat(other, pr)
    }    

    fn nest(self, nest_amt : u16, pr: &mut impl HasDocs<'s, M>) -> Doc<'s, M> {
        let (doc, ptr) = self.alloc_full(pr);
        Nest {
            nest_amt,
            doc: ptr,
            has_newline : doc.has_newline(),
            dist_next_newline : doc.dist_next_newline(pr),
            flat_len : doc.flat_len(pr),
        }
    }

    fn nest_line(self, other : impl Doclike<'s, M>, amt: u16, pr : &mut impl HasDocs<'s, M>) -> Doc<'s, M> {
        self
        .concat(Newline(None).nest(amt, pr), pr)
        .concat(other, pr)
    }

    fn nest_doc(self, other : impl Doclike<'s, M>, amt: u16, pr: &mut impl HasDocs<'s, M>) -> Doc<'s, M> {
        self.concat(
            Newline(None)
            .concat(other, pr)
            .nest(amt, pr),
            pr
        )
    }

    fn group(self, pr : &mut impl HasDocs<'s, M>) -> Doc<'s, M> {
        let (doc, ptr) = self.alloc_full(pr);
        Group {
            doc: ptr,
            has_newline : doc.has_newline(),
            dist_next_newline : doc.dist_next_newline(pr),
            flat_len : doc.flat_len(pr),
        }
    }     

}

impl<'s, M> Doclike<'s, M> for Doc<'s, M> {
    fn alloc_full(self, store: &mut impl HasDocs<'s, M>) -> (Doc<'s, M>, DocPtr<'s, M>) {
        let (idx, _) = store.store_mut().docs.insert_full(self);
        (self, DocPtr(idx as IDX, PhantomData))
    }
}

impl<'s, M> Doclike<'s, M> for &'s str {
    fn alloc_full(self, store: &mut impl HasDocs<'s, M>) -> (Doc<'s, M>, DocPtr<'s, M>) {
        let doc = Text(StringPtr::Str(self));
        let (idx, _) = store.store_mut().docs.insert_full(doc);
        (doc, DocPtr(idx as IDX, PhantomData))
    }
}

impl<'s, M> Doclike<'s, M> for String {
    fn alloc_full(self, store: &mut impl HasDocs<'s, M>) -> (Doc<'s, M>, DocPtr<'s, M>) {
        let (s_idx, _) = store.store_mut().strings.insert_full(self);
        let string_ptr = StringPtr::Idx(s_idx as IDX, PhantomData);
        let doc = Text(string_ptr);
        let (idx, _) = store.store_mut().docs.insert_full(doc);
        (doc, DocPtr(idx as IDX, PhantomData))
    }
}

impl<'s, M> Doclike<'s, M> for DocPtr<'s, M> {
    fn alloc_full(self, store: &mut impl HasDocs<'s, M>) -> (Doc<'s, M>, DocPtr<'s, M>) {
        (self.read(store), self)
    }
}


impl<'s, M> Doc<'s, M> {
    fn has_newline(self) -> Option<NewlineKind> {
        match self {
            Nil => None,
            Hardline => Some(NewlineKind::Hard),
            Newline {..} => Some(NewlineKind::Soft),
            | Concat { has_newline, .. } 
            | Nest   { has_newline, .. }
            | Group  { has_newline, .. } => has_newline,
            Text   {..}              => None,
            ZeroWidthText {..}              => None,
        }
    }   
    
    fn dist_next_newline(self, pr : &impl HasDocs<'s, M>) -> usize {
        match self {
            Nil 
            | Hardline 
            | Newline {..} => 0,
            Concat   { dist_next_newline, .. }
            | Nest   { dist_next_newline, .. } 
            | Group  { dist_next_newline, .. } => dist_next_newline,
            Text {..}          => self.flat_len(pr),
            ZeroWidthText {..}              => 0,
        }
    }
    
    pub(crate) fn flat_len(self, pr : &impl HasDocs<'s, M>) -> usize {
        match self {
            Nil 
            | Hardline => 0,
            Concat { flat_len, .. }
            | Nest { flat_len, .. }
            | Group { flat_len, .. } => flat_len,
            // In the case of no `alt`, this is rendered as a single space, so it's a size of 1.
            Newline(None) => 1,
            // If this were to be rendered in flat mode, we would use the inner/alt doc, so we only
            // care about what its size is.
            Newline(Some(alt)) => alt.read(pr).flat_len(pr),
            Text(sptr)       => sptr.read(pr).len(),
            ZeroWidthText {..} => 0,
        }
    }    
}

#[derive(Debug)]
pub struct DocStore<'s, M> {
    pub(crate) strings: FxIndexSet<String>,
    docs: FxIndexSet<Doc<'s, M>>
}

impl<'s, M> HasDocs<'s, M> for DocStore<'s, M> {
    fn store(&self) -> &DocStore<'s, M> { self }
    fn store_mut(&mut self) -> &mut DocStore<'s, M> { self }
}

impl<'s, M> DocStore<'s, M> {
    pub fn new() -> Self { Self::default() }
}

impl<'s, M> Default for DocStore<'s, M> {
    fn default() -> Self {
        DocStore {
            strings: FxIndexSet::with_hasher(Default::default()),
            docs: FxIndexSet::with_hasher(Default::default())
        }
    }
}


/// Just used to move rendering info between nodes.
#[derive(Debug, Clone, Copy)]
struct RenderInfo {
    flat : bool,
    nest : u16,
    dist_next_newline : usize,
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
        dist_next_newline : usize
    ) -> Self {
        RenderInfo {
            flat,
            nest,
            dist_next_newline,
        }
    }
}

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
pub struct Renderable<'x, 's : 'x, M, S: HasDocs<'s, M>> {
    doc : Doc<'s, M>,
    store : &'x S,
    line_width : usize,
    flat : bool,
}

/// By implementing this as an instance of `Display` for `Renderable`, 
/// you can render something directly to some sink (as a `Formatter`)
/// without having to build the actual string first and then write it.
///
/// [`Renderable`]: struct.Renderable.html
impl<'x, 's : 'x, M, S : HasDocs<'s, M>> Display for Renderable<'x, 's, M, S>  {
    fn fmt(&self, f : &mut Formatter) -> FmtResult {
        let mut stack = vec![(self.doc, RenderInfo::new(self.flat, 0, 0))];
        let mut size = 0usize;
        let mut eol = self.line_width as usize;
        while let Some((top, info)) = stack.pop() {
            match top {
                // Skip all Nil elements.
                Nil => continue,
                // flatmode + alt specified
                Newline(Some(alt)) if info.flat => stack.push((alt.read(self.store), info)),
                // flatmode + no alt specified
                Newline(None) if info.flat => {
                    f.write_str(consts::SPACE)?;
                    size += consts::SPACE_LEN;
                }
                // not flat; render the newline as a newline
                Hardline | Newline(_) => {
                    debug_assert!(!info.flat || top == Hardline);
                    f.write_str(consts::NEWLINE)?;
                    size += consts::NEWLINE_LEN;
                    eol += (size + self.line_width as usize);
                    for _ in 0..info.nest {
                        f.write_str(consts::SPACE)?;
                    }
                    size += (info.nest as usize);
                },
                Text(sptr) => {
                    let inner = sptr.read(self.store); 
                    size += inner.len();
                    write!(f, "{}", inner)?
                },
                ZeroWidthText(sptr) => {
                    write!(f, "{}", sptr.read(self.store))?
                }
                Concat { l, r, .. } => {
                    let (l, r) = (l.read(self.store), r.read(self.store));

                    let lhs_dist_next_newline = if r.has_newline().is_some() {
                        r.dist_next_newline(self.store)
                    } else {
                        r.dist_next_newline(self.store) + info.dist_next_newline
                    };

                    stack.push((r, info));
                    stack.push(
                        (l, RenderInfo { dist_next_newline: info.dist_next_newline + lhs_dist_next_newline, ..info })
                    )
                },
                // Increase the nest amt for any later newlines that get printed
                Nest { nest_amt, doc, .. } => stack.push(
                    (doc.read(self.store), RenderInfo { nest: info.nest + nest_amt, ..info })
                ),
                // Explicitly in flatmode
                Group { doc, .. } if info.flat => stack.push(
                    (doc.read(self.store), RenderInfo { flat: true, ..info })
                ),
                // If everything in the doc will fit in the space before `eol`, render in flatmode
                Group { doc, flat_len, .. } if (size + flat_len + info.dist_next_newline <= eol) => {
                    stack.push(
                        (doc.read(self.store), RenderInfo { flat: true, ..info })
                    )
                }
                // No flatmode
                Group { doc, .. } => stack.push(
                    (doc.read(self.store), RenderInfo { flat: false, ..info })
                )
            }
        }
        Ok(())
    }
}

impl<'x, 's : 'x, M> Doc<'s, M> {
    pub fn render<S: HasDocs<'s, M>>(self, line_width : usize, store : &'x S) -> Renderable<'x, 's, M, S> {
        Renderable { doc : self, line_width, store, flat : false }
    }

    pub fn render_flat<S: HasDocs<'s, M>>(self, line_width : usize, store : &'x S) -> Renderable<'x, 's, M, S> {
        Renderable { doc : self, line_width, store, flat : true }
    }
}

impl<'x, 's : 'x, M> DocPtr<'s, M> {
    pub fn render<S: HasDocs<'s, M>>(self, line_width : usize, store : &'x S) -> Renderable<'x, 's, M, S> {
        self.read(store).render(line_width, store)
    }

    pub fn render_flat<S: HasDocs<'s, M>>(self, line_width : usize, store : &'x S) -> Renderable<'x, 's, M, S> {
        self.read(store).render_flat(line_width, store)
    }
}


// Putting string slices (&str) in docs does not cause them to be allocated.
#[test]
fn test_print() {
    let s = &mut DocStore::<()>::default();
    let big_string = String::from("this is a string");
    let _ = intercal!("a", "b", "c", "d"; " ", s);
    let _ = "a static string".alloc_full(s);
    let _ = big_string.as_str().alloc_full(s);
    assert!(s.strings.is_empty());
    println!("{:?}", s);
}   

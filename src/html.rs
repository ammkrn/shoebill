//! Implements typed html tags that are usable with the base pretty printer, and anything
//! that has a pretty printer accessible via `HasDocs`. The tags are indexed by two type parameters,
//! the first is the phantom type used to disambiguate printers, the second is the type of html tag.
//! 
//!
//!```
//! use shoebill::{ DocStore, Doclike };
//! use shoebill::html::{ TypedHtml, Span, Div };
//!
//! // Making a basic chunk of html with the builder pattern.
//! let mut s = &mut DocStore::<()>::default();
//!
//! let inner_tag = TypedHtml::<(), Span>::new()
//!     .w_class("inner1", s)
//!     .w_class("inner2", s)
//!     .w_id("my_span", s)
//!     .w_content("content1", s)
//!     .w_content(" content2", s);
//!
//! let html = TypedHtml::<(), Div>::new()
//!     .w_class("outer1", s)
//!     .w_class("outer2", s)
//!     .w_id("my_div", s)
//!     .w_content(inner_tag, s)
//!     .alloc(s);
//!
//! let target =
//!r#"<div class="outer1 outer2" id="my_div">
//!    <span class="inner1 inner2" id="my_span">
//!        content1 content2
//!    </span>
//!</div>"#;
//!
//! assert_eq!(target, format!("{}", html.render(80, s)));
//!```
use std::marker::PhantomData;
use crate::{ concat_, HasDocs, Doc, DocPtr, Doclike, Doc::* };

pub trait IsTag {
    const TAG: &'static str;
    const OPEN: &'static str;
    const CLOSE: &'static str;
}

macro_rules! mk_html_tags {
    ( $( ( $struct_name:ident, $tag:expr) ),*) => {
        $(
            pub struct $struct_name;
            impl IsTag for $struct_name {
                const TAG: &'static str = $tag;
                const OPEN: &'static str = concat!("<", $tag);
                const CLOSE: &'static str = concat!("</", $tag, ">");
            }
        )*
    };
}

mk_html_tags! {
    (Body, "body"),
    (Span, "span"),
    (Div, "div"),
    (Tr, "tr"),
    (Th, "th"),
    (Td, "td")
}

///```
/// use shoebill::{ DocStore, Doclike };
/// use shoebill::html::{ TypedHtml, Span, Div };
///
/// // Making a basic chunk of html with the builder pattern.
/// let mut s = &mut DocStore::<()>::default();
///
/// let inner_tag = TypedHtml::<(), Span>::new()
///     .w_class("inner1", s)
///     .w_class("inner2", s)
///     .w_id("my_span", s)
///     .w_content("content1", s)
///     .w_content(" content2", s);
///
/// let html = TypedHtml::<(), Div>::new()
///     .w_class("outer1", s)
///     .w_class("outer2", s)
///     .w_id("my_div", s)
///     .w_content(inner_tag, s)
///     .alloc(s);
///
/// let target =
///r#"<div class="outer1 outer2" id="my_div">
///    <span class="inner1 inner2" id="my_span">
///        content1 content2
///    </span>
///</div>"#;
///
/// assert_eq!(target, format!("{}", html.render(80, s)));
///```
///
///```
///use shoebill::{ DocStore, Doclike };
///use shoebill::html::{ TypedHtml, Span, Div };
///
/// // Using `group` to render html flat.
///let mut s = &mut DocStore::<()>::default();
///
///let inner_tag = TypedHtml::<(), Span>::new()
///    .w_class("inner1", s)
///    .w_class("inner2", s)
///    .w_id("my_span", s)
///    .w_content("content1", s)
///    .w_content(" content2", s);
///
///let html = TypedHtml::<(), Div>::new()
///    .w_class("outer1", s)
///    .w_class("outer2", s)
///    .w_id("my_div", s)
///    .w_content(inner_tag, s)
///    .group(true)
///    .alloc(s);
///
///let target = r#"<div class="outer1 outer2" id="my_div"> <span class="inner1 inner2" id="my_span"> content1 content2 </span> </div>"#;
///
///assert_eq!(target, format!("{}", html.render(777, s)));
///```
pub struct TypedHtml<'s, M, A: IsTag> {
    classes: Doc<'s, M>,
    id: Doc<'s, M>,
    content: Doc<'s, M>,
    nest: u16,
    group: bool,
    marker: PhantomData<A>,
}

impl<'s, M, A: IsTag> Doclike<'s, M> for TypedHtml<'s, M, A> {
    fn alloc_full(self, pr: &mut impl HasDocs<'s, M>) -> (Doc<'s, M>, DocPtr<'s, M>) {
        let open = concat_!(
            <A as IsTag>::OPEN,
            " class=\"",
            self.classes,
            "\" id=\"",
            self.id,
            "\">";
            pr
        );
        let out = open.concat_newline(self.content, pr).nest(self.nest, pr).concat_newline(<A as IsTag>::CLOSE, pr);
        if self.group {
            out.group(pr).alloc_full(pr)
        } else {
            out.alloc_full(pr)
        }
    }
}

impl<'s, M, A: IsTag> TypedHtml<'s, M, A> {
    pub fn new() -> Self {
        TypedHtml {
            classes: Nil,
            id: Nil,
            content: Nil,
            nest: 4,
            group: false,
            marker: PhantomData
        }
    }

    /// Controls the degree to which inner tags are nested. 
    pub fn nest(self, nest: u16) -> Self {
        TypedHtml { 
            nest, 
            ..self 
        }
    }

    pub fn group(self, group: bool) -> Self {
        TypedHtml { 
            group, 
            ..self
        }
    }

    pub fn w_class(self, class: impl Doclike<'s, M>, pr: &mut impl HasDocs<'s, M>) -> Self {
        if self.classes == Nil {
            TypedHtml { 
                classes: self.classes.concat(class, pr), 
                ..self 
            }
        } else {
            TypedHtml { 
                classes: self.classes.concat_space(class, pr), 
                ..self
            }
        }
    }

    pub fn w_id(self, id: impl Doclike<'s, M>, pr: &mut impl HasDocs<'s, M>) -> Self {
        TypedHtml { 
            id: id.alloc_full(pr).0, 
            ..self 
        }
    }

    pub fn w_content(self, content: impl Doclike<'s, M>, pr: &mut impl HasDocs<'s, M>) -> Self {
        TypedHtml {
            content: self.content.concat(content, pr),
            ..self
        }
    }
}


//! Types for formatting items using Rust Object Notation (the format you see when
//! you debug format Rust items with the `{:#?}` formatter). They all have the same
//! API, which is `new()`, `add_name()` (which adds a path segment), `add_field()`,
//! `to_doc()` and `render()` which is a shortcut that includes a call to `to_doc()`.
//!
//! They're written in such a way that they remain generic over anything that can
//! be converted into `StrOrDoc`, and you only need to give them the Printer object
//! once, when they're being finished and/or rendered.
//!
//! The big thing that needs to be done on top of this is to make a new enum
//! that recognizes other Ron objects and can format accordingly; IE you want tuples
//! to `group()` their elements, UNLESS they're RonStruct mappings, etc.
use crate::{ compose, Doc::*, Doclike, StrOrDoc, HasPrinter, DocPtr, Renderable };

//use Ron::*;

/*
self note:
What you want is to make the primary type for Ron contents a union of StrOrDoc and 
the unified Ron enum so that you can offer more accurate Ron formatting based 
on ungrouping.
*/


/// A Rust struct or mapping
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RonStruct<'p> {
    name : Vec<StrOrDoc<'p>>,
    fields : Vec<(StrOrDoc<'p>, StrOrDoc<'p>)>
}

impl<'x, 'p : 'x> RonStruct<'p> {
    pub fn new() -> Self {
        RonStruct {
            name : Vec::new(),
            fields : Vec::new()
        }
    }

    pub fn add_name(&mut self, n : impl Into<StrOrDoc<'p>>) {
        self.name = vec![n.into()];
    }

    pub fn add_field(&mut self, k : impl Into<StrOrDoc<'p>>, v : impl Into<StrOrDoc<'p>>) {
        self.fields.push((k.into(), v.into()));
    }

    pub fn to_doc(self, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {
        let nullary = self.fields.is_empty();

        let mut d = if self.name.is_empty() {
            "{".alloc(pr)
        } else {
            self.name.into_iter().enumerate().fold(Nil.alloc(pr), |acc, (idx, next)| {
                if idx == 0 {
                    acc.concat(next, pr)
                } else {
                    compose!(pr ; acc <> "::" <> next)
                }
            }).concat(" { ", pr)
        };

        for (k, v) in self.fields.into_iter() {
            let kv_doc = compose!(pr ; k <> ": " <> v <> ", ");
            d = compose!(pr ; d <z> kv_doc);
        }

        d = d.nest(4, pr);

        if nullary {
            d.concat("}", pr)
        } else {
            d.nest_doc_zero("}", 0, pr)
        }
    }

    pub fn render<P : HasPrinter<'p>>(self, line_width : u16, pr : &'x mut P) -> Renderable<'x, 'p, P> {
        let d = self.to_doc(pr);
        d.render(line_width, pr)
    }
}
    
/// A tuple (also tuple structs and enums). A RonTuple with a name but no fields
/// will be interpreted as a nullary tuple struct/enum, so printed as just
/// the path-segments comprising the name. A nameless tuple with no fields will still
/// be printed as `()`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RonTuple<'p> {
    name : Vec<StrOrDoc<'p>>,
    fields : Vec<StrOrDoc<'p>>,
}

impl<'x, 'p : 'x, A, B> From<(A, B)> for RonTuple<'p>
where 
    A : Into<StrOrDoc<'p>>,
    B : Into<StrOrDoc<'p>> {
    fn from(t : (A, B)) -> RonTuple<'p> {
        let mut ron = RonTuple::new();
        ron.add_field(t.0.into());
        ron.add_field(t.1.into());
        ron
    }
}

impl<'x, 'p : 'x, A, B, C> From<(A, B, C)> for RonTuple<'p>
where 
    A : Into<StrOrDoc<'p>>,
    B : Into<StrOrDoc<'p>>, 
    C : Into<StrOrDoc<'p>> {
    fn from(t : (A, B, C)) -> RonTuple<'p> {
        let mut ron = RonTuple::new();
        ron.add_field(t.0.into());
        ron.add_field(t.1.into());
        ron.add_field(t.2.into());
        ron
    }
}

impl<'x, 'p : 'x> RonTuple<'p> {
    pub fn new() -> Self {
        RonTuple {
            name : Vec::new(),
            fields : Vec::new()
        }
    }

    pub fn add_name(&mut self, n : impl Into<StrOrDoc<'p>>) {
        self.name.push(n.into());
    }

    pub fn add_field(&mut self, v : impl Into<StrOrDoc<'p>>) {
        self.fields.push(v.into());
    }

    pub fn to_doc(self, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {
        let unnamed = self.name.is_empty();
        let nullary = self.fields.is_empty();

        if unnamed && nullary {
            return "()".alloc(pr)
        }

        let mut d = if unnamed {
            "(".alloc(pr)
        } else {
            let folded = self.name.into_iter().enumerate().fold(Nil.alloc(pr), |acc, (idx, next)| {
                if idx == 0 {
                    acc.concat(next, pr)
                } else {
                    compose!(pr ; acc <> "::" <> next)
                }
            });

            if nullary {
                folded
            } else {
                folded.concat("(", pr)
            }
        };

        for v in self.fields.into_iter() {
            let v_doc = v.concat(", ", pr);
            d = compose!(pr ; d <z> v_doc);
        }

        d = d.nest(4, pr);

        if nullary {
            d
        } else {
            d.nest_doc_zero(")", 0, pr)
        }
    }

    pub fn render<P : HasPrinter<'p>>(self, line_width : u16, pr : &'x mut P) -> Renderable<'x, 'p, P> {
        let d = self.to_doc(pr);
        d.render(line_width, pr)
    }
}

/// A sequence (slice, array, vec, etc)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RonSequence<'p> {
    name : Option<StrOrDoc<'p>>,
    vals : Vec<StrOrDoc<'p>>,
}

impl<'x, 'p : 'x, A> From<&'p [A]> for RonSequence<'p> 
where &'p A : Into<StrOrDoc<'p>> {
    fn from(sl : &'p [A]) -> RonSequence<'p> {
        let mut ron = RonSequence::new();
        for elem in sl {
            ron.add_field(elem.into());
        }
        ron
    }
}

impl<'x, 'p : 'x> RonSequence<'p> {
    pub fn new() -> Self {
        RonSequence {
            name : None,
            vals : Vec::new()
        }
    }

    pub fn add_name(&mut self, n : impl Into<StrOrDoc<'p>>) {
        self.name = Some(n.into());
    }

    // I know that sequences don't have 'fields' technically, but it seemed
    // stupid to have an otherwise identical API be different here just because.
    pub fn add_field(&mut self, v : impl Into<StrOrDoc<'p>>) {
        self.vals.push(v.into());
    }

    pub fn to_doc(self, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {
        let nullary = self.vals.is_empty();
        let mut d = match self.name {
            None => "[".alloc(pr),
            Some(n) => n.concat(" [", pr)
        };

        for v in self.vals {
            let v_doc = v.concat(",", pr);
            d = compose!(pr ; d <n> v_doc);
        }
        d = d.nest(4, pr);

        if nullary {
            d.concat("]", pr)
        } else {
            d.nest_doc("]", 0, pr)
        }
    }

    pub fn render<P : HasPrinter<'p>>(self, line_width : u16, pr : &'x mut P) -> Renderable<'x, 'p, P> {
        let d = self.to_doc(pr);
        d.render(line_width, pr)
    }
}

/// Formats some Option<T>
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RonOption<'p> {
    name : Option<StrOrDoc<'p>>,
    inner : Option<StrOrDoc<'p>>,
}

impl<'x, 'p : 'x, A> From<Option<A>> for RonOption<'p>
where A : Into<StrOrDoc<'p>> {
    fn from(inner : Option<A>) -> RonOption<'p> {
        RonOption::new(inner)
    }
}

impl<'x, 'p : 'x> RonOption<'p> {
    pub fn new(inner : Option<impl Into<StrOrDoc<'p>>>) -> Self {
        RonOption {
            name : None,
            inner : inner.map(|x| x.into())
        }
    }

    pub fn add_name(&mut self, n : impl Into<StrOrDoc<'p>>) {
        self.name = Some(n.into());
    }

    pub fn to_doc(self, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {

        match (self.name, self.inner) {
            (None, None) => "None".alloc(pr),
            (Some(n), None) => n.concat(": None", pr),
            (None, Some(x)) => {
                "Some(".alloc(pr)
                .nest_doc_zero(x, 4, pr)
                .nest_doc_zero(")", 0, pr)
                //.group(pr)
            }
            (Some(n), Some(x)) => {
                n.concat(": Some(", pr)
                .nest_doc_zero(x, 4, pr)
                .nest_doc_zero(")", 0, pr)
                //.group(pr)
            }
        }
    }

    pub fn render<P : HasPrinter<'p>>(self, line_width : u16, pr : &'x mut P) -> Renderable<'x, 'p, P> {
        let d = self.to_doc(pr);
        d.render(line_width, pr)
    }
}

/// Formats some Result<A, E> where both A and E implement `Into<StrOrDoc<'p>>`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RonResult<'p> {
    name : Option<StrOrDoc<'p>>,
    inner : StrOrDoc<'p>,
    is_ok : bool,
}

impl<'x, 'p : 'x, A, E> From<Result<A, E>> for RonResult<'p>
where 
    A : Into<StrOrDoc<'p>>,
    E : Into<StrOrDoc<'p>> {
    fn from(r : Result<A, E>) -> RonResult<'p> {
        RonResult::new(r)
    }
}

impl<'x, 'p : 'x> RonResult<'p> {
    pub fn new(inner : Result<impl Into<StrOrDoc<'p>>, impl Into<StrOrDoc<'p>>>) -> Self {
        let (is_ok, inner) = match inner {
            Ok(x) => (true, x.into()),
            Err(e) => (false, e.into())
        };

        RonResult {
            name : None,
            inner,
            is_ok
        }
    }

    pub fn add_name(&mut self, n : impl Into<StrOrDoc<'p>>) {
        self.name = Some(n.into());
    }

    pub fn to_doc(self, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {
        let mut d = match (self.name, self.is_ok) {
            (None, true) => "Ok(".alloc(pr),
            (None, false) => "Err(".alloc(pr),
            (Some(n), true) => n.concat(": Ok(", pr),
            (Some(n), false) => n.concat(": Err(", pr)
        };
        d = d.nest_doc_zero(self.inner, 4, pr);
        d.nest_doc_zero(")", 0, pr) 
        //.group(pr)
    }

    pub fn render<P : HasPrinter<'p>>(self, line_width : u16, pr : &'x mut P) -> Renderable<'x, 'p, P> {
        let d = self.to_doc(pr);
        d.render(line_width, pr)
    }
}

// This isn't used yet.
//#[derive(Debug, Clone, PartialEq, Eq)]
//pub enum Ron<'p> {
//    Struct(RonStruct<'p>),
//    Tuple(RonTuple<'p>),
//    Sequence(RonSequence<'p>),
//    Res(RonResult<'p>),
//    Opt(RonOption<'p>),
//}

//impl<'p> From<RonStruct<'p>> for Ron<'p> {
//    fn from(s : RonStruct<'p>) -> Ron<'p> {
//        Struct(s)
//    }
//}

//impl<'p> Ron<'p> {

//}

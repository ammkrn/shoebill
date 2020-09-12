//! Types for formatting items using Rust Object Notation (the format you see when
//! you debug format Rust items with the `{:#?}` formatter). 
//!
//! They all have the same API, which is `new()`, `add_name()` 
//! (which adds a path segment), `add_field()`, `to_doc()` and `render()` 
//! which is a shortcut that includes a call to `to_doc()`.
//!
//! They're written in such a way that they remain generic over anything that can
//! be converted into `StrOrDoc`, and you only need to give them the Printer object
//! once, when they're being finished and/or rendered.
//!
//! The big thing that needs to be done on top of this is to make a new enum
//! that recognizes other Ron objects and can format accordingly; IE you want tuples
//! to `group()` their elements, UNLESS they're RonStruct mappings, etc.
use crate::{ compose, Doc::*, Doclike, StrOrDoc, HasPrinter, DocPtr, Renderable };

pub(crate) fn mk_path_name<'p>(segs : Vec<StrOrDoc<'p>>, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {
    segs.into_iter().enumerate().fold(Nil.alloc(pr), |acc, (idx, next)| {
        if idx == 0 {
            acc.concat(next, pr)
        } else {
            compose!(pr ; acc <> "::" <> next)
        }
    })
}

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
        self.name.push(n.into());
    }

    pub fn add_field(&mut self, k : impl Into<StrOrDoc<'p>>, v : impl Into<StrOrDoc<'p>>) {
        self.fields.push((k.into(), v.into()));
    }

    pub fn to_doc(self, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {
        let name = mk_path_name(self.name, pr);

        if self.fields.is_empty() {
            return name
        } else {
            let mut d = match name.read(pr) {
                Nil => name.concat("{", pr),
                _ => name.concat(" {", pr)
            };

            for (k, v) in self.fields.into_iter() {
                let kv_doc = compose!(pr ; k <> ": " <> v <> ",");
                d = compose!(pr ; d <n> kv_doc);
            }

            d = d.nest(4, pr);
            d.nest_doc("}", 0, pr)
        }
    }

    pub fn render<P : HasPrinter<'p>>(self, line_width : u32, pr : &'x mut P) -> Renderable<'x, 'p, P> {
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

        // If it's unnamed and has no fields, it should be rendered as `()`
        if unnamed && nullary {
            return "()".alloc(pr)
        }

        let mut d = mk_path_name(self.name, pr);

        if !nullary { d = d.concat("(", pr); }

        for (idx, v) in self.fields.into_iter().enumerate() {
            let v_doc = v.concat(",", pr);
            if idx == 0 {
               d = compose!(pr ; d <z> v_doc);
            } else {
               d = compose!(pr ; d <n> v_doc);
            }
        }

        d = d.nest(4, pr);

        if nullary {
            d
        } else {
            d.nest_doc_zero(")", 0, pr)
        }
    }

    pub fn render<P : HasPrinter<'p>>(self, line_width : u32, pr : &'x mut P) -> Renderable<'x, 'p, P> {
        let d = self.to_doc(pr);
        d.render(line_width, pr)
    }
}

/// A sequence (slice, array, vec, etc)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RonSequence<'p> {
    name : Vec<StrOrDoc<'p>>,
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
            name : Vec::new(),
            vals : Vec::new()
        }
    }

    pub fn add_name(&mut self, n : impl Into<StrOrDoc<'p>>) {
        self.name.push(n.into());
    }

    // I know that sequences don't have 'fields' technically, but it seemed
    // stupid to have an otherwise identical API be different here just because.
    pub fn add_field(&mut self, v : impl Into<StrOrDoc<'p>>) {
        self.vals.push(v.into());
    }

    pub fn to_doc(self, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {
        let nullary = self.vals.is_empty();
        let mut d = match self.name.is_empty() {
            true => "[".alloc(pr),
            false => mk_path_name(self.name, pr).concat(" [", pr)
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

    pub fn render<P : HasPrinter<'p>>(self, line_width : u32, pr : &'x mut P) -> Renderable<'x, 'p, P> {
        let d = self.to_doc(pr);
        d.render(line_width, pr)
    }
}

/// Formats some Option<T>
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RonOption<'p> {
    name : Vec<StrOrDoc<'p>>,
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
            name : Vec::new(),
            inner : inner.map(|x| x.into())
        }
    }

    pub fn add_name(&mut self, n : impl Into<StrOrDoc<'p>>) {
        self.name.push(n.into());
    }

    pub fn to_doc(self, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {

        match (self.name.is_empty(), self.inner) {
            (true, None) => "None".alloc(pr),
            (false, None) => mk_path_name(self.name, pr).concat(": None", pr),
            (true, Some(x)) => {
                "Some(".alloc(pr)
                .nest_doc_zero(x, 4, pr)
                .nest_doc_zero(")", 0, pr)
                //.group(pr)
            }
            (false, Some(x)) => {
                mk_path_name(self.name, pr)
                .concat(": Some(", pr)
                .nest_doc_zero(x, 4, pr)
                .nest_doc_zero(")", 0, pr)
                //.group(pr)
            }
        }
    }

    pub fn render<P : HasPrinter<'p>>(self, line_width : u32, pr : &'x mut P) -> Renderable<'x, 'p, P> {
        let d = self.to_doc(pr);
        d.render(line_width, pr)
    }
}

/// Formats some Result<A, E> where both A and E implement `Into<StrOrDoc<'p>>`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RonResult<'p> {
    name : Vec<StrOrDoc<'p>>,
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
            name : Vec::new(),
            inner,
            is_ok
        }
    }

    pub fn add_name(&mut self, n : impl Into<StrOrDoc<'p>>) {
        self.name.push(n.into());
    }

    pub fn to_doc(self, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {
        let mut d = match (self.name.is_empty(), self.is_ok) {
            (true, true) => "Ok(".alloc(pr),
            (true, false) => "Err(".alloc(pr),
            (false, true) => mk_path_name(self.name, pr).concat(": Ok(", pr),
            (false, false) => mk_path_name(self.name, pr).concat(": Err(", pr)
        };
        d = d.nest_doc_zero(self.inner, 4, pr);
        d.nest_doc_zero(")", 0, pr) 
        //.group(pr)
    }

    pub fn render<P : HasPrinter<'p>>(self, line_width : u32, pr : &'x mut P) -> Renderable<'x, 'p, P> {
        let d = self.to_doc(pr);
        d.render(line_width, pr)
    }
}

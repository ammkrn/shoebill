use shoebill::{
    compose,
    Printer,
    HasPrinter,
    Doclike,
    brackets::{ 
        Brackets,
        Bracketable
    },
};
use AExp::*;

// Example showing how the built-in bracketing functionality is supposed to work, with
// the interesting part being the function `AExp::pp()`.
// As mentioned in the bracket module, the complexity of bracketing comes from the fact
// that it's extremely likely that we don't want to change our types and logic
// just to accommodate printing, meaning we'll need both the caller and the callee
// to be able to have a say in whether a given element is bracketed. 
#[derive(Debug, Clone)]
pub enum AExp {
    Zero,
    Succ(Box<AExp>),
    Max(Box<AExp>, Box<AExp>),
    Var(&'static str),
}

impl AExp {
    pub fn zero() -> Self {
        Zero
    }

    pub fn succ(&self) -> Self {
        Succ(Box::new(self.clone()))
    }

    pub fn max(&self, other : &Self) -> Self {
        Max(Box::new(self.clone()), Box::new(other.clone()))
    }

    pub fn var(s : &'static str) -> Self {
        Var(s)
    }

    pub fn to_offset(&self) -> (i32, Option<AExp>) {
        match self {
            Succ(a) => {
                let (n, inner) = a.to_offset();
                (1 + n, inner)
            },
            Zero => (0, None),
            _ => (0, Some(self.clone()))
        }
    }

    // Because this is a simple example, we could technically achieve this
    // with just bracket_always and bracket_never, but that seems like less of an example.
    pub fn pp<'p>(&self, pr : &mut impl HasPrinter<'p>) -> Brackets<'p> {
        match self {
            Zero => "0".alloc(pr).bracket_never(),
            Succ(..) => match self.to_offset() {
                (n, None) => n.to_string().bracket(1),
                (n, Some(rest)) => {
                    compose!(pr; (n.to_string()) <> "+" <> (rest.pp(pr).paren_upto(0, pr)))
                    .bracket(0)
                },
            }
            Max(l, r) => {
                let l = l.pp(pr).paren_upto(0, pr);
                let r = r.pp(pr).paren_upto(0, pr);
                compose!(pr ; "max" <s> l <s> r).bracket(0)
            },
            Var(c) => c.alloc(pr).bracket_never(),
        }
    }
}


fn main() {
    let mut store = Printer::new();
    let z = AExp::zero();
    let v = AExp::Var("z");
    let v1 = v.succ();
    let v2 = v1.succ();
    let _1 = z.succ();
    let _2 = _1.clone().succ();
    let _3 = _2.clone().succ();
    let m = AExp::max(&v2, &_2);
    let m = m.max(&v2);
    let m = _3.max(&v2).max(&m);

    println!("\n{}\n", _2.pp(&mut store).render(80, &mut store));
    println!("\n{}\n", v2.pp(&mut store).wo_brackets(&mut store).render(80, &store));
    println!("\n{}\n", m.pp(&mut store).render(80, &mut store));
}

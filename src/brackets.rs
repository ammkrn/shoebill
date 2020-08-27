use crate::concat;
use crate::Doclike;
use crate::DocPtr;
use crate::StrOrDoc;
use crate::HasPrinter;
use crate::Renderable;

use BracketPrio::*;

/// A mechanism for managing bracketing during pretty-printing based on an implementation I saw
/// Gabriel Ebner use. It feels complicated, but it makes sense once you get going, and there's
/// no obvious solution that's better while preserving the generality this offers.
/// The general source of the complexity is that you want both the caller and the callee
/// to have control over whether or not something gets bracketed.
/// There's a working example in the `pp` function of /examples/aexp
/// The idea is that the pretty printing implementation for some type you have is a recursive
/// (or faux-recursive) function that returns elements wrapped in this Parenable type, and have
/// an associated number which lets the functions waiting on their return know whether or not
/// they should wrap the value they get in brackets.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Brackets<'p> {
    pub doc : StrOrDoc<'p>,
    pub prio : BracketPrio,
}


//trait HasPrettyDisplay {
//}
//trait HasPrettyDebug {
//}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BracketPrio {
    Always,
    Never,
    Num(u16)
}

// The point of this is so that .bracket() doesn't require users to pass
// an allocator, and to make it more generic. We don't want to implement these methods
// in `Doclike` since we don't want users to be able to do it directly for `Doc` elements.
pub trait Bracketable<'p> : Sized {
    fn bracket(self, prio : u16) -> Brackets<'p>;
    fn bracket_never(self) -> Brackets<'p> {
        let mut b = self.bracket(0);
        b.prio = Never;
        b
    }
    fn bracket_always(self) -> Brackets<'p> {
        let mut b = self.bracket(0);
        b.prio = Always;
        b
    }
}

impl<'p, A> Bracketable<'p> for A
where StrOrDoc<'p> : From<A> {
    fn bracket(self, prio : u16) -> Brackets<'p> {
        Brackets {
            doc : self.into(),
            prio : Num(prio),
        }
    }
}

impl<'x, 'p : 'x> Brackets<'p> {

    pub fn paren_upto(self, upto : u16, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {
        self.bracket_upto(upto, "(", ")", pr)
    }

    pub fn squre_upto(self, upto : u16, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {
        self.bracket_upto(upto, "[", "]", pr)
    }

    pub fn wave_upto(self, upto : u16, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {
        self.bracket_upto(upto, "{", "}", pr)
    }

    pub fn bracket_upto(
        self, 
        upto : u16, 
        open : &'p str, 
        close : &'p str, 
        pr : &mut impl HasPrinter<'p>
    ) -> DocPtr<'p> {
        match self.prio {
            Never => self.doc.alloc(pr),
            Always => concat!([open, self.doc, close], pr),
            Num(n) => {
                if n <= upto {
                    concat!([open, self.doc, close], pr)
                } else {
                    self.doc.alloc(pr)
                }
            }
        }
    }

    /// Do not add brackets to this item, disregarding its priority. We want the caller to have
    /// the last word, so this will override an inner priority of `BracketPrio::Always`.
    pub fn wo_brackets(self, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {
        self.doc.alloc(pr)
    }

    pub fn w_parens(self, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {
        self.w_brackets("(", ")", pr)
    }

    pub fn w_square(self, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {
        self.w_brackets("[", "]", pr)
    }

    pub fn w_wave(self, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {
        self.w_brackets("{", "}", pr)
    }

    /// Add brackets to this item, disregarding its assigned priority. We want the caller to have
    /// the last word, so this will override an inner priority of `BracketPrio::Never`.
    pub fn w_brackets(
        self, 
        open : &'p str,
        close : &'p str,
        pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {
        concat!([open, self.doc, close], pr)
    }

    /// A convenience function for rendering the 'last' item; this ignores/discards the given
    /// item's priority and does not add brackets, since it's more likely than not that the final
    /// expression isn't meant to be bracketed.
    pub fn render<P>(self, line_width : u16, pr : &'x mut P) -> Renderable<'x, 'p, P> 
    where P : HasPrinter<'p> {
        self.doc.alloc(pr).render(line_width, pr)
    }

}

//! Tools for managing bracketing during pretty-printing based.
//! 
//! Based on an implementation I saw Gabriel Ebner use that feels complicated, 
//! but it makes sense once you get going, and there's
//! no obvious solution that's better while preserving the generality this offers.
//! The general source of the complexity is that you want both the caller and the callee
//! to have control over whether or not something gets bracketed.
//! There's a working example in the `pp` function of /examples/aexp
//! The idea is that the pretty printing implementation for some type you have is a recursive
//! (or faux-recursive) function that returns elements wrapped in this Parenable type, and have
//! an associated number which lets the functions waiting on their return know whether or not
//! they should wrap the value they get in brackets.
use crate::concat_;
use crate::Doclike;
use crate::Doc;
use crate::HasDocs;
use crate::Renderable;

use Level::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Bracket<'s, M> {
    pub doc : Doc<'s, M>,
    pub level : Level,
}

impl<'s, M> Doc<'s, M> {
    pub fn level(self, level: u16) -> Bracket<'s, M> {
        Bracket {
            doc: self,
            level: Num(level)
        }
    }

    pub fn never(self) -> Bracket<'s, M> {
        Bracket {
            doc: self,
            level: Never
        }
    }
    pub fn always(self) -> Bracket<'s, M> {
        Bracket {
            doc: self,
            level: Always
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Level {
    Never,
    Num(u16),
    Always,
}

impl<'x, 's : 'x, M> Bracket<'s, M> {

    pub fn paren_upto(self, upto : u16, pr : &mut impl HasDocs<'s, M>) -> Doc<'s, M> {
        self.bracket_upto(upto, "(", ")", pr)
    }

    pub fn square_upto(self, upto : u16, pr : &mut impl HasDocs<'s, M>) -> Doc<'s, M> {
        self.bracket_upto(upto, "[", "]", pr)
    }

    pub fn wave_upto(self, upto : u16, pr : &mut impl HasDocs<'s, M>) -> Doc<'s, M> {
        self.bracket_upto(upto, "{", "}", pr)
    }

    pub fn bracket_upto(
        self, 
        upto : u16, 
        open : &'s str, 
        close : &'s str, 
        pr : &mut impl HasDocs<'s, M>
    ) -> Doc<'s, M> {
        match self.level {
            Never => self.doc,
            Always => concat_!(open, self.doc, close; pr),
            Num(n) if n <= upto => concat_!(open, self.doc, close; pr),
            _ => self.doc
        }
    }

    /// Do not add brackets to this item, disregarding its level. We want the caller to have
    /// the last word, so this will override an inner level of `Level::Always`.
    pub fn wo_bracket(self) -> Doc<'s, M> {
        self.doc
    }

    pub fn w_paren(self, pr : &mut impl HasDocs<'s, M>) -> Doc<'s, M> {
        self.w_bracket("(", ")", pr)
    }

    pub fn w_square(self, pr : &mut impl HasDocs<'s, M>) -> Doc<'s, M> {
        self.w_bracket("[", "]", pr)
    }

    pub fn w_wave(self, pr : &mut impl HasDocs<'s, M>) -> Doc<'s, M> {
        self.w_bracket("{", "}", pr)
    }

    /// Add brackets to this item, disregarding its assigned level. We want the caller to have
    /// the last word, so this will override an inner level of `Level::Never`.
    pub fn w_bracket(
        self, 
        open : &'s str,
        close : &'s str,
        pr : &mut impl HasDocs<'s, M>) -> Doc<'s, M> {
        concat_!(open, self.doc, close; pr)
    }

    /// A convenience function for rendering the 'last' item; this ignores/discards the given
    /// item's level and does not add brackets, since it's more likely than not that the final
    /// expression isn't meant to be bracketed.
    pub fn render<P: HasDocs<'s, M>>(self, line_width : usize, pr : &'x mut P) -> Renderable<'x, 's, M, P> {
        self.doc.alloc(pr).render(line_width, pr)
    }
}


//! A bunch of implementations for standard traits that need to be done by hand 
//! (they're copy-pasted from the macro output with the necessary bounds on `M` removed)
//! since the derive macro places unnecessary bounds on `M` even though it's only ever 
//! used as phantom data.
use std::marker::PhantomData;
use crate::{ StringPtr, Doc, DocPtr, Doc::* };

impl<'s, M> Clone for Doc<'s, M> {
    fn clone(&self) -> Self {
        match self {
            Nil => Nil,
            Hardline => Hardline,
            Newline(alt) => Newline(*alt),
            Text(s) => Text(*s),
            ZeroWidthText(s) => ZeroWidthText(*s),
            Concat { l, r, flat_len, has_newline, dist_next_newline } => {
                Concat { 
                    l: *l, 
                    r: *r, 
                    flat_len: *flat_len, 
                    has_newline: *has_newline, 
                    dist_next_newline: *dist_next_newline 
                } 
            }
            Nest { doc, nest_amt, flat_len, has_newline, dist_next_newline } => {
                Nest { 
                    doc: *doc, 
                    nest_amt: *nest_amt, 
                    flat_len: *flat_len, 
                    has_newline: *has_newline, 
                    dist_next_newline: *dist_next_newline 
                } 
            }
            Group { doc, flat_len, has_newline, dist_next_newline } => {
                Group { 
                    doc: *doc, 
                    flat_len: *flat_len, 
                    has_newline: *has_newline, 
                    dist_next_newline: *dist_next_newline
                }
            }
        }
    }
}


impl<'s, M> Copy for Doc<'s, M> { }


impl<'s, M> Copy for DocPtr<'s, M> { }

impl<'s, M> Clone for DocPtr<'s, M> {
    fn clone(&self) -> Self {
        DocPtr(self.0, PhantomData)
    }
}

impl<'s, M> Clone for StringPtr<'s, M> {
    fn clone(&self) -> Self {
        match self {
            StringPtr::Idx(ref idx, ref ph) => StringPtr::Idx(*idx, *ph),
            StringPtr::Str(s) => StringPtr::Str(s),
        }
    }
}

impl<'s, M> Copy for StringPtr<'s, M> { }

impl<'s, M> ::core::hash::Hash for StringPtr<'s, M> {
    fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
        match *self {
            StringPtr::Idx(ref idx, _) => {
                ::core::hash::Hash::hash(&std::mem::discriminant(self), state);
                ::core::hash::Hash::hash(&(*idx), state);
            }
            StringPtr::Str(s) => {
                ::core::hash::Hash::hash(&std::mem::discriminant(self), state);
                ::core::hash::Hash::hash(s, state);
            }
        }
    }
}

impl<'s, M> ::core::cmp::PartialEq for StringPtr<'s, M> {
    #[inline]
    fn eq(&self, other: &StringPtr<'s, M>) -> bool {
        match (self, other) {
            (StringPtr::Idx(i1, _), StringPtr::Idx(i2, _)) => i1 == i2,
            (StringPtr::Str(s1), StringPtr::Str(s2)) => s1 == s2,
            _ => false
        }
    }

    #[inline]
    fn ne(&self, other: &StringPtr<'s, M>) -> bool {
        match (self, other) {
            (StringPtr::Idx(i1, _), StringPtr::Idx(i2, _)) => i1 != i2,
            (StringPtr::Str(s1), StringPtr::Str(s2)) => s1 != s2,
            _ => true
        }
    }
}

impl<'s, M> ::core::cmp::Eq for StringPtr<'s, M> { }


impl<'s, M> ::core::cmp::PartialEq for Doc<'s, M> {
    #[inline]
    fn eq(&self, other: &Doc<'s, M>) -> bool {
        {
            let __self_vi = std::mem::discriminant(&*self);
            let __arg_1_vi = std::mem::discriminant(&*other);
            if true && __self_vi == __arg_1_vi {
                match (&*self, &*other) {
                    (&Doc::Newline(ref __self_0), &Doc::Newline(ref __arg_1_0)) => {
                        (*__self_0) == (*__arg_1_0)
                    }
                    (&Doc::Text(ref __self_0), &Doc::Text(ref __arg_1_0)) => {
                        (*__self_0) == (*__arg_1_0)
                    }
                    (&Doc::ZeroWidthText(ref __self_0), &Doc::ZeroWidthText(ref __arg_1_0)) => {
                        (*__self_0) == (*__arg_1_0)
                    }
                    (
                        &Doc::Concat {
                            l: ref __self_0,
                            r: ref __self_1,
                            flat_len: ref __self_2,
                            has_newline: ref __self_3,
                            dist_next_newline: ref __self_4,
                        },
                        &Doc::Concat {
                            l: ref __arg_1_0,
                            r: ref __arg_1_1,
                            flat_len: ref __arg_1_2,
                            has_newline: ref __arg_1_3,
                            dist_next_newline: ref __arg_1_4,
                        },
                    ) => {
                        (*__self_0) == (*__arg_1_0)
                            && (*__self_1) == (*__arg_1_1)
                            && (*__self_2) == (*__arg_1_2)
                            && (*__self_3) == (*__arg_1_3)
                            && (*__self_4) == (*__arg_1_4)
                    }
                    (
                        &Doc::Nest {
                            doc: ref __self_0,
                            nest_amt: ref __self_1,
                            flat_len: ref __self_2,
                            has_newline: ref __self_3,
                            dist_next_newline: ref __self_4,
                        },
                        &Doc::Nest {
                            doc: ref __arg_1_0,
                            nest_amt: ref __arg_1_1,
                            flat_len: ref __arg_1_2,
                            has_newline: ref __arg_1_3,
                            dist_next_newline: ref __arg_1_4,
                        },
                    ) => {
                        (*__self_0) == (*__arg_1_0)
                            && (*__self_1) == (*__arg_1_1)
                            && (*__self_2) == (*__arg_1_2)
                            && (*__self_3) == (*__arg_1_3)
                            && (*__self_4) == (*__arg_1_4)
                    }
                    (
                        &Doc::Group {
                            doc: ref __self_0,
                            flat_len: ref __self_1,
                            has_newline: ref __self_2,
                            dist_next_newline: ref __self_3,
                        },
                        &Doc::Group {
                            doc: ref __arg_1_0,
                            flat_len: ref __arg_1_1,
                            has_newline: ref __arg_1_2,
                            dist_next_newline: ref __arg_1_3,
                        },
                    ) => {
                        (*__self_0) == (*__arg_1_0)
                            && (*__self_1) == (*__arg_1_1)
                            && (*__self_2) == (*__arg_1_2)
                            && (*__self_3) == (*__arg_1_3)
                    }
                    _ => true,
                }
            } else {
                false
            }
        }
    }
    #[inline]
    fn ne(&self, other: &Doc<'s, M>) -> bool {
        {
            let __self_vi = std::mem::discriminant(&*self);
            let __arg_1_vi = std::mem::discriminant(&*other);
            if true && __self_vi == __arg_1_vi {
                match (&*self, &*other) {
                    (&Doc::Newline(ref __self_0), &Doc::Newline(ref __arg_1_0)) => {
                        (*__self_0) != (*__arg_1_0)
                    }
                    (&Doc::Text(ref __self_0), &Doc::Text(ref __arg_1_0)) => {
                        (*__self_0) != (*__arg_1_0)
                    }
                    (&Doc::ZeroWidthText(ref __self_0), &Doc::ZeroWidthText(ref __arg_1_0)) => {
                        (*__self_0) != (*__arg_1_0)
                    }
                    (
                        &Doc::Concat {
                            l: ref __self_0,
                            r: ref __self_1,
                            flat_len: ref __self_2,
                            has_newline: ref __self_3,
                            dist_next_newline: ref __self_4,
                        },
                        &Doc::Concat {
                            l: ref __arg_1_0,
                            r: ref __arg_1_1,
                            flat_len: ref __arg_1_2,
                            has_newline: ref __arg_1_3,
                            dist_next_newline: ref __arg_1_4,
                        },
                    ) => {
                        (*__self_0) != (*__arg_1_0)
                            || (*__self_1) != (*__arg_1_1)
                            || (*__self_2) != (*__arg_1_2)
                            || (*__self_3) != (*__arg_1_3)
                            || (*__self_4) != (*__arg_1_4)
                    }
                    (
                        &Doc::Nest {
                            doc: ref __self_0,
                            nest_amt: ref __self_1,
                            flat_len: ref __self_2,
                            has_newline: ref __self_3,
                            dist_next_newline: ref __self_4,
                        },
                        &Doc::Nest {
                            doc: ref __arg_1_0,
                            nest_amt: ref __arg_1_1,
                            flat_len: ref __arg_1_2,
                            has_newline: ref __arg_1_3,
                            dist_next_newline: ref __arg_1_4,
                        },
                    ) => {
                        (*__self_0) != (*__arg_1_0)
                            || (*__self_1) != (*__arg_1_1)
                            || (*__self_2) != (*__arg_1_2)
                            || (*__self_3) != (*__arg_1_3)
                            || (*__self_4) != (*__arg_1_4)
                    }
                    (
                        &Doc::Group {
                            doc: ref __self_0,
                            flat_len: ref __self_1,
                            has_newline: ref __self_2,
                            dist_next_newline: ref __self_3,
                        },
                        &Doc::Group {
                            doc: ref __arg_1_0,
                            flat_len: ref __arg_1_1,
                            has_newline: ref __arg_1_2,
                            dist_next_newline: ref __arg_1_3,
                        },
                    ) => {
                        (*__self_0) != (*__arg_1_0)
                            || (*__self_1) != (*__arg_1_1)
                            || (*__self_2) != (*__arg_1_2)
                            || (*__self_3) != (*__arg_1_3)
                    }
                    _ => false,
                }
            } else {
                true
            }
        }
    }
}

impl<'s, M> ::core::cmp::Eq for Doc<'s, M> { }

#[automatically_derived]
#[allow(unused_qualifications)]
impl<'s, M> ::core::hash::Hash for Doc<'s, M> {
    fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
        match (&*self,) {
            (&Doc::Newline(ref __self_0),) => {
                ::core::hash::Hash::hash(&std::mem::discriminant(self), state);
                ::core::hash::Hash::hash(&(*__self_0), state)
            }
            (&Doc::Text(ref __self_0),) => {
                ::core::hash::Hash::hash(&std::mem::discriminant(self), state);
                ::core::hash::Hash::hash(&(*__self_0), state)
            }
            (&Doc::ZeroWidthText(ref __self_0),) => {
                ::core::hash::Hash::hash(&std::mem::discriminant(self), state);
                ::core::hash::Hash::hash(&(*__self_0), state)
            }
            (&Doc::Concat {
                l: ref __self_0,
                r: ref __self_1,
                flat_len: ref __self_2,
                has_newline: ref __self_3,
                dist_next_newline: ref __self_4,
            },) => {
                ::core::hash::Hash::hash(&std::mem::discriminant(self), state);
                ::core::hash::Hash::hash(&(*__self_0), state);
                ::core::hash::Hash::hash(&(*__self_1), state);
                ::core::hash::Hash::hash(&(*__self_2), state);
                ::core::hash::Hash::hash(&(*__self_3), state);
                ::core::hash::Hash::hash(&(*__self_4), state)
            }
            (&Doc::Nest {
                doc: ref __self_0,
                nest_amt: ref __self_1,
                flat_len: ref __self_2,
                has_newline: ref __self_3,
                dist_next_newline: ref __self_4,
            },) => {
                ::core::hash::Hash::hash(&std::mem::discriminant(self), state);
                ::core::hash::Hash::hash(&(*__self_0), state);
                ::core::hash::Hash::hash(&(*__self_1), state);
                ::core::hash::Hash::hash(&(*__self_2), state);
                ::core::hash::Hash::hash(&(*__self_3), state);
                ::core::hash::Hash::hash(&(*__self_4), state)
            }
            (&Doc::Group {
                doc: ref __self_0,
                flat_len: ref __self_1,
                has_newline: ref __self_2,
                dist_next_newline: ref __self_3,
            },) => {
                ::core::hash::Hash::hash(&std::mem::discriminant(self), state);
                ::core::hash::Hash::hash(&(*__self_0), state);
                ::core::hash::Hash::hash(&(*__self_1), state);
                ::core::hash::Hash::hash(&(*__self_2), state);
                ::core::hash::Hash::hash(&(*__self_3), state)
            }
            _ => ::core::hash::Hash::hash(&std::mem::discriminant(self), state),
        }
    }
}

impl<'s, M> ::core::cmp::PartialEq for DocPtr<'s, M> {
    #[inline]
    fn eq(&self, other: &DocPtr<'s, M>) -> bool {
        match *other {
            DocPtr(ref idx1, _) => match *self {
                DocPtr(ref idx2, _) => {
                    (*idx1) == (*idx2) 
                }
            },
        }
    }
    #[inline]
    fn ne(&self, other: &DocPtr<'s, M>) -> bool {
        match *other {
            DocPtr(ref idx1, _) => match *self {
                DocPtr(ref idx2, _) => {
                    (*idx1) != (*idx2)
                }
            },
        }
    }
}

impl<'s, M> Eq for DocPtr<'s, M> {}

impl<'s, M> ::core::hash::Hash for DocPtr<'s, M> {
    fn hash<__H: ::core::hash::Hasher>(&self, state: &mut __H) -> () {
        match *self {
            DocPtr(ref __self_0_0, ref __self_0_1) => {
                ::core::hash::Hash::hash(&(*__self_0_0), state);
            }
        }
    }
}

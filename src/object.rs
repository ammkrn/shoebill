use crate::{ concat, Doclike, StrOrDoc, HasPrinter, DocPtr };

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Object<'p> {
    name : Option<StrOrDoc<'p>>,
    fields : Vec<(StrOrDoc<'p>, StrOrDoc<'p>)>,
    delims : (&'p str, &'p str),
    sep : &'p str,
    assn : &'p str,
    nest : u8
}

impl<'p> Object<'p> {
    pub fn new() -> Self {
        Object {
            name : None,
            fields : Vec::new(),
            delims : (" {", "};"),
            assn : ": ",
            sep : ",",
            nest : 4u8
        }
    }

    pub fn name(mut self, n : impl Into<StrOrDoc<'p>>) -> Self {
        self.name = Some(n.into());
        self
    }

    pub fn field_nest(mut self, amt : u8) -> Self {
        self.nest = amt;
        self
    }

    pub fn delims(mut self, open : &'p str, close : &'p str) -> Self {
        self.delims = (open, close);
        self
    }
    
    pub fn assn(mut self, assn : &'p str) -> Self {
        self.assn = assn;
        self
    }

    pub fn sep(mut self, sep : &'p str) -> Self {
        self.sep = sep;
        self
    }

    pub fn add_field(mut self, k : impl Into<StrOrDoc<'p>>, v : impl Into<StrOrDoc<'p>>) -> Self {
        let field = (k.into(), v.into());
        self.fields.push(field);
        self
    }

    pub fn to_doc(self, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {
        let mut d = match self.name {
            None => self.delims.0.alloc(pr),
            Some(n) => {
                n.concat(self.delims.0, pr)
            }
        };

        for (k, v) in self.fields {
            let kv = concat!([k, self.assn, v, self.sep], pr);

            d = d.nest_doc(kv, self.nest as u32, pr);
        }

        d.nest_doc(self.delims.1, 0, pr)
    }
}

use crate::{ concat, Doclike, StrOrDoc, HasPrinter, DocPtr, Renderable };
use crate::ron::mk_path_name;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Object<'p> {
    name : Vec<StrOrDoc<'p>>,
    fields : Vec<(StrOrDoc<'p>, StrOrDoc<'p>)>,
    delims : (&'p str, &'p str),
    sep : &'p str,
    assn : &'p str,
    nest : u8
}

impl<'x, 'p : 'x> Object<'p> {
    pub fn new() -> Self {
        Object {
            name : Vec::new(),
            fields : Vec::new(),
            delims : (" {", "};"),
            assn : ": ",
            sep : ",",
            nest : 4u8
        }
    }

    pub fn add_name(&mut self, n : impl Into<StrOrDoc<'p>>) {
        self.name.push(n.into());
    }

    pub fn field_nest(&mut self, amt : u8) {
        self.nest = amt;
    }

    pub fn delims(&mut self, open : &'p str, close : &'p str) {
        self.delims = (open, close);
    }
    
    pub fn assn(&mut self, assn : &'p str) {
        self.assn = assn;
    }

    pub fn sep(&mut self, sep : &'p str) {
        self.sep = sep;
    }

    pub fn add_field(&mut self, k : impl Into<StrOrDoc<'p>>, v : impl Into<StrOrDoc<'p>>) {
        self.fields.push((k.into(), v.into()));
    }

    pub fn to_doc(self, pr : &mut impl HasPrinter<'p>) -> DocPtr<'p> {
        let mut d = mk_path_name(self.name, pr);
        d = d.concat(self.delims.0, pr);

        for (k, v) in self.fields {
            let kv = concat!([k, self.assn, v, self.sep], pr);

            d = d.nest_doc(kv, self.nest as u32, pr);
        }

        d.nest_doc(self.delims.1, 0, pr)
    }

    pub fn render<P : HasPrinter<'p>>(self, line_width : u32, pr : &'x mut P) -> Renderable<'x, 'p, P> {
        let d = self.to_doc(pr);
        d.render(line_width, pr)
    }    
}

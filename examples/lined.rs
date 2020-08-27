use shoebill::compose;
use shoebill::Printer;
use shoebill::DocPtr;
use shoebill::HasPrinter;
use shoebill::Doclike;

// Stand-in for some type a user might want to implement. Let's say it holds
// a parsed file's contents as a string slice, and the printer.
pub struct FilePrinter<'p> {
    s : &'p str,
    printer : Printer<'p>
}

impl<'p> HasPrinter<'p> for FilePrinter<'p> {
    fn printer(&self) -> &Printer<'p> {
        &self.printer
    }

    fn printer_mut(&mut self) -> &mut Printer<'p> {
        &mut self.printer
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    start : usize,
    end : usize,
}

// We want to be able to use spans like documents without any extra syntactic
// noise, so we implement Doclike, fixing the `P : HasPrinter` as the thing
// to which our span is tethered. second call to alloc (`slice.alloc`) offloads
// the allocation of the retrieved slice to the implementation of Doclike for
// string slices, which is provided by the library.
impl<'p> Doclike<'p, FilePrinter<'p>> for Span {
    fn alloc(self, p : &mut FilePrinter<'p>) -> DocPtr<'p> {
        let slice = &p.s[self.start..self.end];
        slice.alloc(p)
    }
}

fn main() {
    let source = "Here is some sample text that might represent a parsed file";
    let mut f = FilePrinter {
        s : &source,
        printer : Printer::new(),
    };

    let span1 = Span { start : 0, end : 4 };
    let span2 = Span { start : 6, end : 7 };
    let span3 = Span { start : 8, end : 12 };
    let span4 = Span { start : 20, end :  24 };

    let d1 = compose!(
        &mut f ;
        span1 <> span2 <s> span3 <s> span4
    );

    // prints "Heres some text"
    println!("{}\n", d1.render(80, &mut f));
}

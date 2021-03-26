use shoebill::{ concat_, DocStore, Doclike, Doc::* };
use shoebill::{ intercal, intercal_trailing };

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ph1;
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ph2;

fn test0() {
    let s = &mut DocStore::<Ph1>::default();
    let target = format!("line1\n    line2\n        line3\nline4");
    let l2 = Newline(None).concat("line2", s).nest(4, s);
    let l3 = Newline(None).concat("line3", s).nest(8, s);
    let l4 = Newline(None).concat("line4", s);
    let out = "line1".concat(l2, s).concat(l3, s).concat(l4, s);
    let out_ = format!("{}", out.render(80, s));
    assert_eq!(out_, target);
}

#[test]
fn test1() {
    let s = &mut DocStore::<Ph1>::default();
    let target = format!("line1\n    line2\n        line3\nline4");
    let out = "line1"
        .concat(
            Newline(None)
            .concat("line2", s)
            .nest(4, s), 
            s
        )
        .concat(
            Newline(None)
            .concat("line3", s)
            .nest(8, s),
            s
        )
        .concat(
            Newline(None)
            .concat("line4", s),
            s
        );
    let out_ = format!("{}", out.render(80, s));
    assert_eq!(out_, target);
}    
    
#[test]
fn test0m() {
    let s = &mut DocStore::<Ph1>::default();
    /* 
    line1
        line2
            line3
    line4
    */
    let target = format!("line1\n    line2\n        line3\nline4");
    let l2 = Newline(None).concat("line2", s).nest(4, s);
    let l3 = Newline(None).concat("line3", s).nest(8, s);
    let l4 = Newline(None).concat("line4", s);
    let out = concat_!("line1", l2, l3, l4; s);
    assert_eq!(target, format!("{}", out.render(80, s)));
}    
// from line1 Newline(None) line2 Newline(" asdf ") line3
// line1 line2 asdf line3
#[test]
fn test2() {
    let s = &mut DocStore::<Ph1>::default();
    let target = format!("line1 line2 asdf line3");
    let l2 = Newline(None).concat("line2", s);
    let l3 = Newline(Some(" asdf ".alloc(s))).concat("line3", s);

    let out = "line1".concat(l2, s).concat(l3, s);
    assert_eq!(target, format!("{}", out.render_flat(80, s)));
}      

// from line1 Newline(None) line2 Newline(" asdf ") line3
// line1 line2 asdf line3
#[test]
fn test3() {
    let s = &mut DocStore::<Ph1>::default();
    let target = format!("line1 line2 asdf line3");
    let l2 = Newline(None).concat("line2", s);
    let l3 = Newline(Some(" asdf ".alloc(s))).concat("line3", s);

    let out = "line1".concat(l2, s).concat(l3, s).group(s);
    assert_eq!(target, format!("{}", out.render(80, s)));
}          

#[test]
fn test_inter0() {
    let s = &mut DocStore::<Ph1>::default();
    let target = format!("a @ b @ c @ d");

    let out = intercal!("a", "b", "c", "d"; " @ ", s);
    assert_eq!(target, format!("{}", out.render(80, s)));
}        

#[test]
fn test_inter_trailing0() {
    let s = &mut DocStore::<Ph1>::default();
    let target = format!("a @ b @ c @ d @");

    let out = intercal_trailing!("a", "b", "c", "d"; " @ ", " @", s);
    assert_eq!(target, format!("{}", out.render(80, s)));
}     

#[test]
fn test_inter_trailing1() {
    let s = &mut DocStore::<Ph1>::default();
    let target = format!("a @"); 

    let out1 = intercal_trailing!("a"; " @ ", " @", s);
    let out2 = intercal_trailing!("a"; " @", s);
    assert_eq!(target, format!("{}", out1.render(80, s)));
    assert_eq!(target, format!("{}", out2.render(80, s)));
}     




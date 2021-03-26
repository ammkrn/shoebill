
use shoebill::DocStore;
use shoebill::Doclike;

#[test]
fn bracket0() {
    let pr = &mut DocStore::<()>::default();
    let s1 = "as".concat_space("df", pr);
    let br1 = s1.always();
    let out = br1.w_paren(pr);
    assert_eq!("(as df)", format!("{}", out.render(80, pr)));

    let out = br1.wo_bracket();
    assert_eq!("as df", format!("{}", out.render(80, pr)));
}

#[test]
fn bracket1() {
    let pr = &mut DocStore::<()>::default();
    let s1 = "as".concat_space("df", pr);
    let br1 = s1.level(10);
    let out = br1.paren_upto(10, pr);
    assert_eq!("(as df)", format!("{}", out.render(80, pr)));
    let out = br1.square_upto(10, pr);
    assert_eq!("[as df]", format!("{}", out.render(80, pr)));
    let out = br1.wave_upto(10, pr);
    assert_eq!("{as df}", format!("{}", out.render(80, pr)));

    let out = br1.paren_upto(9, pr);
    assert_eq!("as df", format!("{}", out.render(80, pr)));
    let out = br1.square_upto(9, pr);
    assert_eq!("as df", format!("{}", out.render(80, pr)));
    let out = br1.wave_upto(9, pr);
    assert_eq!("as df", format!("{}", out.render(80, pr)));

    let out = br1.paren_upto(0, pr);
    assert_eq!("as df", format!("{}", out.render(80, pr)));
    let out = br1.square_upto(0, pr);
    assert_eq!("as df", format!("{}", out.render(80, pr)));
    let out = br1.wave_upto(0, pr);
    assert_eq!("as df", format!("{}", out.render(80, pr)));
}    

#[test]
fn bracket2() {
    let pr = &mut DocStore::<()>::default();
    let s1 = "as".concat_newline("df", pr).group(pr).level(10).wave_upto(10, pr);
    assert_eq!("{as df}", format!("{}", s1.render(80, pr)));

    let s1 = "as".concat_newline("df", pr).level(10).wave_upto(10, pr);
    assert_eq!("{as\ndf}", format!("{}", s1.render(80, pr)));
}     
use shoebill::Printer;
use shoebill::brackets::Bracketable;

#[test]
fn bracket_test1() {
    let mut store = Printer::new();
    let x = "test_str".bracket_never();
    let s = format!("{}", x.paren_upto(0, &mut store).render(80, &mut store));
    assert_eq!(s, "test_str");
}

#[test]
fn bracket_test2() {
    let mut store = Printer::new();
    let x = "test_str".bracket_always();
    let s = format!("{}", x.paren_upto(0, &mut store).render(80, &mut store));
    assert_eq!(s, "(test_str)");
}


#[test]
fn bracket_test3() {
    let mut store = Printer::new();
    let x = "test_str".bracket_always();
    let s = format!("{}", x.wave_upto(0, &mut store).render(80, &mut store));
    assert_eq!(s, "{test_str}");
}


#[test]
fn bracket_test4() {
    let mut store = Printer::new();
    let x = "test_str".bracket(10);
    let s = format!("{}", x.wave_upto(10, &mut store).render(80, &mut store));
    assert_eq!(s, "{test_str}");
}

#[test]
fn bracket_test5() {
    let mut store = Printer::new();
    let x = "test_str".bracket(10);
    let s = format!("{}", x.wave_upto(9, &mut store).render(80, &mut store));
    assert_eq!(s, "test_str");
}

#[test]
fn bracket_test6() {
    let mut store = Printer::new();
    let x = "test_str".bracket_never();
    let s = format!("{}", x.w_brackets("$", "~", &mut store).render(80, &mut store));
    assert_eq!(s, "$test_str~");
}




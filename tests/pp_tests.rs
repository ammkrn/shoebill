
use shoebill::{ compose, concat, concat_w, inter, inter_trailing };
use shoebill::Printer;
use shoebill::Doc::*;
use shoebill::object::Object;
use shoebill::Doclike;

#[test]
fn test1() {
    let mut store = Printer::new();
    let d1 = "asdf".alloc(&mut store);
    let d2 = String::from("hjkl").alloc(&mut store);
    let d3 = d1.concat(d2, &mut store);
    let rendered = format!("{}", d3.render(80, &store));
    println!("rendered : {}\n", rendered);
    assert_eq!(rendered, String::from("asdfhjkl"));
}

#[test]
fn test2() {
    let mut store = Printer::new();
    let d = "asdf".concat(format!("hjkl"), &mut store);
    let rendered = format!("{}", d.render(80, &store));
    assert_eq!(rendered, String::from("asdfhjkl"));
}
#[test]
fn test3() {
    let mut store = Printer::new();
    let d = "a".concat("a", &mut store)
            .concat("b", &mut store)
            .concat("c", &mut store);
    assert_eq!(format!("{}", d.render(80, &store)), "aabc");
}

#[test]
fn test4() {
    let mut store = Printer::new();
    let d = "a".concat(" : ", &mut store);
    let d = d.nest_doc("{", 0, &mut store);
    let d = d.nest_doc("field1 : X,", 4, &mut store);
    let d = d.nest_doc("field2 : YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY,", 4, &mut store);
    let d = d.nest_doc("field3 : Z,", 4, &mut store);
    let d = d.nest_doc("}", 0, &mut store);
    let rendered = d.render(30, &store);
    println!("rendered 4 : \n{}", rendered);
}

#[test]
fn test5() {
    let mut store = Printer::new();
    let d = "a".concat(Newline(None), &mut store);
    let d = d.concat("b", &mut store);
    let rendered = d.group(&mut store).render(80, &mut store);
    println!("rendered 5 : \n{}", rendered);

}
#[test]
fn test6() {
    let mut store = Printer::new();
    let d = concat!(["a", "b", "c", "d"], &mut store);
    println!("rendered macro : \n{}", d.render(80, &mut store));
}

#[test]
fn test7() {
    let mut store = Printer::new();
    let d = inter!(["a", "b", "c", "d"], ", ", &mut store);
    println!("rendered macro : \n{}", d.render(80, &mut store));
}

#[test]
fn test8() {
    let mut store = Printer::new();
    let d = inter_trailing!(["a", "b", "c", "d"], ", ", &mut store);
    println!("rendered macro : \n{}", d.render(80, &mut store));
}


#[test]
fn test10() {
    let mut store = Printer::new();
    let mut obj = Object::new();
    obj.add_name("MyObject");
    obj.add_field("key1", "val1");
    obj.add_field("key2", format!("val2"));

    let obj = obj.to_doc(&mut store);
    println!("rendered obj (10) : \n{}", obj.render(80, &store));
}

#[test]
fn test11() {
    let mut store = Printer::new();
    let mut obj1 = Object::new();
    obj1.add_name("MyObject");
    obj1.add_field("key1", "val1");
    obj1.add_field("key2", format!("val2"));
    assert_eq!(format!("{}", obj1.clone().render(80, &mut store)).as_str(), "MyObject {\n    key1: val1,\n    key2: val2,\n};");

    let mut obj2 = Object::new();
    obj2.add_name("Obj2");
    obj2.add_field("keyA", "X");
    obj2.add_field("keyB", obj1.to_doc(&mut store));
    obj2.add_field("keyC", format!("Y"));

    println!("rendered obj2 (11) : \n{}", obj2.render(80, &mut store));
}

#[test]
fn test12() {
    let mut store = Printer::new();
    let mut obj1 = Object::new();
    obj1.add_name("MyObject");
    obj1.add_field("key1", "val1");
    obj1.add_field("key2", format!("val2"));
    obj1.delims("(", ")");

    let mut obj2 = Object::new();
    obj2.add_name("Obj2");
    obj2.add_field("keyA", "X");
    obj2.add_field("keyB", obj1.to_doc(&mut store));
    obj2.add_field("keyC", format!("Y"));
    obj2.delims(" [", "];");
    obj2.assn(" := ");

    println!("rendered obj2 (11) : \n{}", obj2.render(80, &mut store));
}

#[test]
fn test13() {
    let mut store = Printer::new();
    let d1 = "asdf".concat("hjkl", &mut store);
    let val = compose!{
        &mut store ; d1
    };
    println!("rendered 13 : {}\n", val.render(80, &store));
}


#[test]
fn test14() {
    let mut store = Printer::new();
    let val = compose!{
        &mut store ; ("asdf" <> "hjkl") <> " qwerty"
    };
    println!("rendered 14 : {}\n", val.render(80, &store));
}

#[test]
fn test15() {
    let mut store = Printer::new();
    let val = compose!{
        &mut store ; ("fst1" <n> "snd2" <n> "asdf") <> ("asdf" <n> "hjkl")
    };
    println!("rendered 15 : {}\n", val.render(80, &store));

}
#[test]
fn test16() {
    let mut store = Printer::new();
    let val = compose!{
        &mut store ; ("x" <s> "y" <s> "z") <s> ("a" <s> "b")
    };
    let rendered = format!("{}", val.render(80, &store));
    assert_eq!(rendered, "x y z a b");
}


#[test]
fn test18() {
    let mut store = Printer::new();
    let d = compose!(&mut store; "first" <n> "second" <n> "third");
    let d = d.group(&mut store);
    println!("test18 : {}\n", d.render(80, &store));


}

#[test]
fn test19() {
    let mut store = Printer::new();
    let d = compose!(&mut store; "first" <n> "second" <n> "third" <n> "fourth" <n> "fifth");
    let d = d.group(&mut store);
    println!("test19 : \n{}\n", d.render(20, &store));
}

#[test]
fn test20() {
    let mut store = Printer::new();

    let d = compose!(&mut store; g @ ("first" <n> "second" <n> "third" <n> "fourth" <n> "fifth"));
    println!("test20 : \n{}\n", d.render(80, &store));
}

#[test]
fn test22() {
    let mut store = Printer::new();

    let d = compose!(&mut store; ("first" <n> "second"));
    let d = d.group(&mut store);
    println!("test22 : {}\n", d.render(80, &store));
}

#[test]
fn test23() {
    let mut store = Printer::new();

    let d = compose!(&mut store; n @ ("first" <n> "second"));
    println!("test23 : {}\n", d.render(80, &store));
}

#[test]
fn test24() {
    let mut store = Printer::new();

    let d = compose!(&mut store; n @ n @ ("first" <n> "second"));
    println!("test24 : {}\n", d.render(80, &store));
}

#[test]
fn test25() {
    let mut store = Printer::new();

    let d = compose!(&mut store; "asdf" <> (n @ 8 @ n @ ("first" <n> "second")));
    println!("test25 : {}\n", d.render(80, &store));
}

#[test]
fn test26() {
    let mut store = Printer::new();

    let d = compose!(&mut store; 12 @ n @ g @ n @ ("first" <n> "second"));
    println!("test26 : {}\n", d.render(80, &store));
}

#[test]
fn test27() {
    let mut store = Printer::new();
    let newline_zero = Newline(Some("".alloc(&mut store)));
    let d = concat_w!(["a", "b", "c", "d"], newline_zero, &mut store);
    assert_eq!(format!("{}", d.render(80, &mut store)), format!("a\nb\nc\nd"));
    
    let mut store = Printer::new();
    let d = compose!(&mut store ; "this" <s> "is" <s> "text");
    assert_eq!(format!("{}", d.render(80, &mut store)), format!("this is text"));    
}

#[test]
fn test28() {
    let mut store = Printer::new();
    let d1 = concat_w!(["a", "b", "c", "d"], Hardline, &mut store).group(&mut store);
    let d2 = concat_w!(["a", "b", "c", "d"], Newline(None), &mut store).group(&mut store);
    assert_eq!(format!("{}", d1.render(80, &mut store)), format!("a\nb\nc\nd"));
    assert_eq!(format!("{}", d2.render(80, &mut store)), format!("a b c d"));
}

#[test]
fn test29() {
    let mut store = Printer::new();
    let d1 = compose!(&mut store ; "this is some sample" <h> "text for" <h> "hardline testing");
    let s1 = format!("this is some sample\ntext for\nhardline testing");
    assert_eq!(format!("{}", d1.render(120, &mut store)), s1);
}

#[test]
fn test30() {
    let mut store = Printer::new();
    let d1 = compose!(&mut store ; g @ ("this is some sample" <z> "text for" <z> "zero line testing"));
    let s1 = format!("this is some sampletext forzero line testing");
    assert_eq!(format!("{}", d1.render(120, &mut store)), s1);
}

#[test]
fn test31() {
    let mut store = Printer::new();
    let alt = "an 'a'".alloc(&mut store);
    let d = compose!(&mut store ; g @ "There should be" <s> (Newline(Some(alt))) <s> "here");
    let dman = "There should be"
    .concat(" ", &mut store)
    .concat(Newline(Some(alt)), &mut store)
    .concat(" ", &mut store)
    .concat("here", &mut store)
    .group(&mut store);

    let s1 = format!("There should be an 'a' here");
    assert_eq!(d, dman);
    assert_eq!(format!("{}", d.render(80, &mut store)), s1);
}

#[test]
fn group_test0() {
    let mut store = Printer::new();
    let mut d = compose!(&mut store ; g @ "12" <z> "34");
    let extra = compose!(&mut store ; "56" <z> "789");
    d = d.concat(extra, &mut store);
    // Should break even though it doesn't run over. 
    let finished = format!("{}", d.render(8, &mut store));
    assert_eq!(finished.as_str(), "123456\n789");
}

#[test]
fn group_test1() {
    let mut store = Printer::new();
    let mut d = compose!(&mut store ; g @ "12" <z> "34");
    let extra = compose!(&mut store ; "56" <z> "78901234");
    d = d.concat(extra, &mut store);

    // Should break since it runs over.
    let finished = format!("{}", d.render(8, &mut store));
    assert_eq!(finished.as_str(), "123456\n78901234");
}

#[test]
fn group_test2() {
    let mut store = Printer::new();
    let mut d = compose!(&mut store ; g @ "12" <z> "34");
    d = compose!(&mut store ; d <> "56" <z> "78901234");

    // Should break since it runs over.
    let finished = format!("{}", d.render(8, &mut store));
    assert_eq!(finished.as_str(), "123456\n78901234");
}

#[test]
fn group_test3() {
    let mut store = Printer::new();
    let mut d = compose!(&mut store ; "12" <z> "34");
    d = compose!(&mut store ; g @ (d <> "56" <z> "01234567"));

    let finished = format!("{}", d.render(8, &mut store));
    assert_eq!(finished.as_str(), "12\n3456\n01234567");
}
#[test]
fn group_test4() {
    let mut store = Printer::new();
    let empty = "".alloc(&mut store);
    let n = Newline(Some(empty)).alloc(&mut store);
    let row1 = concat_w!(["0123401234", "0123401234", "0123401234"], n, &mut store).group(&mut store);
    let part1 = concat_w!(["01234", "01234"], n, &mut store).group(&mut store);
    let row2 = concat_w!([part1, part1, part1], n, &mut store).group(&mut store);

    let finished1 = format!("{}", row1.render(11, &mut store));
    let finished2 = format!("{}", row2.render(11, &mut store));
    let finished3 = format!("{}", row2.render_flat(11, &mut store));
    let target = format!("0123401234\n0123401234\n0123401234");
    assert_eq!(finished1, target);
    assert_eq!(finished2, target);
    assert_eq!(finished3, "012340123401234012340123401234");
}


use shoebill::{ Doclike, Printer };
use shoebill::ron::{ RonStruct, RonTuple, RonSequence, RonResult, RonOption };

#[test]
fn ron_test0_0() {
    let mut store = Printer::new();
    let k1 = "key1".alloc(&mut store);
    let v1 = format!("val1").alloc(&mut store);

    let mut obj = RonStruct::new();
    obj.add_name("SomeData");
    obj.add_field(k1, v1);
    obj.add_field(format!("key2"), "val2");
    obj.add_field("key3".alloc(&mut store), "va".concat("l3", &mut store));
    let obj = obj.to_doc(&mut store);

    println!("obj : \n{}\n", obj.render(80, &mut store));
}
    
// unnamed
#[test]
fn ron_test0_1() {
    let mut store = Printer::new();
    let k1 = "key1".alloc(&mut store);
    let v1 = format!("val1").alloc(&mut store);

    let mut obj = RonStruct::new();
    obj.add_field(k1, v1);
    obj.add_field(format!("key2"), "val2");
    obj.add_field("key3".alloc(&mut store), "va".concat("l3", &mut store));
    let obj = obj.to_doc(&mut store);

    println!("obj : \n{}\n", obj.render(80, &mut store));
}

#[test]
fn ron_test1_0() {
    let mut store = Printer::new();
    let k1 = "key1".alloc(&mut store);
    let v1 = format!("val1").alloc(&mut store);

    let mut obj1 = RonStruct::new();
    obj1.add_name("SomeData");
    obj1.add_field(k1, v1);
    obj1.add_field(format!("key2"), "val2");
    obj1.add_field("key3".alloc(&mut store), "va".concat("l3", &mut store));
    let obj1 = obj1.to_doc(&mut store);
    let s1 = format!("obj : \n{}\n", obj1.render(80, &mut store));

    
    let mut obj2 = RonStruct::new();
    obj2.add_name("SomeData");
    obj2.add_field(k1, v1);
    obj2.add_field(format!("key2"), "val2");
    obj2.add_field("key3".alloc(&mut store), "va".concat("l3", &mut store));
    let obj2 = obj2.to_doc(&mut store);
    let s2 = format!("obj : \n{}\n", obj2.render(80, &mut store));
    assert_eq!(s1, s2)
}

// Nested object formatting
#[test]
fn ron_test1_1() {
    let mut store = Printer::new();
    let k1 = "key1".alloc(&mut store);
    let v1 = format!("val1").alloc(&mut store);

    let mut obj1 = RonStruct::new();
    obj1.add_name("SomeData");
    obj1.add_field(k1, v1);
    obj1.add_field(format!("key2"), "val2");
    obj1.add_field("key3".alloc(&mut store), "va".concat("l3", &mut store));
    let obj1 = obj1.to_doc(&mut store);

    
    let mut obj2 = RonStruct::new();
    obj2.add_name("SomeData");
    obj2.add_field(k1, v1);
    obj2.add_field(format!("key2"), "val2");
    obj2.add_field("key3".alloc(&mut store), "va".concat("l3", &mut store));
    obj2.add_field("key4".alloc(&mut store), obj1);
    let obj2 = obj2.to_doc(&mut store);
    println!("obj : \n{}\n", obj2.render(80, &mut store));
}    

#[test]
fn ron_test2_0() {
    let mut store = Printer::new();
    let v1 = format!("val1").alloc(&mut store);

    let mut obj = RonTuple::new();

    println!("no name, no fields : \n{}\n", obj.clone().to_doc(&mut store).render(80, &mut store));
    obj.add_field(v1);
    obj.add_field("asdf");
    println!("no name, w/ fields : \n{}\n", obj.clone().to_doc(&mut store).render(80, &mut store));
    obj.add_name("name1");
    obj.add_name("name2");
    println!("w/ name, w/ fields : \n{}\n", obj.clone().to_doc(&mut store).render(80, &mut store));

    let mut obj = RonTuple::new();
    obj.add_name("Named_w_no_fields");
    obj.add_name("n2");
    println!("\n{}\n", obj.clone().to_doc(&mut store).render(80, &mut store));
}


#[test]
fn ron_test2_0_1() {
    let mut store = Printer::new();
    let v1 = format!("val1").alloc(&mut store);

    let mut obj = RonStruct::new();

    println!("no name, no fields : \n{}\n", obj.clone().to_doc(&mut store).render(80, &mut store));
    obj.add_field("f1", v1);
    obj.add_field("f2", "asdf");
    println!("no name, w/ fields : \n{}\n", obj.clone().to_doc(&mut store).render(80, &mut store));
    obj.add_name("name1");
    obj.add_name("name2");
    println!("w/ name, w/ fields : \n{}\n", obj.clone().to_doc(&mut store).render(80, &mut store));

    let mut obj = RonTuple::new();
    obj.add_name("Named_w_no_fields");
    obj.add_name("n2");
    println!("\n{}\n", obj.clone().to_doc(&mut store).render(80, &mut store));
}
    
// unnamed
#[test]
fn ron_test2_1() {
    let mut store = Printer::new();
    let v1 = format!("val1").alloc(&mut store);
    let mut nested_struct = RonStruct::new();
    nested_struct.add_field("longish_struct_key1", "longish_struct_val1");
    nested_struct.add_field("longish_struct_key2", "longish_struct_val2");
    let s = nested_struct.to_doc(&mut store);

    let mut tup = RonTuple::new();
    tup.add_field(v1);
    tup.add_field(s);
    tup.add_field("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
    tup.add_field("va".concat("l3", &mut store));
    let tup = tup.to_doc(&mut store);

    let mut seq = RonSequence::new();
    seq.add_field(tup);
    seq.add_field(tup);
    seq.add_field(tup);
    let seq = seq.to_doc(&mut store);

    println!("tuple only : \n{}\n", tup.render(40, &mut store));

    println!("as_seq : \n{}\n", seq.render(40, &mut store));
}    

#[test]
fn ron_test3_0() {
    let mut store = Printer::new();
    let v1 = format!("val1").alloc(&mut store);
    let mut nested = RonStruct::new();
    nested.add_name("SomeData");
    nested.add_field("key1", v1);
    nested.add_field(format!("key2"), "val2");
    nested.add_field("key3".alloc(&mut store), "va".concat("l3", &mut store));
    let nested = nested.to_doc(&mut store);

    let mut obj = RonSequence::new();
    obj.add_name("SomeData");
    obj.add_field(v1);
    obj.add_field("val2");
    obj.add_field(nested);
    obj.add_field("va".concat("l3", &mut store));
    let obj = obj.to_doc(&mut store);

    println!("named seq : \n{}\n", obj.render(80, &mut store));
}

#[test]
fn ron_test3_1() {
    let mut store = Printer::new();
    let v1 = format!("val1").alloc(&mut store);

    let mut nested = RonStruct::new();
    nested.add_name("SomeData");
    nested.add_field("key1", v1);
    nested.add_field(format!("key2"), "val2");
    nested.add_field("key3".alloc(&mut store), "va".concat("l3", &mut store));
    let nested = nested.to_doc(&mut store);
    assert_eq!(
        format!("{}", nested.clone().render_flat(80, &mut store)).as_str(),
        "SomeData { key1: val1, key2: val2, key3: val3, }"
    );

    let mut obj = RonSequence::new();
    obj.add_field(v1);
    obj.add_field("val2");
    obj.add_field(nested);
    obj.add_field("va".concat("l3", &mut store));
    let obj = obj.to_doc(&mut store);
    println!("nested : \n{}\n", obj.render(80, &mut store));
}       

#[test]
fn ron_test4_1() {
    let mut store = Printer::new();
    let r : Result<String, &'static str> = Ok(format!("qwertyhjklasdfuiop"));
    let ron_result = RonResult::new(r);

    assert_eq!(format!("{}", ron_result.clone().render(80, &mut store)).as_str(), "Ok(\n    qwertyhjklasdfuiop\n)");
    assert_eq!(format!("{}", ron_result.render(0, &mut store)).as_str(), "Ok(\n    qwertyhjklasdfuiop\n)");
}       

#[test]
fn ron_test4_2() {
    let mut store = Printer::new();
    let r : Result<String, &'static str> = Err("qwertyhjklasdfuiop");
    let ron_result = RonResult::new(r);

    assert_eq!(format!("{}", ron_result.clone().render(80, &mut store)).as_str(), "Err(\n    qwertyhjklasdfuiop\n)");
    assert_eq!(format!("{}", ron_result.render(0, &mut store)).as_str(), "Err(\n    qwertyhjklasdfuiop\n)");
}       

#[test]
fn ron_test5_0() {
    let mut store = Printer::new();
    let r = Some(format!("ASDF"));
    let mut ron = RonOption::new(r);
    ron.add_name("MyOption");

    assert_eq!(format!("{}", ron.clone().render(80, &mut store)).as_str(), "MyOption: Some(\n    ASDF\n)");
    assert_eq!(format!("{}", ron.render(0, &mut store)).as_str(), "MyOption: Some(\n    ASDF\n)");
}       

#[test]
fn ron_test5_1() {
    let mut store = Printer::new();
    let r : Option<String> = None;
    let mut ron = RonOption::new(r);
    ron.add_name("MyOption");

    println!("named option : \n{}\n", ron.render(80, &mut store));
}       

#[test]
fn ron_test6_1() {
    let mut store = Printer::new();
    let mut ron = RonTuple::new();
    ron.add_name("ATuple");

    ron.add_field("field1");
    ron.add_field("field2");
    assert_eq!(format!("{}", ron.clone().render(80, &mut store)).as_str(), "ATuple(\n    field1,\n    field2,\n)");
    assert_eq!(format!("{}", ron.to_doc(&mut store).render_flat(80, &mut store)).as_str(), "ATuple(field1, field2,)");
}       

#[test]
fn ron_test_unit_struct0() {
    let mut store = Printer::new();
    let mut ron = RonStruct::new();
    ron.add_name("MyUnitStruct");
    let finished = ron.render(80, &mut store);
    assert_eq!("MyUnitStruct", format!("{}", finished));
}       

#[test]
fn ron_test_unit_struct1() {
    let mut store = Printer::new();
    let mut ron = RonStruct::new();
    ron.add_name("MyUnitStruct");
    ron.add_name("Path1");
    ron.add_name("Path2");
    let finished = ron.render(80, &mut store);
    assert_eq!("MyUnitStruct::Path1::Path2", format!("{}", finished));
}       

use simple_derive_builder::Builder;

#[derive(Debug, Builder)]
struct Test<'a, T: Default>
where
    T: Clone,
{
    #[setter(name = "test_int")]
    test_i32: i32,
    #[setter(each = "test_i32")]
    test_vec_i32: Vec<i32>,
    test_generic: T,
    test_lifetime: &'a (),
}

fn main() {
    let b = TestBuilder::<i32>::new();
    let b = b
        .test_int(0)
        .test_i32(0)
        .test_i32(1)
        .test_generic(0)
        .test_lifetime(&());

    let t = b.build().unwrap();

    println!("{:?}", t);
}

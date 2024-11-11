mod ast;
mod parser;

fn main() {
    let src = std::env::args().nth(1).unwrap();

    println!("{:?}", parser::must_parse(&src));
}

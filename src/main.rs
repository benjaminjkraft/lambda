mod ast;
mod checker;
mod parser;

fn main() {
    let src = std::env::args().nth(1).unwrap();

    println!("{:?}", checker::check(&parser::must_parse(&src)));
}

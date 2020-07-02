mod cpu;
mod isa;
mod parser;
fn main() {
    let rom = parser::read_nesrom(String::from("donkeykong.nes"));

    println!("Header: {:?}", rom.header);
    println!("Trainer: {:?}", rom.trainer);
}

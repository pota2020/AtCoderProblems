use proconio::input;
 
fn main() {
    input! {
        x: u32,
    }
    let ans =
        match x{
            0 => 1,
            1 => 0,
            _ => 0,
        };
    println!("{}",ans);
}
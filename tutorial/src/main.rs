use std::io;
use rand::Rng;


fn main(){
    println!("Guess the number");
    println!("Please input your number");
    let mut guess = String::new();

    io::stdin()
        .read_line(&mut guess)
        .expect("Failed to read Line");
    
    let guess:i32 = guess.trim().parse()
        .expect("The inputted value was not recognized as a number: Please try next time!");
    

    let secret_num:i32 = rand::thread_rng().gen_range(1..=100);

    println!("You guessed {} !", guess);
    println!("The secret_num was {secret_num}");

    if secret_num == guess
    { 
        println!("You got it right!");}
    else
    { 
        println!("You got it wrong :C .");
    }
    



}

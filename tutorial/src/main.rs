use std::io;
use rand::Rng;
use std::cmp::Ordering;

fn main(){
    println!("Guess the number");
    let mut guess = String::new();

    let secret_num:i32 = rand::thread_rng().gen_range(1..=100);
    println!("The secret_num was {secret_num}");
    

    loop {
        println!("Please input your number");
        io::stdin()
            .read_line(&mut guess)
            .expect("Failed to read Line");
        
        let guess:i32 = match guess.trim().parse()
        {
            Ok(number) => number,
            Err(_) => {println!("Unable to convert input to i32");continue;},

        };    
    
        println!("You guessed {} !", guess);
    
        match guess.cmp(&secret_num){
            Ordering::Less => println!("Too little!"),
            Ordering::Greater => println!("Too much!"),
            Ordering::Equal =>{ 
                println!("You got it!");
                break;
            },
            } 
        }


}

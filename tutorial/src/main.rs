use std::io;
use rand::Rng;
use std::cmp::Ordering;

fn main(){
    println!("Guess the number");
    

    let tupes: (i32,f64,char) = (55,21.0,'a');

    let secret_num:i32 = rand::thread_rng().gen_range(1..=100);
    

    loop {

        let mut guess = String::new();

        println!("Please input your number");
        io::stdin()
            .read_line(&mut guess)
            .expect("Failed to read Line");
    
        let guess:i32 = match guess.trim().parse()
        {
            Ok(number) => {
                println!("You guessed {number}");
                number
            },
            Err(_) => {
                println!("Unable to convert input to i32");
                continue;
            },

        };    
    
        // Handling <=> conditions  
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

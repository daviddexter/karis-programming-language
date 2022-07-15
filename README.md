# Karis Programming Language

Lightweight programming-ish language built for education purposes.

Karis is statically typed. It borrows inspiration from Rust, Go and JavaScript.

## Example program in Karis

```kr

let add @int = fn(x @int, y @int){
    return x + y;
};

let sub @int = fn(x @int, y @int){
    return x - y;
};

let mul @int = fn(x @int, y @int){
    return x * y;
};

let div @int = fn(x @int, y @int){
    return x * y;
};

let echo @string = fn(name @string){
    return name;
};

let printer @unit = fn(name @string){
    print("Name #name");
};

let max @int = fn(x @int, y @int){
    if x > y{
        return x;
    };
    
    return y;
};

let factorial @int = fn(n @int){
    if n == 1 {
		return 1;
	};

    return n * factorial(n-1);
};

let fibonacci @int = fn(n @int){
    if n == 0 {
		return 0;
	};

    if n == 1 || n == 2 {
		return 1;
	};

    return fibonacci(n-1) + fibonacci(n-2);
};

@main fn(){
    let x @int = 5;
    let y @int = 7;
    let name @string = "Karis";
    
    let result0 @int = add(x,y);
    let result1 @int = sub(x,y);
    let result2 @int = mul(x,y);
    let result3 @int = div(x,y);
    let result4 @string = echo(name);
    let result5 @int = echo(x,y);
    let result6 @int = add(x,y) + 5 / 10 * 9;
    let result6 @int =  5 / 10 * 9 + add(x,y);  
    let result7 @int = factorial(5);
    let result8 @int = fibonacci(3);

    print(result0);
    print(result1);
    print(result2);
    print(result3);
    print(result4);
    print(result5);
    print(result7);  
    print(result8); 
}@end;

```

## Structure

1. *Console*

The `RLPL` and `REPL` are located here. It's the main entry point into the Karis. 

Yet to be implemented:

- CLI interface to compile source file

2. *Errors*

Custom errors used throughout the entire language

3. *Lexer*

Definitions and logic to split source file into tokens that can later be interpreted.

4. *Parser*

Contains logic tha builds the AST of the program before evaluation. Syntax errors are caught 
here. [Top Down Operator Precedence or Pratt Parser](https://tdop.github.io/) is employed so 
that we don't use some formal grammar that can be fed to something like `yacc`. 
Again, the intention is to learn.


## Currently not yet supported

1. Multi-file source code.

Having multiple source files required a module system or something similar. That is somewhat out of scope of 
what `Karis` what intended for.

2. Libraries

For day one, the purpose was to build a compile language. As such, libraries or packages are not yet supported.
A `main` function must be defined for the program to be valid

3. Code comments

While this can be easily implemented, the choice not to support it intentional so as to reduce the scope of what needs
to be done

4. Decimal

Decimals/Floats are intentionally left out. The idea is to have a generic number type which encompasses Int and Floats.
This may be added when bandwdith allows.

5. String concatenation

String literal can not be joined together to create a new string from the invidual parts. Will be nice to explore how this
can be implemented. That's for later. Currently, no string concatenation.

6. Advanced language features

Features like struct/classes, impl/interface, logical loops, complex mathematical operations etc,  are not supported. Check out the sample 
program above to get a gist of a set of functionalities/features `karis` supports.

## Author's note

This project is dedicated to my parents who have always supported me.
**_I love you mum and dad :)_**

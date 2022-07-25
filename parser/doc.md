# Parsing fancy

## Parsing strategies

Let examine the syntax of Karis language;

```karis
let add = fn(x @int, y @int) @int{
    return x + y;
};

let x @int = 5;

let y @int = 7;

let name @string = "Karis";

let echo = fn(name @string){
    print("Name #name")
};

let x0 @int = 1 + 2;

@main fn(){
    let result0 @int = add(x,y);
    print(result0);       
}@end;
```

What can we note? 

Everything we the exception of `@main` and `print` function, are bindings. That is, they
have a `let` keyword infront of them. Whether it's a literal or a custom function, it must have
that binding that we will used to extract meaing.

Looking at the `add` function even further, we can see that the body returns something, an addition of `x` and `y`

Statements vs Expressions

Simply, statements declare while expressions produce, in this case produce values. We are going to take the approach borrowed from
functional programming languages and declare that in `karis lang` everythin is a expression. Everything produces a value

Taking this approach, we will have the following expressions in our AST

- LiteralExpression
- BinaryExpression
- FunctionExpression
- CallExpression
- ReturnExpression
- MainExpression (@main)


## Pratt parser

The main idea is pratt is `binding power`. Essentially, how strongly is one entity related to the one one the left.

## References

- https://dev.to/jrop/pratt-parsing
- https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/boolean-logical-operators#conditional-logical-and-operator-




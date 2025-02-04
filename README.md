# MiniPlait

## Implementation
In this project, I used plait as a metalanguage to implement a small functional programming language called miniPlait.
In essence, miniPlait is a stripped-down version of plait: 
* it is lexically scoped,
* uses eager, call-by-value evaluation
* featuring variables, 
* basic operators, 
* first-class functions, 
* and static types.
### Parser
a function `parse` which consumes an expression in the miniPlait
language's concrete syntax, `S-Exp`, and returns the abstract syntax
representation of that expression (an `Expr`).

```
parse :: S-Exp -> Expr
```

`parse` only accepts expressions that follow miniPlait's [grammar](#grammar).

### Interpreter
a function: `interp`.

- `interp :: Expr -> Value`

    which consumes an abstract syntax tree (i.e. an `Expr`) and returns a miniPlait `Value`.

    `interp` should evaluate programs by performing a
    _post-order traversal_ of the abstract syntax tree (AST): first, evaluate
    all of the children of an expression from left to right, then evaluate the
    expression itself. This makes it unambiguous which error to
    raise if there are multiple errors in the AST.

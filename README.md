# MiniPlait

## Grammar

The grammar of miniPlait is as follows:

```
<expr> ::= <num>
         | <string>
         | <var>                        # variable (a.k.a. identifier)
         | true
         | false
         | (+ <expr> <expr>)
         | (++ <expr> <expr>)
         | (num= <expr> <expr>)
         | (str= <expr> <expr>)
         | (if <expr> <expr> <expr>)
         | (lam <var> <expr>)           # anonymous function
         | (<expr> <expr>)             # function application
```

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
`interp :: Expr -> Value`

    which consumes an abstract syntax tree (i.e. an `Expr`) and returns a miniPlait `Value`.

    `interp` evaluates programs by performing a
    _post-order traversal_ of the abstract syntax tree (AST): first, evaluate
    all of the children of an expression from left to right, then evaluate the
    expression itself. This makes it unambiguous which error to
    raise if there are multiple errors in the AST.

The features of the `interp`:
- Environment:
    - The interpreter uses an environment, `Env`, to keep track of the `Value`s of variables in scope: `(define-type-alias Env (Hashof Symbol Value))`
    -  `interp` allows _variable shadowing_, meaning that if you bind a variable that is already bound, the new binding takes precedence.
    -  When `interp` encounters an unbound variable, `interp` raises an error.
- Binary Operators: iniPlait includes binary addition (`+`) and number equality testing (`num=`), as
well as string appending (`++`) and string equality testing (`str=`).
- Conditionals:
    - `if`-expressions in miniPlait have three parts:
      - `cond`, which should evaluate to a Boolean `Value`
      - `consq`, which evaluates if `cond` evaluated to `true`
      - `altern`, which evaluates if `cond` evaluated to `false`
  - `if` statements does short-circuit (i.e. only evaluate the relevant branch). If `cond` evaluates to a non-Boolean `Value`, an error is raised. 
- Functions: Functions in miniPlait are unary (i.e. they take exactly 1 argument).

#### Abstract Syntax

Refer to [Environment](#environment) for the definition of `Env` and
[Binary Operators](#binary-operators) for the definition of `Operator`.

```
(define-type Value
  (v-num [value : Number])
  (v-str [value : String])
  (v-bool [value : Boolean])
  (v-fun [param : Symbol]
         [body : Expr]
         [env : Env]))

(define-type Expr
  (e-num [value : Number])
  (e-str [value : String])
  (e-bool [value : Boolean])
  (e-op [op : Operator]
        [left : Expr]
        [right : Expr])
  (e-if [cond : Expr]
        [consq : Expr]
        [altern : Expr])
  (e-lam [param : Symbol]
         [body : Expr])
  (e-app [func : Expr]
         [arg : Expr])
  (e-var [name : Symbol]))
```


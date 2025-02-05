# MiniPlait

## Grammar

The grammar of miniPlait is as follows:

```
<expr> ::= <num>
         | <string>
         | true
         | false
         | (+ <expr> <expr>)
         | (++ <expr> <expr>)
         | (num= <expr> <expr>)
         | (str= <expr> <expr>)
         | (if <expr> <expr> <expr>)
         | (and <expr> <expr>)
         | (or <expr> <expr>)
         | <id>
         | (<expr> <expr>)
         | (lam (<id> : <type>) <expr>)
         | (let (<id> <expr> : <type>) <expr>)
         | (first <expr>)
         | (rest <expr>)
         | (is-empty <expr>)
         | (empty : <type>)
         | (link <expr> <expr>)

<type> ::= Num
         | Str
         | Bool
         | (List <type>)
         | (<type> -> <type>)
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
representation of that expression (an `Expr`). `Expr+` is very similar to the `Expr`. However,
it includes three new constructors, `sugar-and`, `sugar-or`, and `sugar-let`. These represent
the new syntactic sugar expressions. Also, to avoid confusion, the constructors it shares
with `Expr` have been renamed to `e-num+`, `e-app+`, and so on.

```
parse :: S-Exp -> Expr+
```

`parse` only accepts expressions that follow miniPlait's [grammar](#grammar).

### Interpreter
`interp :: Expr -> Value`

which consumes an abstract syntax tree (i.e. an `Expr`) and returns a miniPlait `Value`. `interp` evaluates programs by performing a  _post-order traversal_ of the abstract syntax tree (AST): first, evaluate all of the children of an expression from left to right, then evaluate the expression itself. This makes it unambiguous which error to raise if there are multiple errors in the AST.

The features of the `interp`:
- Environment:
    - The interpreter uses an environment, `Env`, to keep track of the `Value`s of variables in scope: `(define-type-alias Env (Hashof Symbol Value))`
    -  `interp` allows _variable shadowing_, meaning that if you bind a variable that is already bound, the new binding takes precedence.
    -  When `interp` encounters an unbound variable, `interp` raises an error.
- Binary Operators: miniPlait includes binary addition (`+`) and number equality testing (`num=`), as
well as string appending (`++`) and string equality testing (`str=`).
- Unary Operators: miniPlait includes features to manipulates list. See Type Checker features for details.
- Conditionals:
    - `if`-expressions in miniPlait have three parts:
      - `cond`, which should evaluate to a Boolean `Value`
      - `consq`, which evaluates if `cond` evaluated to `true`
      - `altern`, which evaluates if `cond` evaluated to `false`
  - `if` statements does short-circuit (i.e. only evaluate the relevant branch). If `cond` evaluates to a non-Boolean `Value`, an error is raised. 
- Functions: Functions in miniPlait are unary (i.e. they take exactly 1 argument).
### Desugar
```
desugar :: Expr+ -> Expr
```

which consumes an abstract syntax tree (i.e. an `Expr+`, as returned
by `parse`), replaces all instances of syntactic sugar with
desugared equivalents, and returns the result.

The features of the `desugar`:
- `and` and `or`: `desugar` should convert `sugar-and` and `sugar-or` `Expr+`s in such a way that, when `interp` interprets the desugared code, `and` and `or` **short-circuit**. In `and` expressions, this means that if the first argument of `and` evaluates to `false`, the second argument to `and` is not evaluated and the `and` expression evaluates to `false`. Similarly, if the first argument of `or` evaluates to `true`, the second argument to `or` should not be evaluated.) Thus, the second argument of a short-circuited expression should never throw an error.
  - `and` consumes two boolean expressions. It evaluates to `true` if both boolean
  expressions are true; otherwise, it evaluates to `false`.
  - `or` consumes two boolean expressions. It evaluates to `true` if at least one
  boolean expression is true; otherwise, it evaluates to `false`.
- `let`: should accept a single variable-value pair and a body. `let` evaluates the value, binds it to the variable, and evaluates the body with the newly bound variable in scope. For example, the following should evaluate to `3`: ``` (let (x 1) (+ x 2)) ```

### Type Checker

```
type-of :: Expr -> Type
```

that consumes a miniPlait program in abstract syntax form. If the program is well-typed, `type-of` returns the `Type` of that program; otherwise, it raises an exception.

The features of the `type-of`: 
- Type Environment: The type language the type checker works with respectively, represent the types of numbers, Booleans, strings, (one-argument) functions, and (homogenous) lists. Just as how Interpreter had an `Env` for mapping identifiers to values, your interpreter should use a type environment (`TEnv`) to keep track of the _types_ of identifiers in scope. In `type-of`, if the program binds an identifier that is already bound, the new binding should take precedence. This is known as _identifier shadowing_. The new binding shadows the existing binding.
- `let`: is a new expression which should accept a single identifier-value pair and a body. `let` evaluates the value, binds it to the identifier, and evaluates the body with the newly bound identifier in scope.
- Lists: Typed version of miniPlait contains support for lists via the constant `empty` and the operations `link`, `is-empty`, `first`, and `rest`. Lists in miniPlait are _homogeneous_: all elements in the list must have the same type.
  - `empty`

    `(empty : t)` makes an empty list whose elements have type `t`.

- `link :: t, (List t) -> (List t)`

    `(link x y)` appends the element `x` to the front of the list `y`.

- `first :: (List t) -> t`

    `(first x)` returns the first element of `x`.

- `rest :: (List t) -> (List t)`

    `(rest x)` returns the list `x` except for the first element of `x`.

- `is-empty :: (List t) -> Bool`

    `(is-empty x)` returns `true` if `x` is `empty`; otherwise, it returns false.
- d


### Abstract Syntax

```racket
(define-type Type
  (t-num)
  (t-bool)
  (t-str)
  (t-fun [arg-type : Type] [return-type : Type])
  (t-list [elem-type : Type]))

(define-type Value
  (v-num [value : Number])
  (v-bool [value : Boolean])
  (v-str [value : String])
  (v-fun [param : Symbol]
         [body : Expr]
         [env : Env])
  (v-list [vals : (Listof Value)]))

(define-type Expr+
  (e-num+ [value : Number])
  (e-bool+ [value : Boolean])
  (e-str+ [value : String])
  (e-op+ [op : Operator]
         [left : Expr+]
         [right : Expr+])
  (e-un-op+ [op : UnaryOperator]
            [expr : Expr+])
  (e-if+ [cond : Expr+]
         [consq : Expr+]
         [altern : Expr+])
  (e-lam+ [param : Symbol]
          [arg-type : Type]
          [body : Expr+])
  (e-app+ [func : Expr+]
          [arg : Expr+])
  (e-var+ [name : Symbol])
  (sugar-and [left : Expr+]
             [right : Expr+])
  (sugar-or [left : Expr+]
            [right : Expr+])
  (sugar-let [var : Symbol]
             [value : Expr+]
             [type : Type]
             [body : Expr+])
  (e-empty+ [elem-type : Type]))

(define-type Operator
  (op-plus)
  (op-append)
  (op-num-eq)
  (op-str-eq)
  (op-link))

(define-type UnaryOperator
  (op-first)
  (op-rest)
  (op-is-empty))
```


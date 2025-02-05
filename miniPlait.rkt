ight#lang plait

;; =============================================================================
;; Typed Interpreter: tynterp.rkt
;; =============================================================================

(require "support.rkt"
         (rename-in (typed-in "err-support.rkt"
                              [raise-type-error : (String -> 'a)]
                              [raise-interp-error : (String -> 'b)])))

(define (eval [str : S-Exp]): Value
  (let* ([expr (desugar (parse str))]
         [t (type-of expr)])
    (interp expr)))

;; =================================================

(define (desugar [expr : Expr+]): Expr
  (type-case Expr+ expr
   ;Convert the Expr+ => Expr
    [(e-num+ n) (e-num n)]
    [(e-str+ n) (e-str n)]
    [(e-bool+ n) (e-bool n)]
    [(e-empty+ elem-type) (e-empty elem-type)]
    [(e-op+ op left right) (e-op op (desugar left) (desugar right))]
    [(e-un-op+ op expr) (e-un-op op (desugar expr))]
    [(e-if+ cond consq altern) (e-if (desugar cond) (desugar consq) (desugar altern))]
    [(e-lam+ param arg-type body) (e-lam param arg-type (desugar body))]
    [(e-app+ func arg) (e-app (desugar func) (desugar arg))]
    [(e-var+ name) (e-var name)]
    ; Use helperfunctions to desugar
    [(sugar-and left right) (desugar-and (desugar left) (desugar right))]
    [(sugar-or left right) (desugar-or (desugar left) (desugar right))]
    [(sugar-let var value type body) (desugar-let var (desugar value) type (desugar body))]
    )
  )

(define (type-ofT [expr : Expr] [env : TEnv]): Type
  (type-case Expr expr
    [(e-num x) (t-num)]
    [(e-bool x) (t-bool)]
    [(e-str x) (t-str)]
    [(e-empty elem-type) (t-list elem-type)]
    [(e-op op left right)
     (type-case Operator op
       [(op-plus) (type-check-plus (type-ofT left env) (type-ofT right env))]
       [(op-append) (type-check-append (type-ofT left env) (type-ofT right env))]
       [(op-num-eq) (type-check-num-eq (type-ofT left env) (type-ofT right env))]
       [(op-str-eq) (type-check-str-eq (type-ofT left env) (type-ofT right env))]
       [(op-link) (type-check-link (type-ofT left env) (type-ofT right env))]
       )]
    [(e-un-op op expr)
      (type-case UnaryOperator op
        [(op-first) (type-check-first (type-ofT expr env))]
        [(op-rest) (type-check-rest (type-ofT expr env))]
        [(op-is-empty) (type-check-is-empty (type-ofT expr env))]
       )]
    [(e-if cond consq altern) (type-check-if (type-ofT cond env) (type-ofT consq env) (type-ofT altern env))]
    [(e-lam param arg-type body) (lambda param arg-type body env)]
    [(e-var name) (lookupTypeCheck env name)]
    [(e-app func arg) (applyTypeCheck (type-ofT func env) (type-ofT arg env))]
    )
  )

;Variable empty-env
(define empty-tenv (hash empty))

(define (type-of [expr : Expr]) : Type
  (type-ofT expr empty-tenv))

(define (interpE [expr : Expr] [env : Env]): Value
   (type-case Expr expr
    [(e-num v) (v-num v)]
    [(e-str v) (v-str v)]
    [(e-bool v) (v-bool v)]
    [(e-empty elem-type) (v-list empty)]
    [(e-op o l r)
     (type-case Operator o
       [(op-plus) 
        (add (interpE l env) (interpE r env))]
       [(op-append)
        (append (interpE l env) (interpE r env))]
       [(op-str-eq)
        (streql (interpE l env) (interpE r env))]
       [(op-num-eq)
        (numeql (interpE l env) (interpE r env))]
       [(op-link)
        (link (interpE l env) (interpE r env))]
       )]
    [(e-un-op op expr)
     (type-case UnaryOperator op
       [(op-first) (opFirst (interpE expr env))]
       [(op-rest) (opRest (interpE expr env))]
       [(op-is-empty) (is-empty (interpE expr env))]
      )]
    [(e-if cond cons alt)
     (if (boolean-decision (interpE cond env))
         (interpE cons env)
         (interpE alt env))
     ]
    [(e-var name) (lookupInterp env name)]
    [(e-lam param arg-type body) (v-fun param body env)]
    [(e-app func arg) (applyInterp (interpE func env) (interpE arg env))]
   ))

;Variable empty-env
(define empty-env (hash empty))

(define (interp [expr : Expr]): Value
  (interpE expr empty-env))

;; ################################################
;; ############### Helper functions ###############
;; ################################################

;; ################## desugar ######################
; Desugar/compile and otherwise give back desugar error
(define (desugar-and [left : Expr] [right : Expr]): Expr
  (type-case Expr left
    [(e-bool vLeft) (if vLeft ;If first value is true go further
                    (type-case Expr right
                      [(e-bool vRight) (e-bool vRight)]
                      [else (raise-type-error "The second arg of the and operator expects to be a boolean")]
                      )
                    (e-bool #f);Else return false
                 )]
    [else (raise-type-error "The first arg of the and operator expects to be a boolean")]
    )
  )

; Desugar/ compile or otherwise give back desugar error
(define (desugar-or [left : Expr] [right : Expr]): Expr
  (type-case Expr left
    [(e-bool vLeft) (if vLeft ;If first value is false go further
                         (e-bool #t)
                         (type-case Expr right
                           [(e-bool vRight) (e-bool vRight)]
                           [else (raise-type-error "The second arg of the or operator expects to be a boolean")]
                           )
                      )]
    [else (raise-type-error "The first arg of the or operator expects to be a boolean")]
    )
  )

;Bound the value to the var and interp the body or give back type error
(define (desugar-let [var : Symbol] [value : Expr] [type : Type] [body : Expr]): Expr
  (e-app (e-lam var type body) value)
  )

;; ################## tychk  ######################
;Check if both argument types are numbers and return num type or throw error
(define (type-check-plus [left : Type] [right : Type]): Type
  (type-case Type left
    [(t-num)
     (type-case Type right
       [(t-num) (t-num)]
       [else (raise-type-error "The second arg of the + operator expects to be a number")])]
    [else (raise-type-error "The first arg of the + operator expects to be a number")]))

;Check if both argument types are strings and return str type or throw error
(define (type-check-append [left : Type] [right : Type]): Type
  (type-case Type left
    [(t-str)
     (type-case Type right
       [(t-str) (t-str)]
       [else (raise-type-error "The second arg of the ++ operator expects to be a string")])]
    [else (raise-type-error "The first arg of the ++ operator expects to be a string")]))

;Check if both argument types are number ande return num type or throw error
(define (type-check-num-eq [left : Type] [right : Type]): Type
  (type-case Type left
    [(t-num)
     (type-case Type right
       [(t-num) (t-bool)]
       [else (raise-type-error "The second arg of the num= operator expects to be a number")])]
    [else (raise-type-error "The first arg of the num= operator expects to be a number")]))

; Check if both argument types are strings and return str type or throw error
(define (type-check-str-eq [left : Type] [right : Type]): Type
  (type-case Type left
    [(t-str)
     (type-case Type right
       [(t-str) (t-bool)]
       [else (raise-type-error "The second arg of the str= operator expects to be a string")])]
    [else (raise-type-error "The first arg of the str= operator expects to be a string")]))

;First check if second arg is a list then check first arg list elem type is the same as the type of to new elem that needs to be added and return list type or throw error
(define (type-check-link [left : Type] [right : Type]): Type
  (type-case Type right
    [(t-list elem-type) (if (equal? left elem-type)
                            (t-list elem-type)
                            (raise-type-error "The first arg of the link operator expected to be the type of the elemens of the list in the second arg")
                           )
     ]
    [else (raise-type-error "The second arg of the link operator expected to be a list")]))

;Check if the arg is a list and return and return the lists element type or throw error
(define (type-check-first [expr : Type]): Type
  (type-case Type expr
    [(t-list elem-type) elem-type]
    [else (raise-type-error "The arg of the first operator expected to be a list")]
    ))

;Check if the arg is a list and return list type or throw error
(define (type-check-rest [expr : Type]): Type
  (type-case Type expr
    [(t-list elem-type) (t-list elem-type)]
    [else (raise-type-error "The arg of the rest operator expected to be a list")]
    ))

;Check if arg is a list and return bool type or throw error
(define (type-check-is-empty [expr : Type]): Type
  (type-case Type expr
    [(t-list elem-type) (t-bool)]
    [else (raise-type-error "The arg of the is-empty operator expected to be a list")]
    ))

;Check if cond field is bool then check of cons and altern has the same type and return the type of cons or throw an error
(define (type-check-if [cond : Type] [cons : Type] [altern : Type]): Type
  (type-case Type cond
    [(t-bool) (if (equal? cons altern)
                  cons
                  (raise-type-error "The consequense and alterntieve elements of the if statements need to be the same")
               )]
    [else (raise-type-error "The conditional field of the if-statement expected to be a boolean")]
    ))

;Check if the env(hashmap) has a type for a s(symbol) otherwise give back compile error
(define (lookupTypeCheck [env : TEnv] [s : Symbol]): Type
  (type-case (Optionof Type) (hash-ref env s)
    [(none) (raise-type-error "A variable is not bounded" )] 
    [(some type) type]))

;Make a new env with a nieuw key (new-name) value pair that extends the old env
(define (extendTypeCheck [old-env : TEnv] [new-name : Symbol] [type : Type]): TEnv
  (hash-set old-env new-name type))

;Make a new fun type that extends the the current env with the param that bounds it to an arg-type
(define (lambda [param : Symbol] [arg-type : Type] [body : Expr] [env : TEnv]): Type
  (let ([new-env (extendTypeCheck env param arg-type)])
    (t-fun arg-type (type-ofT body new-env))
   )
  )

;Check if the first arg is a funct value and then check if the arg to applye to the func has the same type as the func requires then return the return-type or give back type error
(define (applyTypeCheck [func : Type] [arg : Type]): Type
  (type-case Type func
       [(t-fun arg-type return-type);Check if the first arg of applying a func is from func type
        (if (equal? arg-type arg); Check if the type of arg is the same as the func expects the type to be
            return-type
            (raise-type-error "When applying a function the type of the argument of the function has to be the same as the type of the given argument")
         )]
       [else (raise-type-error "When applying a function the first argument is expected to be a function value")]
     )
  )

;; ################## interp  ######################

; Add 2 numbers together
(define (add [v1 : Value] [v2 : Value]): Value
  (v-num (+ (v-num-value v1) (v-num-value v2))))

;Append 2 strings together
(define (append [v1 : Value] [v2 : Value]): Value
 (v-str (string-append (v-str-value v1) (v-str-value v2))))

;Check if 2 strings are equal
(define (streql [v1 : Value] [v2 : Value]): Value
  (v-bool (string=? (v-str-value v1) (v-str-value v2))))
   
;Check if 2 numbers are equal
(define (numeql [v1 : Value] [v2 : Value]): Value
  (v-bool (= (v-num-value v1) (v-num-value v2))))

;Return a new list with the added v1 value
(define (link [v1 : Value] [v2 : Value]): Value
  (v-list (cons v1 (v-list-vals v2))))

;Check if the arg is a non empty and return the first element of the list or throw error
(define (opFirst [expr : Value]): Value
 (if (empty? (v-list-vals expr))
     (raise-interp-error "The arg of the first operator expected to be a list with at least one item")
     (first (v-list-vals expr))))

;Check if the arg is a non empty list and return the rest of the elements of the list or throw error
(define (opRest [expr : Value]): Value
  (if (empty? (v-list-vals expr))
     (raise-interp-error "The arg of the first operator expected to be a list with at least one item")
     (v-list (rest (v-list-vals expr)))))

;Check if the arg is a list and return a if the list is empty or throw error
(define (is-empty [expr : Value]): Value
  (v-bool (empty? (v-list-vals expr))))

;Give boolean-value back
(define (boolean-decision [v : Value]): Boolean
  (v-bool-value v))

;Check if the env(hashmap) has a value for a s(symbol) otherwise give back compile error
(define (lookupInterp [env : Env] [s : Symbol]): Value
  (type-case (Optionof Value) (hash-ref env s)
    [(none) (raise-interp-error "A variable is not bounded" )] 
    [(some value) value]))

;Make a new env with a nieuw key (new-name) value pair that extends the old env
(define (extendInterp [old-env : Env] [new-name : Symbol] [value : Value]): Env
  (hash-set old-env new-name value))

;Apply the func by extending the env
(define (applyInterp [func : Value] [arg : Value]): Value
  (let ([new-env (extendInterp (v-fun-env func) (v-fun-param func) arg)]) ; Make a new env that extends the old env with a new key(param)-value(compiled arg) pair
           (interpE (v-fun-body func) new-env))
  )



;; ################################################
;; ################## Local Tests #################
;; ################################################
(print-only-errors #true)
;; ################## tychk  ######################
(test (type-of (e-num 5)) (t-num))
(test (type-of (e-bool #t)) (t-bool))
(test (type-of (e-str "hello")) (t-str))
(test (type-of (e-empty (t-num))) (t-list (t-num)))
(test (type-of (e-empty (t-list (t-num)))) (t-list (t-list (t-num))))

(test (type-of (e-op (op-plus) (e-num 1) (e-num 1))) (t-num))
(test (type-of (e-op (op-append) (e-str "Hello ") (e-str "world"))) (t-str))
(test (type-of (e-op (op-num-eq) (e-num 1) (e-num 1))) (t-bool))
(test (type-of (e-op (op-str-eq) (e-str "Hello ") (e-str "world"))) (t-bool))
(test (type-of (e-op (op-link) (e-num 1) (e-empty (t-num)))) (t-list (t-num)))

(test (type-of (e-un-op (op-first) (e-empty (t-num)))) (t-num))
(test (type-of (e-un-op (op-rest) (e-empty (t-num)))) (t-list (t-num)))
(test (type-of (e-un-op (op-is-empty) (e-empty (t-num)))) (t-bool))

(test (type-of (e-if (e-bool #t) (e-str "Hello") (e-str "world"))) (t-str))

(test (type-of (e-lam 'x (t-str) (e-op (op-append) (e-var 'x) (e-str "test")))) (t-fun (t-str) (t-str)))
(test (type-of (e-lam 'x (t-str) (e-str "test"))) (t-fun (t-str) (t-str)))
(test (type-of (e-app (e-lam 'x (t-num) (e-op (op-plus) (e-var 'x) (e-num 1))) (e-op (op-plus) (e-num 2) (e-num 3)))) (t-num))
;(test (type-of (e-app (e-lam 'x (t-num) (e-op (op-plus) (e-var 'x) (e-num 1))) (e-str "h"))) (t-num))

;; ################## interp  ######################
(test (interp (e-num 5)) (v-num 5))
(test (interp (e-str "hello world")) (v-str "hello world"))
(test (interp (e-bool #t)) (v-bool #t))
(test (interp (e-empty (t-num))) (v-list empty))

(test (interp (e-op (op-plus) (e-num 1) (e-num 2))) (v-num 3))
(test (interp (e-op (op-append) (e-str "hello") (e-str "world"))) (v-str "helloworld"))
(test (interp (e-op (op-str-eq) (e-str "hello") (e-str "hello"))) (v-bool #t))
(test (interp (e-op (op-str-eq) (e-str "hello") (e-str "world"))) (v-bool #f))
(test (interp (e-op (op-num-eq) (e-num 2) (e-num 2))) (v-bool #t))
(test (interp (e-op (op-num-eq) (e-num 2) (e-num 1))) (v-bool #f))
(test (interp (e-op (op-link) (e-num 2) (e-empty (t-num)))) (v-list (list (v-num 2))))

(test (interp (e-un-op (op-first) (e-op (op-link) (e-num 2) (e-empty (t-num))))) (v-num 2))
;(test/exn (interp (e-un-op (op-first) (e-empty (t-num))))"")
(test (interp (e-un-op (op-rest) (e-op (op-link) (e-num 2) (e-empty (t-num))))) (v-list empty))
(test (interp (e-un-op (op-is-empty) (e-op (op-link) (e-num 2) (e-empty (t-num))))) (v-bool #f))
(test (interp (e-un-op (op-is-empty) (e-empty (t-num)))) (v-bool #t))

(test (interp (e-if (e-bool #t) (e-str "cons") (e-str "alt"))) (v-str "cons"))
(test (interp (e-if (e-bool #f) (e-str "cons") (e-str "alt"))) (v-str "alt"))

;(test/exn (interp (e-var 'x)) "compile-error-variable-not-bound: A variable is not bounded")
;(test/exn (interp (e-app (e-num 5) (e-num 1))) "compile-error-expect-function-value: When applying a functio the first argument is expected to be a function value")
;(test/exn (interp (e-app (e-lam 'x (t-num) (e-var 'y)) (e-num 1))) "compile-error-variable-not-bound: A variable is not bounded")
(test (interp (e-app (e-lam 'x (t-num) (e-op (op-plus) (e-var 'x) (e-num 4))) (e-num 4))) (v-num 8))
(test (interp (e-app
         (e-lam 'x (t-num) (e-app
                    (e-lam 'y (t-num) (e-op (op-plus) (e-var 'x) (e-var 'y)))
                    (e-num 2)
                    ))
         (e-num 1)
         )
        ) (v-num 3))
(test (interp (e-app
         (e-lam 'x (t-num) (e-app
                    (e-lam 'x (t-num) (e-op (op-plus) (e-var 'x) (e-var 'x)));variable shadowing
                    (e-num 2)
                    ))
         (e-num 1)
         )
        ) (v-num 4))

;; ################## desugar ######################
(test (desugar (e-num+ 5)) (e-num 5))
(test (desugar (e-str+ "hello world")) (e-str "hello world"))
(test (desugar (e-bool+ #t)) (e-bool #t))
(test (desugar (e-empty+ (t-num))) (e-empty (t-num)))

(test (desugar (e-op+ (op-plus) (e-num+ 1) (e-num+ 2))) (e-op (op-plus) (e-num 1) (e-num 2)))
(test (desugar (e-op+ (op-append) (e-str+ "hello") (e-str+ "world"))) (e-op (op-append) (e-str "hello") (e-str "world")))
(test (desugar (e-op+ (op-str-eq) (e-str+ "hello") (e-str+ "hello"))) (e-op (op-str-eq) (e-str "hello") (e-str "hello")))
(test (desugar (e-op+ (op-str-eq) (e-str+ "hello") (e-str+ "world"))) (e-op (op-str-eq) (e-str "hello") (e-str "world")))
(test (desugar (e-op+ (op-num-eq) (e-num+ 2) (e-num+ 2))) (e-op (op-num-eq) (e-num 2) (e-num 2)))
(test (desugar (e-op+ (op-num-eq) (e-num+ 2) (e-num+ 1))) (e-op (op-num-eq) (e-num 2) (e-num 1)))
(test (desugar (e-op+ (op-link) (e-num+ 2) (e-empty+ (t-num)))) (e-op (op-link) (e-num 2) (e-empty (t-num))))

(test (desugar (e-un-op+ (op-first) (e-op+ (op-link) (e-num+ 2) (e-empty+ (t-num))))) (e-un-op (op-first) (e-op (op-link) (e-num 2) (e-empty (t-num)))))
(test (desugar (e-un-op+ (op-rest) (e-op+ (op-link) (e-num+ 2) (e-empty+ (t-num))))) (e-un-op (op-rest) (e-op (op-link) (e-num 2) (e-empty (t-num)))))
(test (desugar (e-un-op+ (op-is-empty) (e-empty+ (t-num)))) (e-un-op (op-is-empty) (e-empty (t-num))))

(test (desugar (e-if+ (e-bool+ #t) (e-str+ "cons") (e-str+ "alt"))) (e-if (e-bool #t) (e-str "cons") (e-str "alt")))
(test (desugar (e-if+ (e-bool+ #f) (e-str+ "cons") (e-str+ "alt"))) (e-if (e-bool #f) (e-str "cons") (e-str "alt")))

(test (desugar (sugar-and (e-bool+ #t) (e-bool+ #t))) (e-bool #t))
(test (desugar (sugar-and (e-bool+ #t) (e-bool+ #f))) (e-bool #f))
(test (desugar (sugar-and (e-bool+ #f) (e-str+ "This is not evaluated"))) (e-bool #f))
;(test/exn (desugar (sugar-and (e-str+ "This results in an error") (e-bool+ #f))) "desugar-error-and: The first arg of the and operator expects to be a boolean")
;(test/exn (desugar (sugar-and (e-bool+ #t) (e-str+ "This results in an error"))) "desugar-error-and: The second arg of the and operator expects to be a boolean")

(test (desugar (sugar-or (e-bool+ #t) (e-bool+ #t))) (e-bool #t))
(test (desugar (sugar-or (e-bool+ #t) (e-str+ "This is not evaluated"))) (e-bool #t))
(test (desugar (sugar-or (e-bool+ #f) (e-bool+ #t))) (e-bool #t))
(test (desugar (sugar-or (e-bool+ #f) (e-bool+ #f))) (e-bool #f))
;(test/exn (desugar (sugar-or (e-str+ "This results in an error") (e-bool+ #f))) "desugar-error-or: The first arg of the or operator expects to be a boolean")
;(test/exn (desugar (sugar-or (e-bool+ #f) (e-str+ "This results in an error"))) "desugar-error-or: The second arg of the or operator expects to be a boolean")

(test (desugar (e-var+ 'x)) (e-var 'x))
(test (desugar (e-app+ (e-lam+ 'x (t-num) (e-var+ 'x)) (e-num+ 1))) (e-app (e-lam 'x (t-num) (e-var 'x)) (e-num 1)))
(test (desugar (sugar-let 'x (e-str+ "Hello World") (t-num) (e-var+ 'x))) (e-app (e-lam 'x (t-num) (e-var 'x)) (e-str "Hello World")))
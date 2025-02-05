#lang plait

;; =============================================================================
;; Typed Interpreter: support.rkt
;; =============================================================================

;;  =======================================================

(define-type-alias Env (Hashof Symbol Value))
(define-type-alias TEnv (Hashof Symbol Type))

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

(define-type Expr
  (e-num [value : Number])
  (e-bool [value : Boolean])
  (e-str [value : String])
  (e-op [op : Operator]
        [left : Expr]
        [right : Expr])
  (e-un-op [op : UnaryOperator]
           [expr : Expr])
  (e-if [cond : Expr]
        [consq : Expr]
        [altern : Expr])
  (e-lam [param : Symbol]
         [arg-type : Type]
         [body : Expr])
  (e-app [func : Expr]
         [arg : Expr])
  (e-var [name : Symbol])
  (e-empty [elem-type : Type]))

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

; Turn arbitrary sequence into string
(define-syntax-rule (~a arg ...)
  (foldl (lambda (val string)
           (string-append string val))
         ""
         (list (to-string arg) ...)))

; Turn an S-exp that represents a type annotation into a concrete Type
(define (parse-type [t : S-Exp]): Type
  (cond
    [(s-exp-match? `Num t) (t-num)]
    [(s-exp-match? `Bool t) (t-bool)]
    [(s-exp-match? `Str t) (t-str)]
    [(s-exp-match? `{ANY -> ANY} t)
     (t-fun (parse-type (first (s-exp->list t)))
            (parse-type (third (s-exp->list t))))]
    [(s-exp-match? `{List ANY} t)
     (t-list (parse-type (second (s-exp->list t))))]
    [else (error 'type "invalid type annotation")]))

(define (parse [input : S-Exp]): Expr+
  (cond

    ; Matching literals
    [(s-exp-number? input) (e-num+ (s-exp->number input))]
    [(s-exp-string? input) (e-str+ (s-exp->string input))]
    [(s-exp-match? `true input) (e-bool+ #t)]
    [(s-exp-match? `false input) (e-bool+ #f)]

    ; Matching sugar
    [(s-exp-match? `{and ANY ...} input)
     (let ([inlst (s-exp->list input)])
       (if (equal? (length inlst) 3)
           (sugar-and (parse (second inlst)) (parse (third inlst)))
           (error '+ "incorrect number of args to and")))]
    [(s-exp-match? `{or ANY ...} input)
     (let ([inlst (s-exp->list input)])
       (if (equal? (length inlst) 3)
           (sugar-or (parse (second inlst)) (parse (third inlst)))
           (error '+ "incorrect number of args to or")))]
    [(s-exp-match? `{let {SYMBOL ANY : ANY} ANY ...} input)
     (let ([inlst (s-exp->list input)])
       (if (equal? (length inlst) 3)
           (sugar-let
            (s-exp->symbol (first (s-exp->list (second inlst))))
            (parse (second (s-exp->list (second inlst))))
            (parse-type (fourth (s-exp->list (second inlst))))
            (parse (third inlst)))
           (error 'let "incorrect number of args to let")))]

    ; Matching binary operators
    [(s-exp-match? `{+ ANY ...} input)
     (let ([inlst (s-exp->list input)])
       (if (equal? (length inlst) 3)
           (e-op+ (op-plus) (parse (second inlst)) (parse (third inlst)))
           (error '+ "incorrect number of args to +")))]
    [(s-exp-match? `{++ ANY ...} input)
     (let ([inlst (s-exp->list input)])
       (if (equal? (length inlst) 3)
           (e-op+ (op-append) (parse (second inlst)) (parse (third inlst)))
           (error '++ "incorrect number of args to ++")))]
    [(s-exp-match? `{num= ANY ...} input)
     (let ([inlst (s-exp->list input)])
       (if (equal? (length inlst) 3)
           (e-op+ (op-num-eq) (parse (second inlst)) (parse (third inlst)))
           (error 'num= "incorrect number of args to num=")))]
    [(s-exp-match? `{str= ANY ...} input)
     (let ([inlst (s-exp->list input)])
       (if (equal? (length inlst) 3)
           (e-op+ (op-str-eq) (parse (second inlst)) (parse (third inlst)))
           (error 'str= "incorrect number of args to str=")))]
    [(s-exp-match? `{link ANY ...} input)
     (let ([inlst (s-exp->list input)])
       (if (equal? (length inlst) 3)
           (e-op+ (op-link) (parse (second inlst)) (parse (third inlst)))
           (error 'link "incorrect number of args to link")))]

    ; Matching unary operators
    [(s-exp-match? `{empty : ANY ...} input)
     (let ([inlst (s-exp->list input)])
       (if (equal? (length inlst) 3)
           (e-empty+ (parse-type (third inlst)))
           (error 'empty "incorrect number of args to empty")))]
    [(s-exp-match? `{is-empty ANY ...} input)
     (let ([inlst (s-exp->list input)])
       (if (equal? (length inlst) 2)
           (e-un-op+ (op-is-empty) (parse (second inlst)))
           (error 'is-empty "incorrect number of args to is-empty")))]
    [(s-exp-match? `{first ANY ...} input)
     (let ([inlst (s-exp->list input)])
       (if (equal? (length inlst) 2)
           (e-un-op+ (op-first) (parse (second inlst)))
           (error 'first "incorrect number of args to first")))]
    [(s-exp-match? `{rest ANY ...} input)
     (let ([inlst (s-exp->list input)])
       (if (equal? (length inlst) 2)
           (e-un-op+ (op-rest) (parse (second inlst)))
           (error 'rest "incorrect number of args to rest")))]

    ; Matching if expressions
    [(s-exp-match? `{if ANY ...} input)
     (let ([inlst (s-exp->list input)])
       (if (equal? (length inlst) 4)
           (e-if+ (parse (second inlst)) (parse (third inlst)) (parse (fourth inlst)))
           (error '+ "incorrect number of args to if")))]

    ; Matching lambda expressions
    [(s-exp-match? `{lam {SYMBOL : ANY} ANY ...} input)
     (let ([inlst (s-exp->list input)])
       (if (equal? (length inlst) 3)
           (e-lam+
            (s-exp->symbol (first (s-exp->list (second inlst))))
            (parse-type (third (s-exp->list (second inlst))))
            (parse (third inlst)))
           (error 'lam "lambdas should only have one body")))]
    [(s-exp-match? `{lam {SYMBOL : ANY ...} ANY ...} input)
     (error 'lam "lambdas must have exactly one parameter")]

    ; Matching application expressions
    [(s-exp-match? `{ANY ...} input)
     (let ([inlst (s-exp->list input)])
       (if (equal? (length inlst) 2)
           (e-app+ (parse (first inlst)) (parse (second inlst)))
           (error 'app "incorrect number of args to app")))]

    ; Matching variable names
    [(s-exp-symbol? input) (e-var+ (s-exp->symbol input))]))

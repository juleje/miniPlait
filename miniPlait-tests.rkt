#lang racket

;; =============================================================================
;; Typed Interpreter: tynterp-tests.rkt
;; =============================================================================

(require (only-in "miniPlait.rkt" eval)
         "support.rkt"
         "test-support.rkt")

;; =================================================

(define/provide-test-suite student-tests ;; DO NOT EDIT THIS LINE ==========
  ; TODO: Add your own tests below!
  
  ;##################Testing primitive values#################
  (test-equal? "Works with Num primitive"
               (eval `2) (v-num 2))
  (test-equal? "Works with Str primitive"
               (eval `"hello world") (v-str "hello world"))
  (test-equal? "Works with Bool primitive"
               (eval `true) (v-bool #t))
  (test-equal? "Works with list primitive"
               (eval `{empty : Num}) (v-list empty))

   ;##################Testing operators      ##################
  ; Testing +
  (test-equal? "Works with +"
               (eval `{+ 1 2}) (v-num 3))
  (test-equal? "Works with + and negative values"
               (eval `{+ 1 -2}) (v-num -1))
  (test-not-equal? "+ Fails because of floating point imprecision"
               (eval `{+ 0.1 0.2}) (v-num 0.3))
  (test-equal? "Works with + first arg nested"
               (eval `{+ {+ 1 1} 1}) (v-num 3))
  (test-equal? "Works with + second arg nested"
               (eval `{+ 1 {+ 1 1}}) (v-num 3))
  (test-equal? "Works with + args nested"
               (eval `{+ {+ 1 1} {+ 1 1}}) (v-num 4))
  (test-raises-type-error? "Passing Str to first + arg results in error"
                             (eval `{+ "bad" 1}))
  (test-raises-type-error? "Passing Str to second + arg results in error"
                             (eval `{+ 1 "bad"}))
  (test-raises-type-error? "Passing Bool to first + arg results in error"
                             (eval `{+ true 1}))
  (test-raises-type-error? "Passing Bool to second + arg results in error"
                             (eval `{+ 1 false}))
   ; Testing ++
  (test-equal? "Works with ++"
               (eval `{++ "Hello" "World"}) (v-str "HelloWorld"))
  (test-not-equal? "++ order of args is important"
               (eval `{++ "world" "hello"}) (v-str "helloworld"))
  (test-equal? "Works with ++ first arg nested"
               (eval `{++ {++ "Hello" " "} "World"}) (v-str "Hello World"))
  (test-equal? "Works with ++ second arg nested"
               (eval `{++ "Hello" {++ " " "World"}}) (v-str "Hello World"))
  (test-equal? "Works with ++ args nested"
               (eval `{++ {++ "Hello" " "} {++ "world" "!"}}) (v-str "Hello world!"))
  (test-raises-type-error? "Passing num to first ++ arg results in error"
                             (eval `{++ 1 "hello"}))
  (test-raises-type-error? "Passing num to second ++ arg results in error"
                             (eval `{+ "hello" 1}))
  (test-raises-type-error? "Passing Bool to first ++ arg results in error"
                             (eval `{++ true "hello"}))
  (test-raises-type-error? "Passing Bool to second ++ arg results in error"
                             (eval `{++ "hello" true}))
  ;Testing str=
  (test-equal? "Works with str= t"
               (eval `{str= "hello" "hello"}) (v-bool #t))
  (test-equal? "Works with str= f"
               (eval `{str= "hello" "world"}) (v-bool #f))
  (test-equal? "Works with str= and first arg nested"
               (eval `{str= {++ "hello" "world"} "helloworld"}) (v-bool #t))
  (test-equal? "Works with str= and second arg nested"
               (eval `{str= "helloworld" {++ "hello" "world"}}) (v-bool #t))
  (test-equal? "Works with str= args nested"
               (eval `{str= {++ "hello" "world"} {++ "hello" "world"}}) (v-bool #t))
  (test-raises-type-error? "Passing Num to first str= arg results in error"
               (eval `{str= 1 "hello"}))
  (test-raises-type-error? "Passing Num to second str= arg results in error"
               (eval `{str= "hello" 1}))
  (test-raises-type-error? "Passing Bool to first str= arg results in error"
               (eval `{str= true "hello"}))
  (test-raises-type-error? "Passing Bool to second str= arg results in error"
               (eval `{str= "hello" true}))
   ;Testing num=
  (test-equal? "Works with num= t"
               (eval `{num= 1 1}) (v-bool #t))
  (test-equal? "Works with num= f"
               (eval `{num= 1 2}) (v-bool #f))
  (test-equal? "Works with num= and first arg nested"
             (eval `{num= {+ 1 2} 3}) (v-bool #t))
  (test-equal? "Works with num= and second arg nested"
             (eval `{num= 3 {+ 1 2}}) (v-bool #t))
  (test-equal? "Works with num= args nested"
             (eval `{num= {+ 1 3} {+ 2 2}}) (v-bool #t))
  (test-raises-type-error? "Passing Str to first num= arg results in error"
             (eval `{num= "hello" 1}))
  (test-raises-type-error? "Passing Str to second num= arg results in error"
             (eval `{num= 1 "hello"}))
  (test-raises-type-error? "Passing Bool to first num= arg results in error"
             (eval `{num= true 1}))
  (test-raises-type-error? "Passing Bool to second num= arg results in error"
             (eval `{num= 1 true}))
  ; Testing link
  (test-equal? "Works with link"
               (eval `{link 1 {empty : Num}}) (v-list (list (v-num 1))))
  (test-equal? "Works with link first arg nested"
               (eval `{link {+ 1 1} {empty : Num}}) (v-list (list (v-num 2))))
  (test-equal? "Works with link second arg nested"
               (eval `{link 1 {link 2 {empty : Num}}}) (v-list (list (v-num 1) (v-num 2))))
  (test-equal? "Works with link args nested"
               (eval `{link {+ 1 1} {link 2 {empty : Num}}}) (v-list (list (v-num 2) (v-num 2))))
  (test-raises-type-error? "Link with wrong type element to add results in error"
               (eval `{link "Hello" {empty : Num}}))
  (test-raises-type-error? "Passing Str to second link arg results in error"
                             (eval `{link 1 "bad"}))
  (test-raises-type-error? "Passing Bool to second link arg results in error"
                             (eval `{link 1 false}))
  (test-raises-type-error? "Passing Num to second link arg results in error"
                             (eval `{link 1 2}))
  ; Testing first
  (test-equal? "Works with first"
               (eval `{first {link 1 {empty : Num}}}) (v-num 1))
  (test-equal? "Works with first nested"
               (eval `{first {link 1 {link 2 {empty : Num}}}}) (v-num 1))
  (test-raises-type-error? "first with arg num results in error"
               (eval `{first 1}))
  (test-raises-type-error? "first with arg str results in error"
               (eval `{first "hello"}))
  (test-raises-type-error? "first with arg bool results in error"
               (eval `{first true}))
  (test-raises-interp-error? "first with empty list results in error"
               (eval `{first {empty : Num}}))
  ; Testing rest
  (test-equal? "Works with rest"
               (eval `{rest {link 1 {empty : Num}}}) (v-list empty))
  (test-equal? "Works with rest nested"
               (eval `{rest {link 1 {link 2 {empty : Num}}}}) (v-list (list (v-num 2))))
  (test-raises-type-error? "rest with arg num results in error"
               (eval `{rest 1}))
  (test-raises-type-error? "rest with arg str results in error"
               (eval `{rest "hello"}))
  (test-raises-type-error? "rest with arg bool results in error"
               (eval `{rest true}))
  (test-raises-interp-error? "rest with empty list results in error"
               (eval `{rest {empty : Num}}))
  ; Testing is-empty
  (test-equal? "Works with is-empty - t"
               (eval `{is-empty {empty : Num}}) (v-bool #t))
  (test-equal? "Works with is-empty - f"
               (eval `{is-empty {link 1 {empty : Num}}}) (v-bool #f))
  (test-raises-type-error? "is-empty with arg num results in error"
               (eval `{is-empty 1}))
  (test-raises-type-error? "is-empty with arg str results in error"
               (eval `{is-empty "hello"}))
  (test-raises-type-error? "is-empty with arg bool results in error"
               (eval `{is-empty true}))
  ;;################## and operation       ################
  (test-equal? "Works with and t t"
               (eval `{and true true}) (v-bool #t))
  (test-equal? "Works with and t f "
               (eval `{and true false}) (v-bool #f))
  (test-equal? "Works with and f f "
               (eval `{and false false}) (v-bool #f))
  (test-equal? "Works with and t first arg nested"
               (eval `{and {and true true} true}) (v-bool #t))
  (test-equal? "Works with and t second arg nested"
               (eval `{and true {and true true}}) (v-bool #t))
  (test-equal? "Works with and, args nested"
               (eval `{and {and true true} {and true true}}) (v-bool #t))
  (test-equal? "Works with and, second arg will not be evaluated because of short-circuit"
               (eval `{and false "This is not evaluated"}) (v-bool #f))
  (test-raises-type-error? "And with first args string results in error"
             (eval `{and "This result in an error" true}))
  (test-raises-type-error? "And with first args number results in error"
             (eval `{and 0 true}))
  (test-raises-type-error? "And with second args string results in error"
             (eval `{and true "This result in an error"}))
  (test-raises-type-error? "And with second args number results in error"
             (eval `{and true 0}))
  ;;################## or operation       ################
  (test-equal? "Works with or t t"
               (eval `{or true true}) (v-bool #t))
  (test-equal? "Works with or t f"
               (eval `{or true false}) (v-bool #t))
  (test-equal? "Works with or f f"
               (eval `{or false false}) (v-bool #f))
  (test-equal? "Works with or t first arg nested"
               (eval `{or {and true true} true}) (v-bool #t))
  (test-equal? "Works with or t second arg nested"
               (eval `{or false {and true true}}) (v-bool #t))
  (test-equal? "Works with or, args nested"
               (eval `{or {or false false} {or true true}}) (v-bool #t))
  (test-equal? "Works with or, second arg will not be evaluated because of short-circuit"
               (eval `{or true "This is not evaluated"}) (v-bool #t))
  (test-raises-type-error? "Or with first args string results in error"
             (eval `{or "This result in an error" true}))
  (test-raises-type-error? "Or with first args number results in error"
             (eval `{or 0 true}))
  (test-raises-type-error? "Or with second args string results in error"
             (eval `{or false "This result in an error"}))
  (test-raises-type-error? "Or with second args number results in error"
             (eval `{or false 0}))
  
   ;################## If statements      ##################
  (test-equal? "Works with if - cons"
               (eval `{if true "cons" "alt"}) (v-str "cons"))
  (test-equal? "Works with if - alt"
               (eval `{if false "cons" "alt"}) (v-str "alt"))
  (test-equal? "Works with if nested cond - cons"
               (eval `{if {num= 1 1} "cons" "alt"}) (v-str "cons"))  
  (test-equal? "Works with if nested cond - alt"
               (eval `{if {str= "hello" "world"} "cons" "alt"}) (v-str "alt"))
  (test-equal? "Works with if nested cons"
               (eval `{if true {++ "hello" "world"} "alt"}) (v-str "helloworld"))
  (test-equal? "Works with if nested alt"
               (eval `{if false "cons" {++ "hello" "world"}}) (v-str "helloworld"))
  (test-equal? "Works with if nested args - t"
               (eval `{if {num= 1 1} {++ "hello" "world"} {++ "hallo" "wereld"}}) (v-str "helloworld"))  
  (test-equal? "Works with if nested args - f"
               (eval `{if {str= "hello" "world"} {++ "hello" "world"} {++ "hallo" "wereld"}}) (v-str "hallowereld"))
  (test-raises-type-error? "if passes str to first arg result in error"
                      (eval `{if "true" "cons" "alt"}))
  (test-raises-type-error? "if passes str to first arg result in error"
                      (eval `{if 1 "cons" "alt"}))
  (test-raises-type-error? "if consequence and alternative have not the same type result in error"
                      (eval `{if true "cons" 1}))

 ;;################## Bindings & functions ################
  (test-true "Works with lambda"
             (v-fun? (eval `{lam {x : Num} 5})))
  (test-pred "Equivalent to the test case above, but with test-pred"
             v-fun? (eval `{lam {x : Num} 5}))
  (test-equal? "Works with app"
                (eval `{{lam {x : Str} "This"} "Not This"})(v-str "This"))
  (test-equal? "Works with app lambda and variables"
                (eval `{{lam {x : Str} x} "This"})(v-str "This"))
  (test-equal? "Works with app lambda and variables nested body"
                (eval `{{lam {x : Num} {+ 1 x}} 1})(v-num 2))
  (test-equal? "Works with app lambda and variables nested param"
                (eval `{{lam {x : Num} x} {+ 1 1}})(v-num 2))
  (test-equal? "Nested functions"
               (eval `{{lam {y : Num} {{lam {x : Num} {+ y x}} 1}} 2}) (v-num 3))
  (test-equal? "Nested functions variable shadowing the first x with the second x"
               (eval `{{lam {x : Num} {{lam {x : Num} {+ x x}} 2}} 1}) (v-num 4))
  (test-raises-type-error? "Applying func with non func value result in error"
                      (eval `{"not a function" 1}))
  (test-raises-type-error? "Calling a variable that is not bound in a function result in error"
                      (eval `{{lam {x : Num} y} 1}))
  (test-raises-type-error? "Variable not bound lambda"
                      (eval `{lam {x : Num} {+ y 5}}))
  (test-raises-type-error? "Type mismatch between func arg and give arg results in error"
                (eval `{{lam {x : Num} {+ x 5}} "This"}))


  ;;################## let binding       ################
  (test-equal? "Works with let"
               (eval `{let {x "Hello World" : Str} x}) (v-str "Hello World"))
  (test-equal? "Works with complex let"
               (eval `{let {x {lam {y : Num} {+ y y}} : {Num -> Num}} {x {x 1}}}) (v-num 4) )
  (test-equal? "Works with let body nested"
               (eval `{let {x "Hello" : Str} {++ x " World"}}) (v-str "Hello World"))
  (test-equal? "Works with let variable value nested"
               (eval `{let {x {++ "Hello" " "} : Str} {++ x "World"}}) (v-str "Hello World"))
  (test-raises-type-error? "Let with x with recursive definitions results in error"
             (eval `{let {x {+ 1 x} : Num} {+ 1 x}}))
  (test-raises-type-error? "Let mismatch datatype"
             (eval `{let {x 2 : Str} {+ 3 x}}))

  
  
  )


;; DO NOT EDIT BELOW THIS LINE =================================================

(module+ main (run-tests student-tests))
